# Notes: 
# MLP StandardBackprop has problems with hidden units bigger than 60. Not valid weights. Rprop does not present 
# the same problem.
# MLR has problem with parallelization. Processor should be equals to 4. #solved
# Swap was missing

library("e1071")
library("RSNNS")
library("cvTools")
library("data.table")
library("zoo") #for rollmeans
library("biglm")
library(mxnet)

#### Function log
Log <- function(text, ...) {
  msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
  #cat(msg)
  write.socket(log.socket, msg)
}

#### Calculate RSME 
rsme <- function(x,y){
  return (sqrt(mean( (x-y)^2  )))
}

#### Get sequential data
seqdata<-function(sn, tm,imunum, normaldata=F,shuffledata=F,datasetsize=c()){
  imud<-as.data.frame(fread(imu_data[imunum],sep=" ",header=F))
  imut<-as.data.frame(fread(imu_target[imunum],sep=" ",header=F))
  
  # removing first column containing time
  imud<-imud[,2:ncol(imud)]
  imut<-imut[,2:ncol(imut)]
  colnames(imud)<-c("V1","V2","V3","V4","V5","V6")
  colnames(imut)<-c("V1","V2","V3","V4","V5","V6")
  
  if (length(datasetsize)>0){
    imud=imud[datasetsize[1]:datasetsize[2],]
    imut=imut[datasetsize[1]:datasetsize[2],]
  }
  
  if (normaldata == T){
    input<-normalized(imud[,sn])
    target<-normalized(imut[,sn])
  }else{
    input<-imud[,sn] # * -1 workaround for fixing xbow measures
    target<-imut[,sn]
  }
  
  sequences=matrix(data=NA,ncol=tm+1,nrow=nrow(imud))
  counter=1
  # building sequences
  row=1
  while (counter <=nrow(imud)){
    col=c()
    if (shuffledata == T){
      coltmp=input[counter:(counter+tm-1)] #input
      colidx<-sample(length(coltmp),length(coltmp))
      col=cbind(col,t(coltmp[colidx]))
    }else{
      col=cbind(col,t(input[counter:(counter+tm-1)])) #input
    }
    
    col=cbind(col,target[counter+tm-1]) #target
    sequences[row,]<-col
    row=row+1
    counter=counter+slicing
    #  if (counter%%((slicing*100)+1) ==0 )
    #    print (counter)
  }
  # setup  dataset
  seqdf<-na.omit(as.data.frame(sequences))
  colnames(seqdf)[tm+1] <- "target"
  return (seqdf)
}

#### Normalize data
normalized <-function(x){
  return ((x-mean(x))/(max(x)-min(x)))
}
denormalize <-function(normalize,x){
  return (normalized)*(max(x)-min(x))+mean(x)
}

#### Train and test 
exptraintest <- function(imunum,taps_by_sensors=list(),model='lm', model_arguments=c(),normaldata=F,shuffledata=F,resamp=F,nresamp=2, windowsize=1000){
  
  results=c()
  sensors=c()
  names(taps_by_sensors)<-selected_sensors 
  for (sensornumber in selected_sensors){
    timedelay=taps_by_sensors[[as.character(sensornumber)]]
    print(paste("Testing sensor",sensornumber,"with lag",timedelay))
    print (" +Sequencing data..")
    trainset<- seqdata(sensornumber,timedelay,imunum,normaldata,shuffledata, datasetsize = c(1,imu_datainterval[[imunum]]$start))
    print (paste(" +Training Done",nrow(trainset),"sequences"))
    testset_total<- seqdata(sensornumber,timedelay,imunum,normaldata,shuffledata, datasetsize = c(imu_datainterval[[imunum]]$start,imu_datainterval[[imunum]]$end))
    print (paste(" +Testing Done",nrow(testset_total),"sequences"))
    set.seed(1225)
    lmmodel=build_prediction_model(model,model_arguments,trainset,timedelay)
    if (resamp==T){
      resamp_results=c()
      for (samp in seq(1,nresamp)){
        windowsstart=sample(nrow(testset_total)-windowsize,1)
        testset=testset_total[(windowsstart:(windowsstart+windowsize)),]
        prediction_lm <- get_model_predictions(model,model_arguments,testset,timedelay,lmmodel) 
        brsme=rsme(testset$target,testset[,timedelay])
        tt=c(rsme(testset$target,prediction_lm),brsme)
        resamp_results=rbind(resamp_results,tt)
        print(paste("  ++sample",samp, "starting at",windowsstart, "lenght",nrow(testset)))
      }
      sensors<-cbind(sensors,rep(timedelay,nresamp))
      sensors<-cbind(sensors,resamp_results)
    }else{
      testset<- testset_total
      prediction_lm <- build_model(model,model_arguments,trainset,testset,timedelay) 
      brsme=rsme(testset$target,testset[,timedelay])
      tt=c(rsme(testset$target,prediction_lm),brsme)
      sensors<-cbind(sensors,rep(timedelay,1))
      sensors<-cbind(sensors,t(tt))
    }
  }
  
  sensors=cbind(seq(1,nrow(sensors)),sensors)
  results=as.data.frame(sensors)
  names(results)[1]=c("sample")
  rownames(results)=NULL
  colnames(results)[2:(1+(length(selected_sensors)*3))]<-rbind(paste("timedelay",selected_sensors,sep="_"),
                                                               paste("X",selected_sensors,sep=""),
                                                               paste("t_rsme",selected_sensors,sep="_"))

  arguments_str<- paste(names(model_arguments)[1:5],as.vector(unlist(model_arguments))[1:5],sep="_",collapse="_")
  write.csv(results, file = paste(paste(results_dir,basename(imu_data[imunum])),model,arguments_str,"test_results.csv",sep='_') ,quote=F, row.names = T)
  return(results)
}
#### Create a model on a trainset and return prediction from a testset
#### Multiple models supported
build_prediction_model <- function( model='lm',model_arguments=c(),trainset,timedelay){
  if (model == 'mlp') {
    model_arg<-c(list(x=trainset[,1:timedelay],y=trainset$target),model_arguments)
    lmmodel <- do.call(get(model),model_arg)
  }else if(model == "mx.mlp"){
    library("mxnet")
    model_arg<-c(list(data=as.matrix(trainset[,1:timedelay]),label=as.array(trainset$target)),model_arguments)
    mx.set.seed(1212)
    lmmodel=do.call(get(model),model_arg)    
  }else if(model == "ma"){
     lmmodel=NULL 
  }
  else{
    model_arg<-c(list(data=trainset),model_arguments)
    formula <- as.formula(paste('target', paste(paste('', 
                                                      paste('V', seq(1:(ncol(trainset)-1)), sep = ''), 
                                                      sep = '', collapse = ' + ')), sep = ' ~ '))
    lmmodel <- do.call(get(model),c(formula,model_arg))
    
  }
  return(lmmodel)
}

get_model_predictions <- function( model='lm',model_arguments=c(),testset,timedelay,lmmodel=NULL){
  
  if (model == 'mlp') {
    if (timedelay==1){
      prediction_lm <- predict(lmmodel,newdata=cbind(testset[,1:timedelay])) #cbind fixs bugs when only one tap is selected 
      #(vector as a column)
    }else{
      prediction_lm <- predict(lmmodel,newdata=testset[,1:timedelay]) 
    }
  }else if(model == "mx.mlp"){
    mx.set.seed(1212)
    prediction_lm <- predict(lmmodel,as.matrix(testset[,1:timedelay]),array.layout='rowmajor')  
    
  }else if(model == "ma"){
    if(timedelay==1)        
      prediction_lm = testset[,1]
    else
      prediction_lm = rowMeans(testset[,(1:timedelay)])
  }
  else{
    prediction_lm <- predict(lmmodel,newdata=testset)
  }
  return(prediction_lm)
}
#### Create a model on a trainset and return prediction from a testset
#### Multiple models supported
build_model_and_predict <- function( model='lm',model_arguments=c(),trainset,testset,timedelay){
  lmmodel=build_prediction_model(model,model_arguments,trainset,timedelay)
  prediction_lm=get_model_predictions(model,model_arguments,testset,timedelay,lmmodel)
  return(prediction_lm)
}


#### Create a model on a trainset and return prediction from a testset
#### Multiple models supported
build_model_back <- function( model='lm',model_arguments=c(),trainset,testset,timedelay){
  
  if (model == 'mlp') {
    model_arg<-c(list(x=trainset[,1:timedelay],y=trainset$target),model_arguments)
    lmmodel <- do.call(get(model),model_arg)
    if (timedelay==1){
      prediction_lm <- predict(lmmodel,newdata=cbind(testset[,1:timedelay])) #cbind fixs bugs when only one tap is selected 
      #(vector as a column)
    }else{
      prediction_lm <- predict(lmmodel,newdata=testset[,1:timedelay]) 
    }
  }else if(model == "mx.mlp"){
    library("mxnet")
    model_arg<-c(list(data=as.matrix(trainset[,1:timedelay]),label=as.array(trainset$target)),model_arguments)
    mx.set.seed(1212)
    lmmodel=do.call(get(model),model_arg)    
    prediction_lm <- predict(lmmodel,as.matrix(testset[,1:timedelay]),array.layout='rowmajor')  
    
  }else if(model == "ma"){
      if(timedelay==1)        
        prediction_lm = testset[,1]
      else
        prediction_lm = rowMeans(testset[,(1:timedelay)])
    }
  else{
    model_arg<-c(list(data=trainset),model_arguments)
    formula <- as.formula(paste('target', paste(paste('', 
              paste('V', seq(1:(ncol(trainset)-1)), sep = ''), 
              sep = '', collapse = ' + ')), sep = ' ~ '))
    
    lmmodel <- do.call(get(model),c(formula,model_arg))
    prediction_lm <- predict(lmmodel,newdata=testset)
  }
  return(prediction_lm)
}

#### Cross Validation 
crossvalidation <- function(dataset,kfolds,timedelay,model='lm', model_arguments=c(),cv_type="random"){
  set.seed(1225)
  Log("Crossvalidation started")
  Log(paste(nrow(dataset),kfolds))
  folds <- cvFolds(nrow(dataset), K = kfolds, R = 1,type=cv_type)
  indexes=folds$which
  results<-foreach(k=1:kfolds, .combine='rbind', .multicombine=FALSE ) %dopar% {
    Log(paste("worker",k,": Started"))  
    time.start <- Sys.time()
    trainsetid=c()
    testsetid=c()
    for (t in seq(1,kfolds)[-k]) {
      trainsetid<-append(trainsetid,folds$subsets[which(indexes==t)])
    }
    testsetid<-folds$subsets[which(indexes==k)]
    #prediction_lm<-build_model(model,model_arguments,dataset[trainsetid,],dataset[testsetid,],timedelay)
    prediction_lm<-build_model_and_predict(model,model_arguments,dataset[trainsetid,],dataset[testsetid,],timedelay)
    
    time.stop <- Sys.time()
    time.taken <- time.stop - time.start 
    Log(paste("worker",k,": Elapsed Time:",time.taken))
    
    brsme=rsme(dataset[testsetid,]$target,dataset[testsetid,timedelay])
    return (c(rsme(dataset[testsetid,]$target,prediction_lm),brsme))
  }
  return(results)
}

##### Cross Validation Experiments using the first part of a dataset
expcv <- function(model='lm',arguments=c(),normaldata=F,shuffledata=F){
  Log("Experiments started")
  for (imunum in selected_imus){
    results=c()
    grsme=c()
    print(paste("IMU:",imu_data[imunum]))
    for (timedelay in selected_taps){
      print(paste("timetaps",timedelay))
      sensors=c()
      sensors<-cbind(rep(timedelay,kfolds))
      for (sensornumber in selected_sensors){
        print(paste("sensor",sensornumber))
        print ("Sequencing data..")
        sequencesdf<- seqdata(sensornumber,timedelay,imunum,normaldata,shuffledata,datasetsize = c(1,imu_datainterval[[imunum]]$start))
        print (paste("Done",nrow(sequencesdf),"sequences"))
        print("Crossvalidation")
        time.start <- Sys.time()
        cv<-crossvalidation(sequencesdf,kfolds,timedelay,model,arguments)
        arguments_str<- paste(names(arguments),as.vector(unlist(arguments)),sep="_",collapse="_")
        # write.csv(cv, file = paste(imu_data[imunum],model,arguments_str,sensornumber,timedelay,"cv_results.csv",sep='_') ,quote=F, row.names = T,sep=',')
        time.stop <- Sys.time()
        time.taken <- time.stop - time.start 
        print(paste("Done","Elapsed Time:",time.taken))
        sensors<-cbind(sensors,cv)
      }
      results=rbind(results,sensors)
    }
    colnames(results)<-rep(1,(length(selected_sensors)*2)+1)
    colnames(results)[1]<-"timedelay"
    colnames(results)[1:(length(selected_sensors)*2)+1]<-rbind(selected_sensors,paste(selected_sensors,"t_rsme",sep="_"))
    # some R magic for setting the name of the file based on model's arguments
    arguments_str<- paste(names(arguments)[1:5],as.vector(unlist(arguments))[1:5],sep="_",collapse="_")
    write.csv(results, file = paste(
                                    paste(results_dir,basename(imu_data[imunum]),"_",basename(imu_target[imunum]),sep="")
                                    ,model,arguments_str,"cv_results.csv",sep='_') ,quote=F, row.names = T)
  }
  
}

#### Function for generating Prediction vs Observed data using just 1 fold out a k-fold CV
pred_vs_obs <- function (sensornumber,timedelay,imunum,normaldata=F){
  set.seed(123) #all the imu dataset use the same data points
  print ("Sequencing data..")
  dataset<- seqdata(sensornumber,timedelay,imunum,normaldata)
  print ("Done")
  folds <- cvFolds(nrow(dataset), K = kfolds, R = 1)
  
  indexes=folds$which
  results=c()
  k=3
  trainsetid=c()
  testsetid=c()
  for (t in seq(1,kfolds)[-k]) {
    trainsetid<-append(trainsetid,folds$subsets[which(indexes==t)])
  }
  testsetid<-folds$subsets[which(indexes==k)]
  lmmodel <- lm(target ~ ., data = dataset[trainsetid,] )
  prediction_lm <- predict(lmmodel,dataset[testsetid,])
  r=cbind(dataset[testsetid[1:100],]$target,prediction_lm[1:100],dataset[testsetid[1:100],][,timedelay])
  return(r)
}

### SVM parameters Sweeping
svm_sweep<-function(normaldata=F){
  #nus = c(0.1,0.2,0.5)
  #gammas= c(0.01,0.05,0.1,0.2,0.5,1,2,4,8,16,32)
  nus = c(0.1)
  gammas= c(0.05)
  for (nu in nus){
    for (gamma in gammas){
      arguments<-list(nu=nu,gamma=gamma,type='nu-regression',scale=FALSE,cachesize=1000,shrinking=FALSE,tolerance = 0.005)
      expcv('svm',arguments,normaldata)
    }
  } 
}


ann_sweep<-function(normaldata=F){
  hidneurons=c(60)
  #,80,100)
  iter=200
  learnrate=0.3
  for (hidn in hidneurons){
    print(paste("Size of Hidden Neuron Layer",hidn))
    arguments<-list(
      #learnFunc="Std_Backpropagation",
      learnFunc="BackpropChunk",
      size=hidn,
      maxit=iter,
      learnFuncParams = c(learnrate,0.0,128),
      linOut=T         
    )
    expcv('mlp',arguments,normaldata)
  }
}

mxnet_mlp_sweep<-function(normaldata=F){
  hidneurons=c(80,100)
  #hidneurons=c(30,40,50,70,100)
  #hidneurons=c(60,80,90)
  iter=200
  for (hidn in hidneurons){
    print(paste("Size of Hidden Neuron Layer",hidn))
    arguments<-list(
      optimizer =  'rmsprop',
      hidden_node = hidn, 
      #momentum = 0.7,
      #learning.rate = 0.3,
      #wd=0.000001,
      #dropout=0.1,  
      num.round=iter,
      
      array.layout = "rowmajor",
      out_activation = "rmse",
      device=mx.gpu(0),array.batch.size=12384,
      eval.metric = mx.metric.rmse, 
      activation = "tanh",
      out_node = 1,
      verbose=F
      
    )
    expcv(normaldata=normaldata,shuffledata=F,model = 'mx.mlp',arguments)
  }
}



# Calculate RSME using moving average with different lag length
# usage: calculate_ma(7,c(imu_datainterval[[7]]$start,imu_datainterval[[7]]$end))
# FIXME: Verify proper offset
calculate_ma <- function (imunum,datasetinterval){
  results=cbind(selected_taps)
  for (sensornumber in selected_sensors){
    sensors=c()
    sequencesdf=seqdata(sn = sensornumber,1,imunum,datasetsize = datasetinterval)
    #sequencesdf=seqdata(sn = sensornumber,1,imunum,datasetsize = c(0,imu_datainterval[[imunum]]$start))
    
    for (timedelay in selected_taps){
      ma=rollmean(sequencesdf$V1,timedelay)
      sensors=rbind(sensors,c(rsme(ma,sequencesdf$target[timedelay:nrow(sequencesdf)])) )
     #print(sensors)
      }
    results=cbind(results,sensors)
   # print(results)
  }
  colnames(results)<-c("timedelay",selected_sensors)
  write.csv(results, file = paste(
    paste(results_dir,basename(imu_data[imunum]),"_",basename(imu_target[imunum]),sep="")
    ,"ma","cv_results.csv",sep='_') ,quote=F, row.names = T)

  return(results)
}


#### tune SVM  using 10 k-fold cross-validation
#  tuneSVM <- tune(svm, target~.,data=sequencesdf, ranges=list(nu = seq(0.1,0.5,0.1), gamma = c(0.02, 0.05, 0.1, 0.4, 0.8, 1, 2, 4, 8) ,kernel='radial',type='nu-regression'))
#  svmmodel <- tuneSVM$best.model
#  save(tuneSVM,file=paste("~/Dropbox/shared/MEMS-ANN/results/svm_model_",sensornumber,sep=""))
#  predictions_svm<-predict(svmmodel,sequencesdf)
#  #results[[sensornumber]] <- tuneResult

##### tune LM
#print ("Tuning, CV")
#tuneLM <- tune(lm, target~., data=sequencesdf)
#res<-CVlm(data=sequencesdf,lmmodel,plotit=F,m=10,seed=29)
#print(attr(res,'ms'))
#lmmodel <- lm(target ~ ., data = sequencesdf)
#print ("Model")
#cvlmodel <- cvLm(lmmodel,folds = folds)
#print ("CV")
#lmmodel <- tuneLM$best.model
#lmmodel <- lm(target~., data=sequencesdf)
#predictions_lm<-predict(lmmodel,sequencesdf)


#lmrsme=rsme(sequencesdf$target,predictions_lm)
#grsme=cbind(grsme,c(timedelay,lmrsme))

#### PLOT
# plot(predictions_lm,col='blue',type='l')
# lines(sequencesdf$target,col='red')
# lines(predictions_svm,col='green')
# title(main=paste("sensor",sensornumber,"RSME=",brsme,"E LM_RSME=",lmrsme))
# title(main=paste("sensor",sensornumber,"RSME=",brsme,"E SVM_RSME=",tuneSVM$best.performance, "E LM_RSME=",tuneLM$best.performance))
#title(main=paste("sensor",sensornumber,"RSME=", "E LM_RSME=",tuneLM$best.performance))


#####  Save PLOT 
# dev.copy(png,paste("~/Dropbox/shared/MEMS-ANN/results/svm_model_",sensornumber,".png",sep=""))
# dev.off()
