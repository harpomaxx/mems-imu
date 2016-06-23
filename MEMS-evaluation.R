# Notes: 
# MLP StandardBackprop has problems with hidden units bigger than 60. Not valid weights. Rprop does not present 
# the same problem.
# MLR has problem with parallelization. Processor should be equals to 4. #solved
# Swap was missing
#
library("e1071")
library("RSNNS")
library("cvTools")


##################
# Function log
#################
Log <- function(text, ...) {
  msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
  cat(msg)
  write.socket(log.socket, msg)
}

######################################
# Calculate RSME 
######################################
rsme <- function(x,y){
  return (sqrt(mean( (x-y)^2  )))
}

#######################################
## get sequential data
#######################################
seqdata<-function(sn, tm,imunum, normaldata=F,shuffledata=F){
  imud<-read.csv(imu_data[imunum],sep=" ",header=F)
  imut<-read.csv(imu_target[imunum],sep=" ",header=F)
  
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

##########################################
# Normalize data
##########################################
normalized <-function(x){
  return ((x-mean(x))/(max(x)-min(x)))
}

##########################################
# Cross Validation
##########################################
crossvalidation <- function(dataset,kfolds,timedelay,model=lm, model_arguments=c()){
  Log("Crossvalidation started")
  folds <- cvFolds(nrow(dataset), K = kfolds, R = 1,type="random")
  indexes=folds$which
  gc()
  results<-foreach(k=1:kfolds, .combine='rbind', .multicombine=FALSE ) %dopar% {
    Log(paste("worker",k,": Started"))  
    time.start <- Sys.time()
    trainsetid=c()
    testsetid=c()
    for (t in seq(1,kfolds)[-k]) {
      trainsetid<-append(trainsetid,folds$subsets[which(indexes==t)])
    }
    testsetid<-folds$subsets[which(indexes==k)]
    if (model == 'mlp') {
      model_arg<-c(list(x=dataset[trainsetid,1:timedelay],y=dataset[trainsetid,]$target),model_arguments)
      lmmodel <- do.call(get(model),model_arg)
      if (timedelay==1){
        prediction_lm <- predict(lmmodel,newdata=cbind(dataset[testsetid,1:timedelay])) #cbind fixs bugs when only one taps is selected 
        #(vector as a column)
      }else{
        prediction_lm <- predict(lmmodel,newdata=dataset[testsetid,1:timedelay]) 
      }
    }else{
      model_arg<-c(list(data=dataset[trainsetid,]),model_arguments)
      lmmodel <- do.call(get(model),c(as.formula("target ~ ."),model_arg))
      Log(paste("worker",k,": Model finished"))
      prediction_lm <- predict(lmmodel,newdata=dataset[testsetid,])
      Log(paste("worker",k,": Prediction finished"))
    }
    time.stop <- Sys.time()
    time.taken <- time.stop - time.start 
    Log(paste("worker",k,": Elapsed Time:",time.taken))
    brsme=rsme(dataset[testsetid,]$target,dataset[testsetid,timedelay])
    return (c(rsme(dataset[testsetid,]$target,prediction_lm),brsme))
  }
  return(results)
}

#################################################
# Cross Validation Experiments
#################################################
expcv <- function(model='lm',arguments=c(),normaldata=T,shuffledata=F){
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
        sequencesdf<- seqdata(sensornumber,timedelay,imunum,normaldata,shuffledata)
        print (paste("Done",nrow(sequencesdf),"sequences"))
        print("Crossvalidation")
        time.start <- Sys.time()
        cv<-crossvalidation(sequencesdf[1:(nrow(sequencesdf)/d_divisor),],kfolds,timedelay,model,arguments)
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
    arguments_str<- paste(names(arguments),as.vector(unlist(arguments)),sep="_",collapse="_")
    write.csv(results, file = paste(paste(results_dir,basename(imu_data[imunum]),sep=""),model,arguments_str,"cv_results.csv",sep='_') ,quote=F, row.names = T,sep=',')
  }
  
}
####################################################
# Function for generating Prediction vs Observed data using just 1 fold out a k-fold CV
####################################################
pred_vs_obs <- function (sensornumber,timedelay,imunum,normaldata=T){
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

############################################
# SVM parameters Sweeping
############################################
svm_sweep<-function(normaldata=T){
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

ann_sweep<-function(normaldata=T){
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


#################################################
# Main
#################################################
#expfulldataset()
#expcv(normaldata=F,shuffledata=F)
#plot_obs_vs_pred()
#ann_sweep(normaldata=F)  

#svm_sweep(normaldata=F)  
##################################################
# tune SVM  using 10 k-fold cross-validation
##################################################
#  tuneSVM <- tune(svm, target~.,data=sequencesdf, ranges=list(nu = seq(0.1,0.5,0.1), gamma = c(0.02, 0.05, 0.1, 0.4, 0.8, 1, 2, 4, 8) ,kernel='radial',type='nu-regression'))
#  svmmodel <- tuneSVM$best.model
#  save(tuneSVM,file=paste("~/Dropbox/shared/MEMS-ANN/results/svm_model_",sensornumber,sep=""))
#  predictions_svm<-predict(svmmodel,sequencesdf)
#  #results[[sensornumber]] <- tuneResult

############################################
# tune LM
############################################
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
################################################
#  PLOT
################################################
# plot(predictions_lm,col='blue',type='l')
# lines(sequencesdf$target,col='red')
# lines(predictions_svm,col='green')
# title(main=paste("sensor",sensornumber,"RSME=",brsme,"E LM_RSME=",lmrsme))
# title(main=paste("sensor",sensornumber,"RSME=",brsme,"E SVM_RSME=",tuneSVM$best.performance, "E LM_RSME=",tuneLM$best.performance))
#title(main=paste("sensor",sensornumber,"RSME=", "E LM_RSME=",tuneLM$best.performance))


#############################################
#  Save PLOT 
#############################################
# dev.copy(png,paste("~/Dropbox/shared/MEMS-ANN/results/svm_model_",sensornumber,".png",sep=""))
# dev.off()
