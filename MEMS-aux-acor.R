source("MEMS-config.R",echo = F)
source("MEMS-evaluation.R",echo = F)
source("MEMS-viz.R",echo = F)
source("MEMS-significance.R",echo = F)

calculate_rsme<-function(){
  for (imu in selected_imus){
    rmse=c()
    for(sensor in selected_sensors){
      testset_total<- seqdata(sensor,1,imu, datasetsize = c(imu_datainterval[[imu]]$start,imu_datainterval[[imu]]$end))
      #testset_total<- seqdata(sensor,1,imu, datasetsize = c(1,imu_datainterval[[imu]]$start))
      rmse=cbind(rmse,rsme(testset_total$V1,testset_total$target))
    }
    print(cbind(imu,rmse))
  }
}
calculate_acor_train<-function(){
  set.seed(300)
  x=c()
  for (imu in selected_imus){
    res=c()
    for(sensor in selected_sensors){
      testset_total<- seqdata(sensor,1,imu, datasetsize = c(1,imu_datainterval[[imu]]$start))
      cor=c()
      for ( n in 1:100){
        pos=sample(1:(length(testset_total$V1)-1000),1)
        cor_n<-acf(testset_total$V1[pos:(pos+1000)],1000,plot = F)$acf
        cor<-cbind(cor,acf=which(cor_n<2/sqrt(1000))[1])
      }
      #res=c(res,c(sensor,mean(cor),sd(cor)))
      res=c(res,c(sensor,cor))
    }
    x=rbind(x,c(imu,res))
  }
  return(x)
}

calculate_acor_test<-function(){
  set.seed(300)
  x=c()
  for (imu in selected_imus){
    res=c()
    for(sensor in selected_sensors){
      testset_total<- seqdata(sn = sensor,tm = 1,imunum = imu, datasetsize = c(imu_datainterval[[imu]]$start,imu_datainterval[[imu]]$end))
      cor=c()
      for ( n in 1:100){
        pos=sample(1:(length(testset_total$V1)-1000),1)
        cor_n<-acf(testset_total$V1[pos:(pos+1000)],1000,plot = F)$acf
        cor<-cbind(cor,acf=which(cor_n<0.2)[1])
      }
      #res=c(res,c(sensor,mean(cor),sd(cor)))
      res=c(res,c(sensor,cor))
    }
    x=rbind(x,c(imu,res))
  }
  return(x)
}