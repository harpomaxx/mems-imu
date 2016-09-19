#MEMS Visualization script
# Harpo MAxx (2015-2016)
library(caret)
#######################################################
# Plot Observed vs predicted 
#######################################################
plot_obs_vs_pred <- function(){
  
  pdf("~/Dropbox/shared/MEMS-ANN/results/figs/glad-vs-cross-sens1.pdf",width=6,height=3.5)
  g_results<-pred_vs_obs(1,61,1)
  c_results<-pred_vs_obs(1,16,4) # imunum=4 xbow using glad frequency
  
  write.csv(g_results, file = "~/Dropbox/shared/MEMS-ANN/results/data/glad-sens1.csv" ,quote=F, row.names = F,sep=',')
  write.csv(c_results, file ="~/Dropbox/shared/MEMS-ANN/results/data/cross-sens1.csv",quote=F, row.names = F,sep=',')
  
  par(mgp=c(1.5,0.3,0.2))
  par(mar=c(5,4,2,2)+0.1)
  plot(g_results[,1],g_results[,1],col='blue',type='l',ylab="Predicted",xlab='Observed',main="AccX",cex.main=1.0,cex.lab=1.0, cex.axis=1.0)
  
  points(g_results[,1],g_results[,2],col='green')
  points(c_results[,1],c_results[,3],col='red')
  legend("topleft", c("Honeywell","Gladiator TD-MLR","Crossbow"), cex=0.8,pch=c(-1,1,1),lty = c(1,0,0), col=c("blue","green","red"), bty = "n")
  dev.off()
  
  pdf("~/Dropbox/shared/MEMS-ANN/results/figs/glad-vs-cross-sens2.pdf",width=6,height=3.5)
  g_results<-pred_vs_obs(2,56,1)
  c_results<-pred_vs_obs(2,31,4)
  write.csv(g_results, file = "~/Dropbox/shared/MEMS-ANN/results/data/glad-sens2.csv" ,quote=F, row.names = F,sep=',')
  write.csv(c_results, file ="~/Dropbox/shared/MEMS-ANN/results/data/cross-sens2.csv",quote=F, row.names = F,sep=',')
  
  par(mgp=c(1.5,0.3,0.2))
  par(mar=c(5,4,2,2)+0.1)
  plot(g_results[,1],g_results[,1],col='blue',type='l',ylab="Predicted",xlab='Observed',main="AccY",cex.main=1.0,cex.lab=1.0, cex.axis=1.0)
  points(g_results[,1],g_results[,2],col='green')
  points(c_results[,1],c_results[,3],col='red')
  legend("topleft", c("Honeywell","Gladiator TD-MLR","Crossbow"), cex=0.8,pch=c(-1,1,1),lty = c(1,0,0), col=c("blue","green","red"), bty = "n")
  dev.off()
  
  pdf("~/Dropbox/shared/MEMS-ANN/results/figs/glad-vs-cross-sens6.pdf",width=6,height=3.5)
  g_results<-pred_vs_obs(6,81,1)
  c_results<-pred_vs_obs(6,41,4)
  write.csv(g_results, file = "~/Dropbox/shared/MEMS-ANN/results/data/glad-sens6.csv" ,quote=F, row.names = F,sep=',')
  write.csv(c_results, file ="~/Dropbox/shared/MEMS-ANN/results/data/cross-sens6.csv",quote=F, row.names = F,sep=',')
  par(mgp=c(1.5,0.3,0.2))
  par(mar=c(5,4,2,2)+0.1)
  plot(g_results[,1],g_results[,1],col='blue',type='l',ylab="Predicted",xlab='Observed',main="GyroZ",cex.main=1.0,cex.lab=1.0, cex.axis=1.0)
  points(g_results[,1],g_results[,2],col='green')
  points(c_results[,1],c_results[,3],col='red')
  legend("topleft", c("Honeywell","Gladiator TD-MLR","Crossbow"), cex=0.8,pch=c(-1,1,1),lty = c(1,0,0), col=c("blue","green","red"), bty = "n")
  dev.off()
}

########################################################
#
########################################################
plot_taps_rsme_mean<- function(result_file,imu_name,sel_sensors){
  colors=rainbow(length(sel_sensors))
  g<-read.csv(file=result_file,sep=',',header=T)
  g_means<-aggregate(g[, sel_sensors], list(g$timedelay), mean)
  pdf(paste(results_dir,"figs/",imu_name,"_mean.pdf",sep=""),width=6,height=3.5)
  par(mgp=c(1.5,0.3,0.2))
  par(mar=c(5,4,2,2)+0.1)
  i=0
  plot(g_means$Group.1,g_means[[sel_sensors[1]]],xaxt='n',t='b',cex=0.4,ylim=c(0,max(g_means[sel_sensors])),col=colors[i],ylab=" AVG.RSME",xlab="Number of taps",
       main=imu_name,cex.main=1.0,cex.lab=1.0, cex.axis=1.0)
  for (sensor in sel_sensors[1:length(sel_sensors)]){
      i=i+1 
       lines(g_means$Group.1,g_means[[sensor]],xaxt='n',t='b',cex=0.4,col=colors[i])
     
  }
  axis(1,at=selected_taps,cex.axis=1.0)
  legend(80,max(g_means[sel_sensors]), sensor_type[c(sel_sensors)], cex=0.8,pch=1, col=colors, bty = "n")
  dev.off()
}


plot_taps_rsme_boxplot<- function(result_file,imu_name,sel_sensors, first_tap=4){
  i=1
  colors=rainbow(length(sel_sensors))
  g<-read.csv(file=result_file,sep=',',header=T)
  taps=selected_taps[first_tap: length(selected_taps)]
  for (sensor in sel_sensors){
    pdf(paste(results_dir,"figs/",imu_name,"_",sensor,".pdf",sep=""),width=6,height=3.5)
    par(mgp=c(1.5,0.3,0.2))
    par(mar=c(5,4,2,2)+0.)
    boxplot_data=unstack(g, formula(paste(sensor,"~timedelay",sep="") ))
    boxplot(boxplot_data[first_tap:length(boxplot_data)],
      xaxt='n',ylab="RSME",xlab="Number of taps",
      col=colors[i],
      main=sensor_type[[sensor]],cex.main=1.0,cex.lab=1.0, cex.axis=1.0
    )
    axis(1,at=seq(1,length(taps)),labels=taps,cex.axis=1.0)
    i=i+1
    dev.off()
  }
}

#
# Plot the weight lag distribution of a given lm model
#
plotweights <-function(lm_model,imu_name,sensor){
  pdf(paste(results_dir,"figs/weights_",imu_name,"_",sensor,".pdf",sep=""),width=8,height=5.5)
  weights=rev(lm_model$coefficients[2:length(lm_model$coefficients)])  
  
  wbplot=barplot(weights,las=2,names=sprintf("t-%s",seq(1,length(weights))),
                 col=rgb(1,0,0,1/4),
                 border='skyblue',space=0.1,  cex.lab=0.6,
                 cex.axis = 0.6, xaxt='n',ylim=c(0,max(weights)+0.01))#,ylim=c(min(weights),max(weights)))
  
  text(wbplot,0.0055, labels=sprintf("t-%s",seq(1,length(weights))), col="black",cex=0.5,srt=90)
  
  fitted=loess.smooth(x=seq(1,length(weights)),y=weights,evaluation = length(weights))$y
  lines(wbplot,fitted+0.015,col=rgb(0,0,1,1/4))
  points(wbplot,fitted+0.015,col=rgb(0,0,1,1/4),cex=0.5,pch=20)
  dev.off()
}     

#
# Comparison of 3 time series using boxplots
# TODO: need parametrization
boxplot_time_series<-function()
{
  glad=as.data.frame(fread(imu_data[1],sep=" ",header=F))
  xbow=as.data.frame(fread(imu_data[4],sep=" ",header=F))
  h764=as.data.frame(fread(imu_target[6],sep=" ",header=F))
  glad_sensors=cbind(glad[,c(1,2,6)],rep("Gladiator"),deparse.level=0)
  names(glad_sensors)=c("AccX","AccY","GyroZ","IMU")
  xbow_sensors=cbind(xbow[,c(1,2,6)],rep("Crossbow"))
  names(xbow_sensors)=c("AccX","AccY","GyroZ","IMU")
  h764_sensors=cbind(h764[,c(1,2,6)],rep("Honeywell"))
  names(h764_sensors)=c("AccX","AccY","GyroZ","IMU")
  
  dynamic1=rbind(glad_sensors[1:(nrow(glad_sensors)/2),],
                 xbow_sensors[1:(nrow(xbow_sensors)/2),],
                 h764_sensors[1:(nrow(h764_sensors)/2),])
  
  dynamic2=rbind(glad_sensors[(nrow(glad_sensors)/2):nrow(glad_sensors),],
                 xbow_sensors[(nrow(xbow_sensors)/2):nrow(xbow_sensors),],
                 h764_sensors[(nrow(h764_sensors)/2):nrow(h764_sensors),])
  pdf(paste(results_dir,"figs/boxplot_ts_1_.pdf",sep=""),width=10,height=4.5)
  featurePlot(x = dynamic1[, 1:3], 
              y = dynamic1$IMU, 
              plot = "box", 
              ## Pass in options to bwplot() 
              scales = list(y = list(relation="free",cex=1.2),
                            x = list(rot = 90,cex=1.2)),
              ylim=list(c(-4, 4), c(-5, 5),c(-0.6,0.5)),
              layout = c(3,1 ), 
              auto.key = list(columns = 2),
              do.out = F,main="Trayectory 1 (T1)")
  dev.off()
  pdf(paste(results_dir,"figs/boxplot_ts_2_.pdf",sep=""),width=10,height=4.5)
  
  featurePlot(x = dynamic2[, 1:3], 
              y = dynamic2$IMU, 
              plot = "box", 
              ## Pass in options to bwplot() 
              scales = list(y = list(relation="free",cex=1.2),
                            x = list(rot = 90,cex=1.2)),  
              ylim=list(c(-4, 4), c(-5, 5),c(-0.6,0.5)),
              layout = c(3,1 ), 
              auto.key = list(columns = 2),
              do.out = F,main="Trayectory 2 (T2)")
  dev.off()
  
  
#  boxp=boxplot(dataset[,c(1,7,13,2,8,14,6,12,18)],
#        at=c(1,1.9,2.8,  4.0,4.9,5.8, 7.0,7.9,8.9),xaxt='n',varwidth = T,col=c("pink","skyblue","orange"),outline = F,ylab='Sensor Value')
#  #plot(boxp)
#  text( 
#    c(1:9)-0.1 , 
#    boxp$stats[nrow(boxp$stats) , ]+0.2 , 
#    rep(c("glad","xbow","h764"),3),cex=0.7  
#  )
#  abline(v = 3.4, col = "black")
#  abline(v = 6.4, col = "black")
#  axis(side=1, at=c(2,5,8), 
#       labels=c('AccX',
#                'AccY',
#                'GyroZ'), line=0.5, lwd=0)
  
}  
