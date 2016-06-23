#MEMS Visualization script
# Harpo MAxx (2015-2016)

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
  g<-read.csv(file=result_file,sep=',',header=T)
  g_means<-aggregate(g[, sel_sensors], list(g$timedelay), mean)
  pdf(paste(results_dir,"figs/",imu_name,"_mean.pdf",sep=""),width=6,height=3.5)
  par(mgp=c(1.5,0.3,0.2))
  par(mar=c(5,4,2,2)+0.1)
  plot(g_means$Group.1,g_means$X1,xaxt='n',t='b',cex=0.4,ylim=c(0.005,0.95),col='red',ylab=" AVG.RSME",xlab="Number of taps",
       main=imu_name,cex.main=1.0,cex.lab=1.0, cex.axis=1.0)
  lines(g_means$Group.1,g_means$X2,xaxt='n',t='b',cex=0.4,col='blue')
  lines(g_means$Group.1,g_means$X6,xaxt='n',t='b',cex=0.4,col='green')
  axis(1,at=selected_taps,cex.axis=1.0)
  legend(80,0.8, c("AccX","AccY","GyroZ"), cex=0.8,pch=1, col=c("red","blue","green"), bty = "n")
  dev.off()
}


plot_taps_rsme_boxplot<- function(result_file,imu_name,sel_sensors, first_tap=7){
  i=1
  sensor_type=c("AccX","AccY","GyroZ")
  colors=c("red","blue","green")
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
      main=sensor_type[i],cex.main=1.0,cex.lab=1.0, cex.axis=1.0
    )
    axis(1,at=seq(1,length(taps)),labels=taps,cex.axis=1.0)
    i=i+1
    dev.off()
  }
}
  