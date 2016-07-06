# Scripts for searching the correct number of taps by using ANOVA
# Then, it calculate sthe RSME for the MLR using the number of taps found.
a <-function(){
#g<-read.csv(file="~/Dropbox/shared/MEMS-ANN/datasets/test/glad_data.txt_lm__cv_results-2csv",sep=',',header=T)
g<-read.csv(file="~/Dropbox/shared/MEMS-ANN/results_Navigation_journal/data/glad_data.txt_lm__cv_results.csv",sep=',',header=T)

b<-read.csv(file="~/Dropbox/shared/MEMS-ANN/results_Navigation_journal/data/xbow1_data.txt_lm__cv_results.csv",sep=',',header=T)
s<-read.csv(file="~/Dropbox/shared/MEMS-ANN/results_Navigation_journal/data/xsns1_data.txt_lm__cv_results.csv",sep=',',header=T)

g_means<-aggregate(g[, c(3,4,5,6,7,8,9,10,11,12,13,14)], list(g$timedelay), mean)
g_sd<-aggregate(g[, c(3,4,5,6,7,8,9,10,11,12,13,14)], list(g$timedelay), sd)

b_means<-aggregate(b[, c(3,4,5,6,7,8,9,10,11,12,13,14)], list(b$timedelay), mean)
b_sd<-aggregate(b[, c(3,4,5,6,7,8,9,10,11,12,13,14)], list(b$timedelay), sd)

s_means<-aggregate(s[, c(3,4,5,6,7,8,9,10,11,12,13,14)], list(s$timedelay), mean)
s_sd<-aggregate(s[, c(3,4,5,6,7,8,9,10,11,12,13,14)], list(s$timedelay), sd)



grouped_dataset=list(g,b,s)
grouped_dataset_mean=list(g_means,b_means,s_means)
grouped_dataset_sd=list(g_sd,b_sd,s_sd)

sensors=c(1,2,3,4,5,6)

for (groupid in seq(1,length(grouped_dataset))){
  for (sensorid in seq(1,length(sensors))){
    best_tap<-best_tap_anova(grouped_dataset[[groupid]],sensors[sensorid])
    print(paste('X',sensors[sensorid],sep=""))
    print(grouped_dataset_mean[[groupid]][grouped_dataset_mean[[groupid]]$Group.1==best_tap,][[paste('X',sensors[sensorid],sep="")]])
    print(grouped_dataset_sd[[groupid]][grouped_dataset_sd[[groupid]]$Group.1==best_tap,][[paste('X',sensors[sensorid],sep="")]])
    print(grouped_dataset_mean[[groupid]][grouped_dataset_mean[[groupid]]$Group.1==best_tap,][[paste('X',sensors[sensorid],"_t_rsme",sep="")]])
    print(grouped_dataset_sd[[groupid]][grouped_dataset_sd[[groupid]]$Group.1==best_tap,][[paste('X',sensors[sensorid],"_t_rsme",sep="")]])
  
  }
  print("----------------")
}
}

best_tap_anova <-function (cv_grouped_dataset,sensor)
{
  for (tap in  selected_taps){
    form<-formula(paste(sensor,'~ timedelay',sep=""))
    anovaa<-oneway.test(form,data=cv_grouped_dataset[cv_grouped_dataset$timedelay>tap,])
    
    if (anovaa$p.value >0.05){
      anovaainv<-oneway.test(form,data=cv_grouped_dataset[cv_grouped_dataset$timedelay<=tap,])
      ##print(paste(" >",tap,"|",anovaa$p.value,"| <=",tap,"|",anovaainv$p.value))
      break
    }
  }
  return (tap)
}

select_n_taps<- function(result_file,imu_name,sel_sensors){
  results_l<-c()
  g<-read.csv(file=result_file,sep=',',header=T)
  g_means<-aggregate(g[, c(sel_sensors,paste(sel_sensors,"_t_rsme",sep=""))], list(g$timedelay), mean)
  g_sd<-aggregate(g[, c(sel_sensors,paste(sel_sensors,"_t_rsme",sep=""))], list(g$timedelay), sd)
  
  for (sensorid in seq(1,length(sel_sensors))){
    best_tap<-best_tap_anova(g,sel_sensors[sensorid])
    rsme_mean<-g_means[g_means$Group.1==best_tap,][sel_sensors[sensorid]]
    rsme_sd<-g_sd[g_sd$Group.1==best_tap,][sel_sensors[sensorid]]
    raw_rsme_mean<-g_means[g_means$Group.1==best_tap,][paste(sel_sensors[sensorid],"_t_rsme",sep="")]
    raw_rsme_sd<-g_sd[g_sd$Group.1==best_tap,][paste(sel_sensors[sensorid],"_t_rsme",sep="")]
    results_l<-rbind(results_l,c(imu_name,sel_sensors[sensorid],best_tap,rsme_mean,rsme_sd,raw_rsme_mean,raw_rsme_sd))
  }
  colnames(results_l)<-c("imu_name", "sensorID","tap","rsme_mean","rsme_sd", "raw_rsme_mean","raw_rsme_sd")
  return(as.data.frame(results_l))
  }