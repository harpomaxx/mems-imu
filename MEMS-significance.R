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
    print(tap)
    form<-formula(paste(sensor,'~ timedelay',sep=""))
    anovaa<-oneway.test(form,data=cv_grouped_dataset[cv_grouped_dataset$timedelay>tap,])
    
    if (anovaa$p.value >0.05){
      anovaainv<-oneway.test(form,data=cv_grouped_dataset[cv_grouped_dataset$timedelay<=tap,])
      print(paste(" >",tap,"|",round(anovaa$p.value,4),"| <=",tap,"|",anovaainv$p.value))
      break
    }
  }
  return (tap)
}

# select best model considering multiple model types ---------------------------------------
best_tap_anova_ordered_model <-function (cv_grouped_dataset,sensor)
{
  library(dplyr)
  require("lazyeval")
  library(gsubfn)
  # adding a new id for ordering (timedelay*model)
  cv_grouped_dataset$timedelay_model=apply(cv_grouped_dataset,1,function(x) paste(as.numeric(x[1]),substr(x[2],4,nchar(x[2])),sep="_"))
  
  # calculating mean by timedelay*model
  taps_order= as.data.frame(cv_grouped_dataset %>% group_by(timedelay_model) %>% summarise_(
    .dots = fn$list(mean = "mean($sensor)")))
  # decreasing order
  taps_order=taps_order[order(taps_order$mean,decreasing = T ), c("timedelay_model")]
  for (tapn in  seq(1,length(taps_order))) {
    cv_subset=cv_grouped_dataset[,c(sensor,"timedelay_model","timedelay","model")] %>% filter(timedelay_model %in%  taps_order[tapn:length(taps_order)])
    cv_subset_opposite=cv_grouped_dataset[,c(sensor,"timedelay_model","timedelay","model")] %>% filter(timedelay_model %in% taps_order[1:tapn])
    
    # checking for a minimal tap number
    if (nrow(cv_subset)>10){
      form<-formula(paste(sensor,'~ timedelay_model',sep=""))
      anovaa<-oneway.test(form,data=cv_subset)
      
      # There is no significant difference between the groups
      if (anovaa$p.value >0.05){
        anovaainv<-oneway.test(form,data=cv_subset_opposite)
        print(paste(" >=",taps_order[tapn],"|",round(anovaa$p.value,6),"| <",taps_order[tapn],"|",round(anovaainv$p.value,6)))
        taps_order_subset=taps_order[tapn:length(taps_order)]
        
        print(taps_order_subset)
        
        group_tap = as.data.frame(cv_subset %>% group_by(timedelay,model) %>% summarise_(
          .dots = fn$list(mean = "mean($sensor)"))) %>% mutate(modelnum=substr(model,4,nchar(as.character(model))))
        # The smallest mlp topology is prefered.
        best_model= as.character(group_tap[which.min(group_tap$timedelay*as.numeric(as.character(group_tap$modelnum))),]$model)
        best_tap = as.numeric(group_tap[which.min(group_tap$timedelay*as.numeric(as.character(group_tap$modelnum))),]$timedelay)
        break
      }
    }else{
      best_tap= as.numeric(unlist(strsplit(taps_order[tapn],"_"))[1])
      best_model= paste("mlp",unlist(strsplit(taps_order[tapn],"_"))[2],sep="")
      break
    }
  }
  return (list(best_tap,best_model))
}

# select best model  ---------------------------------------
best_tap_anova_ordered <-function (cv_grouped_dataset,sensor)
{
  library(dplyr)
  require("lazyeval")
  library(gsubfn)
  # calculating mean by timedelay
  taps_order= as.data.frame(cv_grouped_dataset %>% group_by(timedelay) %>% summarise_(
    .dots = fn$list(mean = "mean($sensor)")))
  # decreasing order
  taps_order=taps_order[order(taps_order$mean,decreasing = T ), c("timedelay")]
  print(taps_order)
  for (tapn in  seq(1,length(taps_order))) {
    cv_subset=cv_grouped_dataset[,c(sensor,"timedelay")] %>% filter(timedelay %in%  taps_order[tapn:length(taps_order)])
    cv_subset_opposite=cv_grouped_dataset[,c(sensor,"timedelay")] %>% filter(timedelay %in% taps_order[1:tapn])

    # checking for a minimal tap number
    if (nrow(cv_subset)>10){
      form<-formula(paste(sensor,'~ timedelay',sep=""))
      anovaa<-oneway.test(form,data=cv_subset)
      
      # There is no significant difference between the groups
      if (anovaa$p.value >0.05){
        anovaainv<-oneway.test(form,data=cv_subset_opposite)
        print(paste(" >=",taps_order[tapn],"|",round(anovaa$p.value,4),"| <",taps_order[tapn],"|",anovaainv$p.value))
        taps_order_subset=taps_order[tapn:length(taps_order)]
        
        #if (taps_order[tapn] > taps_order_subset[order(taps_order_subset,decreasing = F)][1] ){
            print(  taps_order_subset[order(taps_order_subset,decreasing = T)] )
            best_tap = as.data.frame(cv_subset %>% group_by(timedelay) %>% summarise_(
          .dots = fn$list(mean = "mean($sensor)")))
            best_tap = best_tap[which.min(best_tap$timedelay),]$timedelay
            break
        #}else{
        #  best_tap=taps_order[tapn]
         # break
        #}
      }
    }else{
      best_tap=taps_order[tapn]
      break
    }
  }
  return (best_tap)
}

# select the number of taps for multiple MLP models ---------------------------------------------
select_n_taps_mlp<- function(results_data,imu_name,sel_sensors){
  results_l<-data.frame()
  results_data_means<-aggregate(results_data[, c(sel_sensors,paste(sel_sensors,"_t_rsme",sep=""))], list(results_data$timedelay,results_data$model), mean)
  results_data_sd<-aggregate(results_data[, c(sel_sensors,paste(sel_sensors,"_t_rsme",sep=""))], list(results_data$timedelay,results_data$model), sd)
  
  for (sensorid in seq(1,length(sel_sensors))){
    best<-unlist(best_tap_anova_ordered_model(results_data,sel_sensors[sensorid]))
    best_model<-best[2]
    best_tap<-as.integer(best[1])
    
    rsme_mean<-results_data_means[(results_data_means$Group.1==best_tap & 
                                results_data_means$Group.2==best_model),][sel_sensors[sensorid]]
    
    rsme_sd<-results_data_sd[(results_data_sd$Group.1==best_tap & 
                                results_data_sd$Group.2==best_model),][sel_sensors[sensorid]]
    raw_rsme_mean<-results_data_means[(results_data_means$Group.1==best_tap & 
                                results_data_means$Group.2==best_model),][paste(sel_sensors[sensorid],"_t_rsme",sep="")]
    raw_rsme_sd<-results_data_sd[(results_data_sd$Group.1==best_tap &  
                                results_data_sd$Group.2==best_model),][paste(sel_sensors[sensorid],"_t_rsme",sep="")]
    row=cbind(imu_name,sel_sensors[sensorid],best_tap,best_model,rsme_mean,rsme_sd,raw_rsme_mean,raw_rsme_sd)
    names(row)<-c("imu_name", "sensorID","tap","model","rsme_mean","rsme_sd", "raw_rsme_mean","raw_rsme_sd")
    results_l<-rbind(results_l,row)
                     
  }
  return(results_l)
}


# select the number of taps generic models ---------------------------------------------
select_n_taps<- function(results_data,imu_name,sel_sensors){
  results_l<-data.frame()
  results_data_means<-aggregate(results_data[, c(sel_sensors,paste(sel_sensors,"_t_rsme",sep=""))], list(results_data$timedelay), mean)
  results_data_sd<-aggregate(results_data[, c(sel_sensors,paste(sel_sensors,"_t_rsme",sep=""))], list(results_data$timedelay), sd)
  
  for (sensorid in seq(1,length(sel_sensors))){
    best_tap<-best_tap_anova_ordered(results_data,sel_sensors[sensorid])
    rsme_mean<-results_data_means[(results_data_means$Group.1==best_tap),][sel_sensors[sensorid]]
    rsme_sd<-results_data_sd[(results_data_sd$Group.1==best_tap),][sel_sensors[sensorid]]
    raw_rsme_mean<-results_data_means[(results_data_means$Group.1==best_tap),][paste(sel_sensors[sensorid],"_t_rsme",sep="")]
    raw_rsme_sd<-results_data_sd[(results_data_sd$Group.1==best_tap),][paste(sel_sensors[sensorid],"_t_rsme",sep="")]
    
    row=cbind(imu_name,sel_sensors[sensorid],best_tap,rsme_mean,rsme_sd,raw_rsme_mean,raw_rsme_sd)
    names(row)<-c("imu_name", "sensorID","tap","rsme_mean","rsme_sd", "raw_rsme_mean","raw_rsme_sd")
    results_l<-rbind(results_l,row)
  }
  return(results_l)
}

