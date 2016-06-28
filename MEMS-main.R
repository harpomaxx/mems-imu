# MEMS-main
# function libraries
source("MEMS-config.R",echo = F)
source("MEMS-evaluation.R",echo = F)
source("MEMS-viz.R",echo = F)
source("MEMS-significance.R",echo = F)

#expcv(normaldata=F,shuffledata=F)

plot_taps_rsme_mean( paste(results_dir,"data/","glad_data.txt_lm__cv_results.csv",sep=""),"Gladiator",c("X1","X2","X6"))
plot_taps_rsme_boxplot( paste(results_dir,"data/","glad_data.txt_lm__cv_results.csv",sep=""),"Gladiator",c("X1","X2","X6"))

plot_taps_rsme_mean( paste(results_dir,"data/","xbow1_data.txt_lm__cv_results.csv",sep=""),"Crossbow",c("X1","X2","X6"))
plot_taps_rsme_boxplot( paste(results_dir,"data/","xbow1_data.txt_lm__cv_results.csv",sep=""),"Crossbow",c("X1","X2","X6"))

plot_taps_rsme_mean( paste(results_dir,"data/","xsns1_data.txt_lm__cv_results.csv",sep=""),"Xsens",c("X1","X2","X6"))
plot_taps_rsme_boxplot( paste(results_dir,"data/","xsns1_data.txt_lm__cv_results.csv",sep=""),"Xsens",c("X1","X2","X6"))

results=c()
results<-cbind(results,select_n_taps( paste(results_dir,"data/","glad_data.txt_lm__cv_results.csv",sep=""),"Gladiator",c("X1","X2","X6")))
results<-cbind(results,select_n_taps( paste(results_dir,"data/","xbow1_data.txt_lm__cv_results.csv",sep=""),"Crossbow",c("X1","X2","X6")))
results<-cbind(results,select_n_taps( paste(results_dir,"data/","xsns1_data.txt_lm__cv_results.csv",sep=""),"XSens",c("X1","X2","X6")))
               
View(results)

