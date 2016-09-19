# MEMS-main
# function libraries
source("MEMS-config.R",echo = F)
source("MEMS-evaluation.R",echo = F)
source("MEMS-viz.R",echo = F)
source("MEMS-significance.R",echo = F)


#expcv(normaldata=F,shuffledata=F)



#plot_taps_rsme_mean( paste(results_dir,"data/","glad_data.txt_lm__cv_results.csv",sep=""),"Gladiator",c("X1","X2","X6"))
#plot_taps_rsme_boxplot( paste(results_dir,"data/","glad_data.txt_lm__cv_results.csv",sep=""),"Gladiator",c("X1","X2","X6"))

#plot_taps_rsme_mean( paste(results_dir,"data/","xbow1_data.txt_lm__cv_results.csv",sep=""),"Crossbow",c("X1","X2","X6"))
#plot_taps_rsme_boxplot( paste(results_dir,"data/","xbow1_data.txt_lm__cv_results.csv",sep=""),"Crossbow",c("X1","X2","X6"))

#plot_taps_rsme_mean( paste(results_dir,"data/","xsns1_data.txt_lm__cv_results.csv",sep=""),"Xsens",c("X1","X2","X6"))
#plot_taps_rsme_boxplot( paste(results_dir,"data/","xsns1_data.txt_lm__cv_results.csv",sep=""),"Xsens",c("X1","X2","X6"))

results_glad_ht764_sgolay=select_n_taps( paste(results_dir,"data/","glad_data.txt_glad_H764_sgolay_target.txt_lm__cv_results.csv",sep=""),"Gladiator",c("X1","X2","X6"))
results_glad_ht764=select_n_taps( paste(results_dir,"data/","glad_data.txt_glad_H764_target.txt_lm__cv_results.csv",sep=""),"Gladiator",c("X1","X2","X6"))
results_xbow_ht764=select_n_taps( paste(results_dir,"data/","glad_xbow1_target.txt_glad_H764_target.txt_lm__cv_results.csv",sep=""),"Xbow",c("X1","X2","X6"))

results_glad_xbow=select_n_taps( paste(results_dir,"data/","glad_data.txt_glad_xbow1_target.txt_lm__cv_results.csv",sep=""),"Xbow",c("X1","X2","X6"))
results_glad_xbow_slogay=select_n_taps( paste(results_dir,"data/","glad_data.txt_glad_xbow1_sgolay_target.txt_lm__cv_results.csv",sep=""),"Xbow",c("X1","X2","X6"))





#exptraintest(1,c(55,45,5,85))