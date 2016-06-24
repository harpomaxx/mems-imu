# MEMS-main
# function libraries
source("MEMS-config.R",echo = F)
source("MEMS-evaluation.R",echo = F)
source("MEMS-viz.R",echo = F)
source("MEMS-significance.R",echo = F)

#expcv(normaldata=F,shuffledata=F)
#plot_taps_rsme_mean( paste(results_dir,"data/","glad_data.txt_lm__cv_results.csv",sep=""),"Gladiator",c("X1","X1_t_rsme"))
#plot_taps_rsme_boxplot( paste(results_dir,"data/","glad_data.txt_lm__cv_results.csv",sep=""),"Gladiator",c(3,4),"red")

#plot_taps_rsme_boxplot( "~/Dropbox/shared/MEMS-ANN/results_Navigation_journal/data/glad_data.txt_lm__cv_results.csv",
#                        "Gladiator",c("X1","X2","X6"))
#plot_taps_rsme_mean( "~/Dropbox/shared/MEMS-ANN/results_Navigation_journal/data/glad_data.txt_lm__cv_results.csv",
#                     "Gladiator",c("X1","X2","X6"))

select_n_taps( "~/Dropbox/shared/MEMS-ANN/results_Navigation_journal/data/glad_data.txt_lm__cv_results.csv","Gladiator",c("X1","X2","X6"))