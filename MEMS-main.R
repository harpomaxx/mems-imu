# MEMS-main
# function libraries
source("MEMS-config.R",echo = F)
source("MEMS-evaluation.R",echo = F)
source("MEMS-viz.R",echo = F)
source("MEMS-significance.R",echo = F)

mxnet_mlp_sweep()

#expcv(normaldata=F,shuffledata=F,model = 'mx.mlp',arguments)
#expcv(normaldata=F,shuffledata=F,model = 'lm')
expcv(normaldata = F,shuffledata = F,model='ma')


#plot_taps_rsme_mean( paste(results_dir,"/","ustrain_data.txt_ustrain_zero_target.txt_biglm__cv_results.csv",sep=""),"Ustrain",c("X1","X2","X6"))
#plot_taps_rsme_boxplot( paste(results_dir,"/","ustrain_data.txt_ustrain_zero_target.txt_biglm__cv_results.csv",sep=""),"Ustrain",c("X1","X2","X6"))
#plot_taps_rsme_mean( paste(results_dir,"/","glad_data.txt_glad_xbow1_target.txt_lm__cv_results.csv",sep=""),"Gladiator",c("X1","X2","X6"))
#plot_taps_rsme_boxplot( paste(results_dir,"/","glad_data.txt_glad_xbow1_target.txt_lm__cv_results.csv",sep=""),"Gladiator",c("X1","X2","X6"))


#results_ustrain_zero=select_n_taps( paste(results_dir,"/","ustrain_mixed_data.txt_ustrain_mixed_target.txt_biglm__cv_results.csv",sep=""),"Ustrain",c("X1","X2","X6"))
#results_glad_ht764=select_n_taps( paste(results_dir,"/","glad_data.txt_glad_H764_target.txt_biglm__cv_results.csv",sep=""),"Gladiator",c("X1","X2","X6"))
#results_glad_xbow=select_n_taps( paste(results_dir,"/","glad_data.txt_glad_xbow1_target.txt_lm__cv_results.csv",sep=""),"Gladiator",c("X1","X2","X6"))




#plot_taps_rsme_mean( paste(results_dir,"data/","xbow1_data.txt_lm__cv_results.csv",sep=""),"Crossbow",c("X1","X2","X6"))
#plot_taps_rsme_boxplot( paste(results_dir,"data/","xbow1_data.txt_lm__cv_results.csv",sep=""),"Crossbow",c("X1","X2","X6"))

#plot_taps_rsme_mean( paste(results_dir,"data/","xsns1_data.txt_lm__cv_results.csv",sep=""),"Xsens",c("X1","X2","X6"))
#plot_taps_rsme_boxplot( paste(results_dir,"data/","xsns1_data.txt_lm__cv_results.csv",sep=""),"Xsens",c("X1","X2","X6"))






#results_glad_ht764_sgolay=select_n_taps( paste(results_dir,"data/","glad_data.txt_glad_H764_sgolay_target.txt_lm__cv_results.csv",sep=""),"Gladiator",c("X1","X2","X6"))
#results_xbow_ht764=select_n_taps( paste(results_dir,"/","glad_xbow1_target.txt_glad_H764_target.txt_lm__cv_results.csv",sep=""),"Xbow",c("X1","X2","X6"))
#results_glad_xbow_slogay=select_n_taps( paste(results_dir,"data/","glad_data.txt_glad_xbow1_sgolay_target.txt_lm__cv_results.csv",sep=""),"Gladitor",c("X1","X2","X6"))




#l=list(X1=60,X2=40,X3=85)

#lch=list(X1=20,X2=20,X3=60)
#exptraintest(6,lch)

ma_results_files=c("glad_data.txt_glad_H764_target.txt_ma__cv_results.csv",
"glad_xbow1_target.txt_glad_H764_target.txt_ma__cv_results.csv",
"glad_xsns1_target.txt_glad_H764_target.txt_ma__cv_results.csv",
"glad_xtal_target.txt_glad_H764_target.txt_ma__cv_results.csv")

                   
for (i in seq(1,length(ma_results_files))){
  plot_taps_rsme_boxplot( paste(results_dir,"/",ma_results_files[i],
                             sep=""), paste("MA_",unlist(strsplit(ma_results_files[i],"_ma_")[[1]])[1],sep=""),
                          c("X1","X2","X6"),first_tap = 2)
}

lm_results_files=list.files(results_dir,pattern= "glad.*_lm_")
for (i in seq(1,length(lm_results_files))){
  plot_taps_rsme_mean( paste(results_dir,"/",lm_results_files[i],
                             sep=""), paste("MLR_",unlist(strsplit(lm_results_files[i],"_lm_")[[1]])[1],sep=""),c("X1","X2","X6"))
}

mlp_results_files=list.files(results_dir,pattern = ".*mlp.*_.*_.*")
for (i in seq(1,length(mlp_results_files))){
  plot_taps_rsme_boxplot( paste(results_dir,"/",mlp_results_files[i],
                             sep=""), paste("MLP_",mlp_results_files[i],sep=""),c("X1","X2","X6"),first_tap = 1)
  plot_taps_rsme_mean( paste(results_dir,"/",mlp_results_files[i],
                                sep=""), paste("MLP_",mlp_results_files[i],sep=""),c("X1","X2","X6"))
}
