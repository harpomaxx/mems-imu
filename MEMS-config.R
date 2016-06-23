# MEMS Config File
# select the number of cores to 
library("doMC")
registerDoMC(cores=5)

slicing=1 # slice windows
kfolds=10
dataset_dir="~/Dropbox/shared/MEMS-ANN/datasets/"
results_dir="~/Dropbox/shared/MEMS-ANN/results/"

## Datasets
imu_data=c()

## INPUT IMU Data
imu_data[1]=paste(dataset_dir,"glad_data.txt",sep="")
imu_data[2]=paste(dataset_dir,"xbow1_data.txt",sep="")
imu_data[3]=paste(dataset_dir,"xsns1_data.txt",sep="")
imu_data[4]=paste(dataset_dir,"glad_xbow1_target.txt") # xbow based on GLAD frequency
imu_data[5]=paste(dataset_dir,"glad_data.txt",sep="")

## TARGET IMU Data
imu_target=c()
imu_target[1]=paste(dataset_dir,"glad_H764_target.txt",sep="")
imu_target[2]=paste(dataset_dir,"xbow1_H764_target.txt",sep="")
imu_target[3]=paste(dataset_dir,"xsns1_HT764_target.txt",sep="")
imu_target[4]=paste(dataset_dir,"glad_HT764_target.txt",sep="") # target based on GLad Frequency
imu_target[5]=paste(dataset_dir,"glad_xbow1_target.txt",sep="") # xbow based on GLAD frequency

selected_imus<-c(1,2,3)
selected_sensors<-c(1,2,3,4,5,6)
d_divisor=2 #divisor for using the half of dataset for training and the rest for testing.
selected_taps<-c(1,seq(5,100,5))

log.socket <- make.socket(port=4000)