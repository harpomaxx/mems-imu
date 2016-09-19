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
imu_data[4]=paste(dataset_dir,"glad_xbow1_target.txt",sep="") # xbow based on GLAD frequency

imu_data[5]=paste(dataset_dir,"glad_data.txt",sep="")
imu_data[6]=paste(dataset_dir,"glad_xbow1_target.txt",sep="")
imu_data[7]=paste(dataset_dir,"glad_data.txt",sep="")
imu_data[8]=paste(dataset_dir,"glad_data.txt",sep="")

## TARGET IMU Data
imu_target=c()
imu_target[1]=paste(dataset_dir,"glad_H764_sgolay_target.txt",sep="")
imu_target[2]=paste(dataset_dir,"xbow1_H764_sgolay_target.txt",sep="")
imu_target[3]=paste(dataset_dir,"xsns1_H764_sgolay_target.txt",sep="")
imu_target[4]=paste(dataset_dir,"glad_H764_target.txt",sep="") # target based on GLad Frequency


imu_target[5]=paste(dataset_dir,"glad_H764_target.txt",sep="")
imu_target[6]=paste(dataset_dir,"glad_H764_target.txt",sep="")
imu_target[7]=paste(dataset_dir,"glad_xbow1_target.txt",sep="") # xbow based on GLAD frequency
imu_target[8]=paste(dataset_dir,"glad_xbow1_sgolay_target.txt",sep="") # xbow based on GLAD frequency + SGOLAY

selected_imus<-c(1,5,6,7,8)
selected_sensors<-c(1,2,6)

selected_taps<-c(1,seq(5,100,5))

#
# DO NOT TOUCH BEYOND THIS LINE
#
sensor_type=list(X1="AccX",X2="AccY",X3="AccZ",X4="GyroX",X5="GyroY",X6="GyroZ")
log.socket <- make.socket(port=2222)