# Script for calculating optimal lag using AIC
#
#lagv=15
prev_aicv=0
df=c()
for (lagv in c(1,seq(5,100,5))){
  
  data_t=seqdata(1,lagv,6)
  data_t=data_t[sample(nrow(data_t),10000),]
  lm_model=lm(target ~ .,data_t)
  aicv=AIC(lm_model)
  print(paste(lagv,aicv,(aicv - prev_aicv)/2))
  df=rbind(df,cbind(lagv,(aicv - prev_aicv)/2 ))
}
d=as.data.frame(df)
plot_ly(x=d[,1],y=d[,2],type='bar')
