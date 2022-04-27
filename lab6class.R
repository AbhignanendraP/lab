rm(list=ls())
wdbc<-read.table(file.choose(),sep=',')
view(wdbc)
wdbc<-wdbc[,-1]
mynorm<-function(x){((x-min(x))/(max(x)-min(x)))}
mydata<-as.data.frame(lapply(wdbc[,-1], mynorm))
summary(wdbc[,2:5])
summary(mydata[,1:4])
train<-mydata[1:400,]
test<-mydata[401:569,]
library(class)
pred<-knn(train,test,wdbc[1:400,1],k=21)
cf<-table(pred,wdbc[401:569,1])
cf
acc=(cf[[1,1]]+cf[[2,2]])/sum(cf)
acc
