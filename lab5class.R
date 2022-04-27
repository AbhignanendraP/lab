setwd("C:/Abhi notes/class3-2/eda/lab")
mydata<-read.csv("Social_Network_Ads.csv") 
mydata$Gender<-as.factor(mydata$Gender) 
mydata$Purchased<-as.factor(mydata$Purchased) 
mymodel <- glm(Purchased ~ Age+Gender+EstimatedSalary, data=mydata, family='binomial')
summary(mymodel)
res<-predict(mymodel,mydata,type='response') 
res 
cfmatrix<-table(Act=mydata$Purchased, pred=res>0.5) 
cfmatrix 
Acc=(cfmatrix[[1,1]]+cfmatrix[[2,2]])/sum(cfmatrix) 
Acc 

