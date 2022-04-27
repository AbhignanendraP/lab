rm(list=ls())
setwd("C:/Abhi notes/class3-2/eda/lab/Lab 7")
data1<-read.csv("iris.csv")
View(data1)
df<-scale(data1)
fit<-kmeans(df,centers=2)
fit$cluster
fit$size
fit$withinss
fit$tot.withinss
Kmax<-15
wcss<-rep(NA,Kmax)
nClust<- list()
for(i in 1:Kmax){
  fit<-kmeans(df,i)
  wcss[i]<-fit$tot.withinss
  nClust[[i]]<-fit$size
}
plot(1:Kmax,wcss,type="b",pch=19)
fit<-kmeans(df,centers=3)
fit$cluster
fit$size
fit$center
library(factoextra)
fviz_nbclust(df, kmeans, method = "wss")
fviz_cluster(fit, data1)
library(cluster)
fitm <- pam(df, 3, metric = "manhattan")
fitm
fitm$medoids
fviz_cluster(fitm, data1)


rm(list=ls())
setwd("C:/Abhi notes/class3-2/eda/lab/Lab 7")
data2<-read.csv("USArrests.csv")
view(data2)
data2<-data2[,-1]
df1<-scale(data2)
fit1<-kmeans(df1,centers=2)
fit1$cluster
fit1$size
fit1$withinss
fit1$tot.withinss
Kmax1<-15
wcss1<-rep(NA,Kmax1)
nClust1<- list()
for(i in 1:Kmax1){
  fit1<-kmeans(df1,i)
  wcss1[i]<-fit1$tot.withinss
  nClust1[[i]]<-fit1$size
}
plot(1:Kmax1,wcss1,type="b",pch=19)
fit1<-kmeans(df1,centers=3)
fit1$cluster
fit1$size
fit1$center
library(factoextra)
fviz_nbclust(df1, kmeans, method = "wss")
fviz_cluster(fit1, data2)
library(cluster)
fitm1 <- pam(df1, 3, metric = "manhattan")
fitm1
fitm1$medoids
fviz_cluster(fitm1, data2)
