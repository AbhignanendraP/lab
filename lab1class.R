rm(list=ls())
#independent variable
x<-1:20
#dependent variable
y=x+rnorm(20)
plot(x,y,main='scatter plot')
cor.test(x,y)
lmodel<- lm(y~x)
abline(lmodel,col='red')
summary(lmodel)
a=data.frame(x=3)
result=predict(lmodel,a)
result
y
library(dplyr)
data1<-mtcars
train=sample_n(data1,15)
train
x=data1$wt
y=data1$mpg
#setwd()

plot(train$wt,train$mpg,main="scatter plot for mpg and weight of mt cars")
