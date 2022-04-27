data <- data.frame(sale.count=c(40,60,70,30,50,30,30,10,70,60,50,60,30,20,20),
                   type=c("Can-A","Can-A","Can-A","Can-A","Can-A","Can-B","Can-B","Can-B","Can-B","Can-B","Can-C","Can-C","Can-C","Can-C","Can-C"))
library(dplyr)
group_by(data,type)%>%summarise(count=n(),mean=mean(sale.count,na.ra=TRUE))
ANOVA<-aov(sale.count~type,data=data)
summary(ANOVA)


setwd("C:/Abhi notes/class3-2/eda/lab")
library(dplyr)
data<-read.csv("color-anova-example.csv")
group_by(data,color)%>%summarise(count=n(),mean=mean(response,na.ra=TRUE))
ANOVA<-aov(response~color,data=data)
summary(ANOVA)
TukeyHSD(ANOVA)

