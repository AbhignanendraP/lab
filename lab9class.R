rm(list=ls())

gd<- function(x,y,m,c,alpha,conv_thr,iter){
  iterations<-0
  Lf<-0
  while(iterations<=iter){
    y_p=m*x+c
    Lf_new<-0.5*sum(y_p-y)^2
    m=m-alpha*sum((y_p-y)*x)
    c=c-alpha*sum(y_p-y)
    if(abs(Lf-Lf_new)<=conv_thr){
      break
    }
    Lf=Lf_new
    iterations=iterations+1
  }
  return(paste("Optimal Intercept ",c,"Optimal slop ",m))
}

data<-mtcars
gd(data$wt,data$mpg,-0.2,32,0.001,0.0001,1000)
plot(data$wt,data$mpg)
reg<-lm(data$mpg~data$wt)
reg
abline(reg,col='red')




rm(list=ls())

gd1<- function(x1,x2,y,m1,m2,c,alpha,conv_thr,iter){
  iterations<-0
  Lf<-0
  while(iterations<=iter){
    y_p=m1*x1+m2*x2+c
    Lf_new<-0.5*sum(y_p-y)^2
    m1=m1-alpha*sum((y_p-y)*x1)
    m2=m2-alpha*sum((y_p-y)*x2)
    c=c-alpha*sum(y_p-y)
    if(abs(Lf-Lf_new)<=conv_thr){
      break
    }
    Lf=Lf_new
    iterations=iterations+1
  }
  return(paste("Optimal Intercept ",c,"Optimal slop one ",m1,"Optimal slop two ",m2))
}

data<-mtcars
view(data)
gd1(data$wt,data$drat,data$mpg,-0.2,-0.4,32,0.001,0.00001,10000)
reg1<-lm(data$mpg~(data$wt+data$drat))
reg1

