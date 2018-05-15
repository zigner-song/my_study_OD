  fct<-function(x){c(mean=mean(x),sd=sd(x),n=length(x))}
  
  
  library(doBy)
  library(ggplot2)
  library(reshape2)
  library(plyr)
  library(psych)

#clean the study2 data
  data_study2<-read.csv("data_study2.csv")
  data_study2<-subset(data_study2,data_study2$valid1==T & data_study2$time>140)
  data_study2$study<-rep("2",nrow(data_study2))
  
  data_pilot.study3<-read.csv("data_pilot.study3.csv")
  data_pilot.study3<-subset(data_pilot.study3,data_pilot.study3$valid==T)
  data_pilot.study3$study<-rep("3.0",nrow(data_pilot.study3))

#answers to the scale
  data_answers<-c(2,2,2,1,2,
                  1,1,2,1,2,
                  2,2,2,2,2,
                  2,2,1,1,1,
                  1,1,1)

#create the scale data

  data_scale2<-data_study2[c(1,77,13:35)]
  data_scale3<-data_pilot.study3[c(1,100,51:75)]
  data_scale3<-data_scale3[c(-10,-18)]
  data_scale<-rbind(data_scale2,data_scale3)
  remove(data_scale2);remove(data_scale3)
  
  ins.scale<-data.frame(data_scale[3:25]==data_answers)
  ins.scale[ins.scale==TRUE]<-1
  ins.scale[ins.scale==FALSE]<-0
  #data_scale[3:25]<-ins.scale
  #ins.scale<-ins.scale[c(-1,-17,-18,-20)]
  #ins.scale<-ins.scale[c(-1,-4,-7,-10,-17,-18,-20)]
  ins.scale<-ins.scale[c(-3,-4,-6,-7,-8,-12,-13,-14,-15,-19,-21)]
  alpha_scale<-alpha(ins.scale,check.keys=TRUE)
  print(alpha_scale)
  
  data_scale<-cbind(data_scale[1:2],ins.scale)
  data_scale$knowledge<-apply(ins.scale,1,sum)
  
#analyzing study2
  
  data_study2<-merge(data_study2,data_scale[c(1,2,ncol(data_scale))])
  
  
  
  
  
  