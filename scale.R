#deal with the scale data in studies
#
###  step3
data_answers<-c(2,2,2,1,2,
                1,1,2,1,2,
                2,2,2,2,2,
                2,2,1,1,1,
                1,1,1)

scaleValid<-paste("item",c(1,3:17),sep="")
  #scale[c(-1#,-17,-18,-20,-12,-4,-21,-7,-5,-19,-14,-13,-6
  #                  )]

#read.data
#study1
data_scale1<-data_study1[c(paste("item",c(1,3:17),sep=""),"default","study","ID")]

data_scale1<-cbind(data_scale1.1,
                   data_study1[c("ID")])
alpha(data_scale1[paste("item",c(1,3:17),sep="")])
names(data_scale1)

knowledge.fml<-paste("data_scale1$item",c(1,3:17),sep="",collapse="+")
eval(parse(text=paste("data_scale1$knowledge<-",knowledge.fml,sep="")))

meansd<-function(x,...){
  my.mean<-mean(x,...)
  my.sd<-sd(x,...)
  y<-paste(floor(my.mean/16*1000)/10,"% ± ",
           floor(my.sd/16*1000)/10,"%",sep="")
  return(y)
}
summaryBy(knowledge~default,data_scale1,FUN=meansd)




data_scale2<-data_study2[c("ID","study",scale)]

#data_scale3.0<-data_pilot.study3[c("ID","study",scale)]
data_scale3<-data_study3[c("ID","study",scale)]
data_scale<-rbind(data_scale2,data_scale3)
#remove(data_scale2);remove(data_scale3);remove(data_scale3.0)

alpha()
data_scale[scale]<-data.frame(t(t(data_scale[scale])==data_answers))
data_scale[scale]<-as.numeric(data_scale[scale,])
data_scale[data_scale==TRUE]<-1
data_scale[data_scale==FALSE]<-0



data_scale2<-merge(data_scale[data_scale$study=="2",],data_study2[c("ID","study","version","default" )])
knowledge.fml<-paste("data_scale2$item",c(1,3:17),sep="",collapse="+")
eval(parse(text=paste("data_scale2$knowledge<-",knowledge.fml,sep="")))
ins<-summaryBy(knowledge~version,data_scale2,FUN=meansd)
ins

options(digits=3)
ins$inssss<-paste(floor(ins$knowledge.mean*100)/100,
                  floor(ins$knowledge.sd*100)/100,sep="±")
alpha(data_scale2[paste("item",c(1,3:17),sep="")])



data_scale3<-merge(data_scale[data_scale$study=="3",],data_study3[c("ID","study","version")])
data_scale3<-subset(data_scale3,version %in% 1:4)
knowledge.fml<-paste("data_scale3$item",c(1,3:17),sep="",collapse="+")
eval(parse(text=paste("data_scale3$knowledge<-",knowledge.fml,sep="")))
ins<-summaryBy(knowledge~version,data_scale3,FUN=meansd)
options(digits=3)
ins$inssss<-paste(floor(ins$knowledge.mean*100)/100,
                  floor(ins$knowledge.sd*100)/100,sep="±")
ins$versionName<-versionName_study3[1:4]
ins
data_study3 %>% names
alpha(data_scale3[paste("item",c(1,3:17),sep="")])


alpha_scale<-alpha(data_scale[scaleValid],check.keys = T)

data_scale$knowledge<-apply(data_scale[scaleValid],1,mean)

ins_scale3<-subset(data_scale,select=c("ID","study","knowledge"))
data_study3<-merge(data_study3,ins_scale3,by=c("ID","study"))
data_study2<-merge(data_study2,ins_scale3,by=c("ID","study"))
#rm(ins_scale3)




#  alpha 4 experience

ins<-rbind(data_study2[c("How_familiar","learning","T13","T14","T15","T16")],
           data_study3[c("How_familiar","learning","T13","T14","T15","T16")])
ins<-ins-1
alpha(ins[c(-1,-2)])
ins





#knowledge
data_scale_total<-rbind(data_scale1[c("ID","study",paste("item",c(1,3:17),sep=""))],
                        data_scale2[c("ID","study",paste("item",c(1,3:17),sep=""))],
                        data_scale3[c("ID","study",paste("item",c(1,3:17),sep=""))])
write.csv(data_scale_total,"data_scale.csv")

alpha(data_scale_total[c(-1,-2)])
options(digits=4)
summary_scale<-rbind(paste("item",c(1,3:17),sep="",collapse="+") %>% 
                       paste(.,"~study") %>% as.formula() %>% summaryBy(.,data_scale_total,FUN=mean),
                     c("total",apply(data_scale_total[paste("item",c(1,3:17),sep="")],2,mean)))

#split-half

