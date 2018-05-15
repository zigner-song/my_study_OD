#study1 cleaning
#step


data_study1<-read.csv("data_study1.csv",stringsAsFactors = F)

names(data_study1)%in%names(data_study3)
names(data_study1)[names(data_study1)%in%names(data_study3)]
names(data_study1)
demography[!demography %in% names(data_study1)]
summary(data_study1)
source('study1_round2.R', encoding = 'UTF-8')


data_study1<-rbind(data_study1,data_study1_round2)
nrow(data_study1)

data_study1<-subset(data_study1,valid==1,
                    select = c(names(data_study1)[names(data_study1)%in%names(data_study3)],"default"))
data_study1$irreligion<-ifelse(data_study1$religion=="0",0,1)

data_study1$DONATE<-factor(data_study1$DONATE)

data_study1$study<-"1"
data_study1$province<-"北京"
#scale=1,3,4,5,6,13,12,11,10,14,17,9,7

data_study1<-subset(data_study1,T13+T14+T15+T16==0)

data_study1$age<-as.numeric(data_study1$age)

data_study1$default<-factor(data_study1$default,levels=0:2,labels=c("OptOut","Neutral","OptIn"))
data_study1$gender<-factor(data_study1$gender,levels=c(1,0),labels=c("Male","Female"))
data_study1$minority<-factor(data_study1$minority,levels=c(1,0),labels=c("HAN","MINORITY"))
data_study1$DONATE<-factor(data_study1$DONATE)


data_study1<-cbind(data_study1,dummy.code(data_study1$default))

mean(data_study1$age,na.rm=T)
