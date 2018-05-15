#step2
data_study2<-read.csv("data_study2.csv",stringsAsFactors = F)
data_study2<-subset(data_study2,data_study2$valid1==T & data_study2$time>140)
data_study2$study<-2

data_study2$study<-factor(data_study2$study)
data_study2$version<-factor(data_study2$version)



#表头
demography
scale
experience

Texas<-paste("Texas_step",trimws(1:5),sep="")
organs_study2<-c("kidney","liver","heart","lung","pancreas","small.intestine","pancreas.islet","cornea")
NewYork<-c("Opt.In_NewYork","Opt.In_NewYork2")

  
data_study2<-subset(data_study2,
                    select=c("ID","study","version","default","DONATE","DONATE_2ndChoice","DONATE_Total",
                             scale,demography,experience,
                             Texas,organs_study2,NewYork,"Welsh_Reason"))
data_study2$version<-factor(data_study2$version,levels=c(1,2,3,4,5,6,7,8),
                            labels=c("OptInBaseline","JapanY","JapanN","Texas","NewYork",
                                     "OptOutBaseline","Cyprus","Welsh"))
data_study2$gender<-factor(data_study2$gender,levels=c(1,2),labels=c("Male","Female"))
data_study2$minority<-factor(data_study2$minority,levels=c(1,0),labels=c("HAN","MINORITY"))

data_study2$default<-factor(data_study2$default,levels=c(0,1),labels=c("OptOut","OptIn"))
#data_study2$DONATE<-factor(data_study2$DONATE)

demography %in% names(data_study2)
names(data_study2) %in% names(data_study3)
names(data_study3)
names(data_study2)
data.frame(names(data_study2) %in% names(data_study3),names(data_study2))


