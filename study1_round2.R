names(data_study1)


if(TRUE){#deal with the data round2
data_study1_round2<-read.csv("data_study1_round2.csv",stringsAsFactors = F)
names(data_study1_round2)
data_study1_round2<-data_study1_round2[c(1,10,12,13,
                                         14:20,22:28,30:32,#knowledge
                                         39:47,48:51,58:61)]
names(data_study1_round2)

data_study1_round2$default<-ifelse(data_study1_round2[2]!=-3,2,#optout
                                   ifelse(data_study1_round2[3]!=-3,0,1))

data_study1_round2$DONATE<-apply(data_study1_round2[2:4],1,max)
data_study1_round2$DONATE<-ifelse(data_study1_round2$DONATE==2,0,1)

#knowledge
names(data_study1_round2)[5:(5+17-1)]<-paste("item",trimws(1:17),sep="")
names(data_study1_round2)[22:38]<-c("gender","age","ethnic","education","school","major","grade","religion","health",
  "T3.10","T3.11","T3.12","T3.13",
  "T13","T14","T15","T16")

#demographic
data_study1_round2$minority<-ifelse(data_study1_round2$ethnic=="汉",1,0)
data_study1_round2$gender<-ifelse(data_study1_round2$gender==2,0,1)
data_study1_round2$religion<-ifelse(data_study1_round2$religion==1,0,4)

#筛去已有经验的被试
data_study1_round2[c("T13","T14","T15","T16")]<-ifelse(data_study1_round2[c("T13","T14","T15","T16")]==2,0,1)


names(data_study1_round2)[1]<-"ID"
data_study1_round2$valid<-1
data_study1_round2$round<-2

data_study1_round2<-data_study1_round2[c(-2,-3,-4)]
data_study1_round2$age

data_study1_round2$ID<-rep(200,nrow(data_study1_round2))+data_study1_round2$ID
nrow(data_study1_round2)


#knowledge
data_study1_round2[paste("item",1:17,sep="")]<-(data_study1_round2[paste("item",1:17,sep="")] %>% t == data_answers[1:17]) %>% ifelse(.,1,0) %>% t

}