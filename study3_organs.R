#study3 analysis2
# 

names(data_study3_s2merged)
data_organ.z
data_organ$organ<-rownames(data_organ); rownames(data_organ)<-1:nrow(data_organ)



############
#cluster
############

library(cluster)
data_organ.clu<-agnes(data_organ.z,stand=TRUE,method="ward")
plot(data_organ.clu)

data_organ$clu.h<-cutree(data_organ.clu,k=2)




#kmeans

kmeans_organ<-kmeans(data_organ.z,3)
kmeans_organ$betweenss / kmeans_organ$totss
kmeans_organ$centers


data_organ$clu.k<-kmeans_organ$cluster

kmeans_organ.appearance<-kmeans(data_organ.z[1],2)
(data_organ$cluster.appearance<-kmeans_organ.appearance$cluster)  ##分为6-10

?kmeans


for(i in 2:4){
  clu_k<-kmeans(data_organ.z,i)
  print(paste("i=",i))
  print(paste("SSB=",clu_k$betweenss))
  print(paste("SSW=",clu_k$tot.withinss))
  print(paste("SST=",clu_k$totss))
  print(paste("%=",clu_k$betweenss/clu_k$tot.withinss))
  print("")
}


for(i in 2:8){
  clu_k<-kmeans(data_organ.z,i)
  SSB<-sum((clu_k$centers-mean(clu_k$centers))^2)
  print(paste("i=",i))
  print(paste("SSB=",SSB))
  print(paste("SSW=",clu_k$tot.withinss))
  print(paste("SST=",clu_k$totss))
  print(paste("%=",SSB/clu_k$tot.withinss))
  print("")
}


















#cluster: treat all participants

data_appearance.t<-data.frame(t(data_appearance)[-1,])
scale(data_appearance.t)
names(data_appearance.t)
data_organ.clu.t<-agnes(data_appearance.t,stand=TRUE,method="ward")
plot(data_organ.clu.t)

cutree(data_organ.clu.t,k=2)

kmeans(data_appearance.t,2)$size


###################k-means可分为5-11；层次聚类可分为3-13


kmeans_organ.appearance.necessity<-kmeans(data_organ.z[-2],3)
data_organ$cluster.appearance.necessity<-kmeans_organ.appearance.necessity$cluster

kmeans_organ.necessity<-kmeans(data_organ.z[3],3)
data_organ$cluster.necessity<-kmeans_organ.necessity$cluster
order(data_organ$necessity.mean)


data_organ[c(3,7)]
data_organ$rank<-16:1
data_organ$organ<-rownames(data_organ)
rownames(data_organ)<-NULL


#######
# ignore the cluster
#######

data_study3_Organs.Analysis<-subset(data_study3_s2merged,
                                    data_study3_s2merged$version %in% c("a_Y","a_N","d_Y","d_N"),
                                    select=c("ID","version",
                                             demography,organs,
                                             "default3","sequ",
                                             "DONATE","N_Organs"))

#distribution

xtabs(~version+gender,
      data=data_study3_Organs.Analysis[data_study3_Organs.Analysis$DONATE=="2" &
                                         data_study3_Organs.Analysis$version %in% c("a_Y","a_N","d_Y","d_N"),])[1:4,]  %>% chisq.test()

options(digits=4)
summaryBy(age~version,FUN=function(x){c(mean(x),sd(x))},
          data=data_study3_Organs.Analysis[data_study3_Organs.Analysis$DONATE=="2",])

xtabs(~version+irreligion,
      data=data_study3_Organs.Analysis[data_study3_Organs.Analysis$DONATE=="2"&
                                       data_study3_Organs.Analysis$version %in% c("a_Y","a_N","d_Y","d_N"),])[1:4,] %>% 
  #prop.table(.,1) %>%
  chisq.test()

table(data_study3_Organs.Analysis[data_study3_Organs.Analysis$DONATE=="2",]$version)






##analyse the amount of organs
table(data_study3_Organs.Analysis[data_study3_Organs.Analysis$DONATE=="2",]$gender)
mean(data_study3_Organs.Analysis[data_study3_Organs.Analysis$DONATE=="2",]$age)
  sd(data_study3_Organs.Analysis[data_study3_Organs.Analysis$DONATE=="2",]$age)
  data_study3_Organs.Analysis<-data_study3_Organs.Analysis[data_study3_Organs.Analysis$DONATE=="2",]

aov.N_organs_study3<-aov(N_Organs~default3*sequ+age+gender+irreligion,
                         data=data_study3_Organs.Analysis)
summary(aov.N_organs_study3)
lsmeans(aov.N_organs_study3,~sequ*default3) %>% pairs(.,by="default3")

require(lsr)
lsr::etaSquared(aov.N_organs_study3)


  ###graph
  predict(aov(N_Organs~default3*sequ+age+as.numeric(gender)+as.numeric(irreligion),
              data=data_study3_Organs.Analysis[data_study3_Organs.Analysis$DONATE=="2",]),
          data.frame(sequ=factor(c(1,1,0,0)),
                     default3=c("N","Y","N","Y"),
                     age=rep(mean(data_study3_Organs.Analysis[data_study3_Organs.Analysis$DONATE=="2",]$age),4),
                     gender=rep(1.5,4),
                     irreligion=rep(1.5,4)),
          se.fit=T)
  



# 1-all 2-part 3-none
data_study3_Organs.Analysis$DONATE %>% table


dfm_study3_Organs<-melt(data_study3_Organs.Analysis[data_study3_Organs.Analysis$DONATE=="2",],
                        id=c("ID","version",
                             demography,
                             "default3","sequ",
                             "DONATE","N_Organs"),
                        variable.name = "organ",
                        value.name = "DONATE.ORGAN")
names(dfm_study3_Organs);head(dfm_study3_Organs[dfm_study3_Organs$DONATE=="2",])

names(data_organ)

data_organ$organ<-toupper(data_organ$organ)

str(dfm_study3_Organs)

dfm_study3_Organs<-merge(dfm_study3_Organs,
                         data_organ,by="organ")

names(dfm_study3_Organs)
names(dfm_study3_Organs)[which(names(dfm_study3_Organs)=="appearance.mean")]<-"appearance"

#dfm_study3_Organs$appearance.sequ<-dfm_study3_Organs$appearance*as.numeric(dfm_study3_Organs$sequ)
dfm_study3_Organs$position<-ifelse(dfm_study3_Organs$sequ==0,
                                   dfm_study3_Organs$rank,
                                   17-dfm_study3_Organs$rank)

dfm_study3_Organs$appearance<-scale(dfm_study3_Organs$appearance)













#--------------------------------------------------fengexian
logi.study3.ins<-glm(DONATE.ORGAN~default3*position+#appearance+
                         
                         #position:appearance+
                         #default3:appearance+
                         #position:appearance:default3+
                           # (position|ID)+(appearance|ID)+
                            age+gender+irreligion,
                          data=dfm_study3_Organs,
                          family = binomial(link = "logit"),
                          control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(logi.study3.ins)



ggplot(dfm_study3_Organs,aes(x=appearance.code,y=DONATE.ORGAN,group=version))+
  binomial_smooth(aes(color=appearance,
                      linetype=default3
                      ))+
  #scale_colour_gradient2()+
  #scale_colour_gradient(low = "white", high = "black")+
  scale_colour_manual(values=c("blue","red"))+
  scale_linetype_manual(values=c(3,1))+
  ylab("器官捐献率")+xlab("position")+
  theme(panel.border  = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(colour = "black",size=0.5),
        text=element_text(size=18))








#--------------------------------------------------fengexian


















if(F){



logi.study3.organs<-glmer(DONATE.ORGAN~default3+sequ+appearance+
                                        sequ:appearance+default3:sequ+
                            (appearance|ID)+
                          age+gender+irreligion,
                          data=dfm_study3_Organs,
                          family = binomial(link = "logit"),
                          control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))

M2<-glmer(DONATE.ORGAN~default3+sequ+appearance+
            sequ:appearance+default3:sequ+
            (appearance|ID),
          data=dfm_study3_Organs,
          family = binomial(link = "logit"),
          control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
anova(logi.study3.organs,
      M2)
summary(logi.study3.organs)

confint_logi.study3.organs<-confint(logi.study3.organs) 
#%>% head(1) %>% t
confint_logi.study3.organs
coef_logi.study3.organs<-(coef(logi.study3.organs)$ID%>% head(1) %>% t)

logi.study3.organs_decsending<-glmer(DONATE.ORGAN~default3+appearance+
        (appearance|ID)+
        age+gender+irreligion,
      data=dfm_study3_Organs[dfm_study3_Organs$sequ==0,],
      family = binomial(link = "logit"),
      control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
  summary(logi.study3.organs_decsending)
  exp(coef(logi.study3.organs_decsending)$ID%>% head(1) )%>% t

  logi.study3.organs_acsending<-glmer(DONATE.ORGAN~default3+appearance+
                                         (appearance|ID)+
                                         age+gender+irreligion,
                                       data=dfm_study3_Organs[dfm_study3_Organs$sequ==1,],
                                       family = binomial(link = "logit"),
                                       control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
  summary(logi.study3.organs_acsending)
  exp(coef(logi.study3.organs_acsending)$ID%>% head(1) )%>% t
  
  
  
  glmer(DONATE.ORGAN~default3*appearance+
          (appearance|ID),
        data=dfm_study3_Organs[dfm_study3_Organs$sequ==0,],
        family = binomial(link = "logit"),
        control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000))) %>% summary
  
  
#??? graph
data_summary_study3.organs<-summaryBy(DONATE.ORGAN~default3+sequ+appearance+version,data=dfm_study3_Organs)

dfm_study3_Organs$sequ<-factor(dfm_study3_Organs$sequ,levels=0:1,labels=c("降序","升序"))
dfm_study3_Organs$default3<-factor(dfm_study3_Organs$default3,levels=c("Y","N"),
                               labels=c("接受反应模式","拒绝反应模式"))
dfm_study3_Organs<-subset(dfm_study3_Organs,
                          select=c("organ","ID",
                                   "version","default3","sequ","appearance","DONATE.ORGAN"))


names(dfm_study3_Organs)[which(names(dfm_study3_Organs)=="default3")]<-"反应模式"
names(dfm_study3_Organs)[which(names(dfm_study3_Organs)=="sequ")]<-"排列顺序"




names(dfm_study3_Organs)





binomial_smooth <- function(...) {
       geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
   }

p_organs_total<-ggplot(dfm_study3_Organs,aes(x=appearance,y=DONATE.ORGAN,group=version))+
  binomial_smooth(aes(color=反应模式,
                      linetype=排列顺序#重点结果用实心突出一下
                      ))+
  scale_colour_manual(values=c("blue","red"))+
  scale_linetype_manual(values=c(3,1))+
  ylab("器官捐献率")+xlab("对外观的影响程度")+
  theme(panel.border  = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(colour = "black",size=0.5),
        text=element_text(size=18))

p_organs_total+labs(title="图2：不同反应模式和呈现顺序条件下不同对外观
                    影响程度的器官捐献比率统计图")

}



###########################################################

head(dfm_study3_Organs)

organsL<-organs[1:11]
organsH<-organs[12:16]

str(dfm_study3_Organs)


dfm_study3_Organs$position.code<-ifelse(dfm_study3_Organs$position<=5,1,
                                        ifelse(dfm_study3_Organs$position>=11,3,2))
dfm_study3_Organs$position.code<-factor(dfm_study3_Organs$position.code)

dfm_study3_Organs$appearance.code<-ifelse(dfm_study3_Organs$organ %in% organsH,"H","L")
dfm_study3_Organs$appearance.code<-factor(dfm_study3_Organs$appearance.code)

dfm_study3_Organs$nsequ<-as.numeric(dfm_study3_Organs$sequ)
dfm_study3_Organs$ndefault<-as.numeric(dfm_study3_Organs$default3)


str(dfm_study3_Organs)
#
#m1 : full model
require(lme4)
m1<-glmer(DONATE.ORGAN~default3*sequ*appearance.code+
        age+gender+irreligion+
          (default3|organ)+(sequ|organ)+(1|organ:ID)+
          (appearance.code|ID)+(appearance.code:sequ|ID),
      data=dfm_study3_Organs,
      family = binomial(link = "logit"))
summary(m1)
m1
require(lsmeans)

m1.lsmeans<-lsmeans(m1,~sequ*appearance.code|default3,
        method="binominal",adjust="Tukey")
pairs(m1.lsmeans,by=c("default3","appearance.code"))
pairs(m1.lsmeans)
contrast(m1.lsmeans)


#%>% pairs(.,by=c("default3","appearance.code"))



m1.1<-glmer(DONATE.ORGAN~default3*sequ*appearance.code+
              age+gender+irreligion+
              
              (appearance.code|ID),
            data=dfm_study3_Organs,
            family = binomial(link = "logit"))
summary(m1.1) #############################################
exp((coef(m1.1)$ID)[1,])%>%t

  library("lsmeans")
  
  m1.1.lsmeans<-lsmeans(m1.1,~sequ*appearance.code*default3,
                        method="binominal",adjust="Tukey")
  pairs(m1.1.lsmeans,by="default3")
  
  xtabs(~DONATE.ORGAN+sequ,data=dfm_study3_Organs) %>% prop.table(.,1)
  
  contrast(m1.1.lsmeans, by = "default3")
  test(m1.1.lsmeans,by = "default3")
  contrast(m1.1.lsmeans,interaction = c("poly", "consec"))
  
  lsmeans(m1.1,~sequ*appearance.code|default3, adjust="tukey",method="binominal") %>% pairs()
  
  
  #note here
  m1.1_contrasts<-lsmeans(m1.1,pairwise~sequ*appearance.code*default3, adjust="tukey") %>% 
    pairs(.,by=c("default3","appearance.code")) %>% summary
  sigmaHat(m1.1)
  
  m1.1_contrasts$Cohns_d <- abs(m1.1_contrasts$estimate) / sigmaHat(m1.1)
  m1.1_contrasts
  
  
  anova(m1.1)
  
  lsmeans(m1,1,list())
  

  
  
  
  
  
  
  
  
  #manova
m1.2<-glmer(DONATE.ORGAN~default3*sequ*appearance.code+
              age+gender+irreligion+
              
            (1|organ)+(1|ID)+(1|organ:ID),
            data=dfm_study3_Organs,
            family = binomial(link = "logit"))
summary(m1.2)

 lsmeans(m1.2,~default3*sequ*appearance.code, adjust="tukey",method="binominal") %>% 
   pairs(.,by=c("default3","appearance.code"))






  #caculate the se
  ins<-dfm_study3_Organs;
  distinct(subset(ins,select=c("appearance.code","version")))
  
  
  
  ins1<-distinct(subset(ins,select=c("appearance.code","default3","sequ"))) %>%
    predict(glm(DONATE.ORGAN~default3*sequ*appearance.code,
          data=ins,
          family = binomial(link = "logit")),
          .,
          se.fit=T,type="response")
  
  ins2<-cbind(distinct(subset(ins,select=c("appearance.code","default3","sequ"))),ins1$fit,ins1$se.fit)
  (ins3<-dcast(appearance.code~default3+sequ,data=ins2,value.var = "ins1$fit"))


#m2 : only for the N mode
m2<-glmer(DONATE.ORGAN~sequ*appearance.code+
            age+gender+irreligion+
            (appearance.code|ID)+(sequ|organ)+(1|ID:organ),
          data=dfm_study3_Organs[dfm_study3_Organs$default3=="Y",],
          family = binomial(link = "logit")
          )
summary(m2)
m2

lsmeans(m2,~sequ*appearance.code, adjust="tukey") %>% pairs(.,by="appearance.code")


m2N<-glmer(DONATE.ORGAN~sequ*appearance.code+
            age+gender+irreligion+
            (appearance.code|ID)+(sequ|organ)+(1|ID:organ),
          data=dfm_study3_Organs[dfm_study3_Organs$default3=="N",],
          family = binomial(link = "logit")
)
summary(m2N)
m2

lsmeans(m2N,~sequ*appearance.code, adjust="tukey",method="binominal") %>% pairs(.,by="appearance.code")









m2.1<-glmer(DONATE.ORGAN~sequ*default3+
            age+gender+irreligion+
            (1|ID),
          data=dfm_study3_Organs[dfm_study3_Organs$appearance.code=="L",],
          family = binomial(link = "logit")
)
summary(m2.1)


lsmeans(m2.1,~sequ*default3, adjust="tukey") %>% pairs(.,by="default3")



m3<-glmer(DONATE.ORGAN~default3*sequ+
            age+gender+irreligion+
            (default3|organ)+(sequ|organ)+(1|organ:ID),
          data=dfm_study3_Organs[dfm_study3_Organs$organ %in% organsL,],
          family = binomial(link = "logit"))
summary(m3)
m3


m1.1_beta<-glmer(DONATE.ORGAN~default3*sequ*appearance.code+
                   age+gender+irreligion+
                   (1|ID)+(sequ|organ),
                 data=dfm_study3_Organs,
                 family = binomial(link = "logit"))
summary(m1.1_beta)
lsmeans(m1.1_beta,pairwise~sequ*appearance.code|default3) %>% pairs(.,by=c("default3","appearance.code"))

m0<-glmer(DONATE.ORGAN~default3*sequ*appearance.code+
            age+gender+irreligion+
            (appearance.code|ID)+(default3*sequ|organ)+(1|ID:organ),
          data=dfm_study3_Organs,
          family = binomial(link = "logit"),REML = F)
summary(m0)

lsmeans(m0,pairwise~sequ*appearance.code|default3) %>% pairs(.,by=c("default3","appearance.code"))

ins.logi<-glm(DONATE.ORGAN~default3*sequ*appearance.code+
        age+gender+irreligion,
      data=dfm_study3_Organs,
      family = binomial(link = "logit"))
summary(ins.logi)
lsmeans(ins.logi,pairwise~default3*sequ*appearance.code,adjust="tukey") %>% pairs(.,by=c("default3"))
rm(ins.logi)


##########################
#   treat as 2 groups    #
##########################

str(dfm_study3_Organs)

dfm_study3_Organs.by.group<-merge(
      distinct(dfm_study3_Organs[c("ID",demography,"appearance.code","default3","sequ")]),
      summaryBy(DONATE.ORGAN~ID+appearance.code,data=dfm_study3_Organs),
      by=c("ID","appearance.code"))

m.bg.1<-lmer(logit(DONATE.ORGAN.mean)~default3*sequ*appearance.code+(appearance.code|ID),
             data=dfm_study3_Organs.by.group,
             control=lmerControl(check.nobs.vs.nRE="ignore"))

summary(m.bg.1)

lsmeans(m.bg.1,pairwise~default3*sequ*appearance.code,adjust="tukey") %>% pairs(.,by="default3")
