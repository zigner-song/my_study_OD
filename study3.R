#study3
names(data_study3)
summaryBy(knowledge    ~ version,data=data_study3,FUN=fct)
summaryBy(N_Organs     ~ version,data=data_study3,FUN=fct,na.rm=T)
summaryBy(DONATE       ~ version,data=data_study3,FUN=fct,na.rm=T)


xtabs(~DONATE+version,data=data_study3)

table(data_study3$DONATE)
sapply(data_study3["knowledge"], FUN=fct)


if(T){
#合并study2中Opt-In基线与study3中的基线
  Var_total<-names(data_study3)[(names(data_study3) %in% names(data_study2))]
  
  ins_data2in3<-subset(data_study2,version=="OptInBaseline",select = Var_total)
  ins_data2in3[names(data_study3)[!(names(data_study3) %in% names(data_study2))]]<-NA
  ins_data2in3$version<-7
  data_study3$DONATE<-factor(data_study3$DONATE)
  data_study3_s2merged<-rbind(ins_data2in3,data_study3)
  

  #data_study3_s2merged<-data_study3
  data_study3_s2merged[paste("version",trimws(1:7),sep="")]<-dummy.code(data_study3_s2merged$version)

  #统一变量类型
  #data_study3_s2merged$version<-factor(data_study3_s2merged$version)
  data_study3_s2merged$DONATE<-factor(data_study3_s2merged$DONATE)
  
  data_study3_s2merged$version<-factor(data_study3_s2merged$version,levels=1:7,labels=versionName_study3)
  
  data_study3_s2merged$sequ<-
    ifelse(data_study3_s2merged$version %in% c("a_Y","a_N","a_s_Y"),1,0)
    data_study3_s2merged$sequ<-factor(data_study3_s2merged$sequ)
  data_study3_s2merged$default3<-
    ifelse(data_study3_s2merged$version %in% c("a_N","d_N"),1,0)
    data_study3_s2merged$default3<-factor(data_study3_s2merged$default3,levels=0:1,labels=c("Y","N"))
  data_study3_s2merged$sepr<-
    ifelse(data_study3_s2merged$version %in% c("a_s_Y","d_s_Y"),1,0)
    data_study3_s2merged$sepr<-factor(data_study3_s2merged$sepr)
  
  data_study3_s2merged$ID<-ifelse(data_study3_s2merged$study==2,data_study3_s2merged$ID+2000,data_study3_s2merged$ID)
#    data_study3_s2merged$ID[data_study3_s2merged$study==2]<-data_study3_s2merged$ID+2000
  
  data_study3_s2merged<-subset(data_study3_s2merged,
                               T13+T14+T15+T16==8)# & !(study==3 & version == "baseline"))
  
}#下面的暂时不用考虑
#---------------------------------------------------------------------
    
#1.描述统计
  table(data_study3_s2merged$version)
  
  options(digits=3)
  xtabs(~gender+version,data=data_study3_s2merged) %>% 
    #prop.table(.,2) %>% 
    chisq.test()
  
  addmargins(xtabs(~province+version,data=data_study3_s2merged),2)
  prop.table(xtabs(~minority+version,data=data_study3_s2merged),2)
  prop.table(xtabs(~irreligion+version,data=data_study3_s2merged),2)
  
  summaryBy(age~version,data=data_study3_s2merged,FUN=fct)
  aov_knlg.v<-aov(age~version,data=data_study3_s2merged)
  summary(aov_knlg.v)

  data_study3_s2merged<-cbind(data_study3_s2merged,dummy.code(data_study3_s2merged$version))
  
#2.Donate*version
  
    table_DONATE.version<-xtabs(~DONATE+version,data=data_study3_s2merged)   #DONATE: 0-不捐 1-捐 2-部分
    table_DONATE.version
    prop.table(table_DONATE.version,2)
    chisq.test(table_DONATE.version[,-7])
    
    data_study3_s2merged$DONATE_combinedPart<-ifelse(data_study3_s2merged$DONATE %in% (1:2),1,0)
    table_DONATE2.version<-xtabs(~DONATE_combinedPart+version,data=data_study3_s2merged)
    prop.table(table_DONATE2.version,2)
    
    chisq.test(table_DONATE2.version)
    chisq.test(xtabs(~DONATE_combinedPart+version,data=data_study3_s2merged))
    
    #ins<-subset(data_study3_s2merged,version %in% c("baseline","a_Y"))
      
    chisq.test(xtabs(~DONATE_combinedPart+version7,
                     data=subset(data_study3_s2merged,version %in% c("baseline","a_Y"))))
    
    
    
    data_study3_s2merged$DONATE_combinedPart<-factor(data_study3_s2merged$DONATE_combinedPart)
    logi.study3<-glm(DONATE_combinedPart~
                       #version+
                       a_Y+a_N+d_Y+d_N+a_s_Y+d_s_Y+
                      # default3/baseline*sequ/baseline+sepr/(default3/baseline)+
                       #baseline+
                       gender+irreligion+age,
                     #contrasts = "contr.helmert",
                     
                     data=data_study3_s2merged#[data_study3_s2merged$version %in% 
                                              #   c("a_Y","d_Y","a_N","d_N","baseline"),]
                                                 #c("a_N","d_N","baseline"),]
                     ,family = binomial(link = "logit"))
    summary(logi.study3)
    coef(logi.study3) %>% exp
    
    #Cox-Snell R^2
    (Cox.Snell.R2_study3<- 1-exp((logi.study3$deviance-logi.study3$null.deviance)/nrow(data_study3_s2merged)))
    #Nagelkerke R^2
    (Nagelkerke.R2_study3<-Cox.Snell.R2_study3/(1-exp((-logi.study3$null.deviance)/nrow(data_study3_s2merged))))
    #print(c(Cox.Snell.R2_study2_OptIn,Nagelkerke.R2_study2_OptIn))
    
    
    
    (OR_study3<-data.frame(exp(confint((logi.study3)))))
    
    
    
#3.N_Organs
  #3.1只看选择部分捐的部分被试
    
    length(data_study3_s2merged$DONATE[data_study3_s2merged$DONATE=="2"])#被试量
    (table_DONATE.version)[3,]  
    chisq.test(prop.table(table_DONATE.version,1)[3,-4]) #不显著
    
    #3.1.1 全部器官
      summaryBy(N_Organs~version,data=data_study3_s2merged,FUN=fct,na.rm=T) 
      summaryBy(N_OrgansH+N_OrgansL~version,
                data=data_study3_s2merged,
                FUN=function(x,...){c(mean=mean(x,...),sd=sd(x,...))},
                na.rm=T)
      
      aov_NO<-aov(N_Organs~version,data=data_study3_s2merged[data_study3_s2merged$version %in% c("a_N","a_Y","d_N","d_Y"),])
      summary(aov_NO)
      TukeyHSD(aov_NO)
      
      ggplot(data_study3_s2merged,aes(x=version,y=N_Organs))+
        geom_boxplot(na.rm=T,fill="grey")+
        geom_jitter(width = 0.2,na.rm=T)
      
      lm_s3<-lm(N_Organs~default3*sequ+
                   gender+irreligion+age,
                 data=data_study3_s2merged[data_study3_s2merged$version %in% c("a_N","a_Y","d_N","d_Y"),])
      lm_s3_contrasts<-lsmeans(lm_s3,~default3*sequ,adjust="Tukey") %>% pairs(.,by="default3") %>% summary
      lm_s3_contrasts$Cohen_d<- abs(lm_s3_contrasts$estimate) / sigmaHat(lm_s3)
      lm_s3_contrasts
      library(lsmeans)
      
      lsmeans(lm_s3)
      
      aov_s3<-aov(N_Organs~default3*sequ
            +gender+irreligion+age,
          data=data_study3_s2merged[data_study3_s2merged$version %in% c("a_N","a_Y","d_N","d_Y"),])
      summary(aov_s3)
      
      TukeyHSD(aov_s3)
      lsmeans(aov_s3,~default3*sequ) %>% pairs
    
      predict(object=lm(N_Organs~default3+sequ,
                 data=data_study3_s2merged[data_study3_s2merged$DONATE=="2" &
                                             data_study3_s2merged$baseline==0&
                                             data_study3_s2merged$sepr==0,]),
              distinct(data_study3_s2merged,default3,sequ),se.fit=T,type="response")
      
    #3.1.2 分两类
      summaryBy(N_OrgansH+N_OrgansL~version,
                data=data_study3_s2merged,FUN=function(x,...){c(mean=mean(x,...),sum=sum(x,...),sd=sd(x,...),n=length(x))},
                na.rm=T) [-c(4,7)]
      
      dfm_N.Organs<-melt(data_study3_s2merged,
                         id=names(data_study3_s2merged)[!names(data_study3_s2merged) %in% c("N_OrgansH","N_OrgansL")],
                         variable.name = "appearance",value.name = "N_OrgansHL")
      
      ggplot(dfm_N.Organs,aes(x=version,y=N_OrgansHL))+geom_boxplot(na.rm=T,aes(color=appearance))#+geom_jitter(width = 0.2,na.rm=T)
      
      aov_NOHL<-aov(N_OrgansHL~version*appearance+Error(ID/(appearance)),data=dfm_N.Organs)
      
      
      dfm_N.Organs$ID<-factor(dfm_N.Organs$ID)
      
      
      summary(aov_NOHL)
      (aov_study3.N_Organs<-aov(N_OrgansHL~sequ+default3+
                                  Error(ID/(appearance))+
                    gender+irreligion+age,
                  data=dfm_N.Organs))
      lmer(N_OrgansHL~sequ*appearance*default3+sepr/default3+sepr+
            (ID|appearance)+
            gender+irreligion+age,
          data=dfm_N.Organs)
      summary(aov_study3.N_Organs)
      
      
      #TukeyHSD(aov_study3.N_Organs)
      #posthocPairwiseT(aov_study3.N_Organs)
      
      #绘图
        #绘图1：
      with(dfm_N.Organs,interaction.plot(appearance,version,N_OrgansHL,
                                         type="b",
                                         pch=c(7,18),
                                         fun=function(x){mean(x, na.rm = TRUE) },
                                         ylab="Organs%"))
      
        #绘图2：
      dfm_N.Organs_summary<-summaryBy(N_OrgansHL~version+sequ+sepr+default3+appearance,data=dfm_N.Organs,na.rm=T)
      dfm_N.Organs_summary$sequColor<-ifelse(dfm_N.Organs_summary$sequ==0,"red","blue")
      
      ggplot(dfm_N.Organs_summary,aes(x=appearance,y=N_OrgansHL.mean,group=version))+
        geom_text(aes(label=ifelse(version=="d_N","降序-拒绝",
                            ifelse(version=="a_N","升序-拒绝",
                            ifelse(version=="a_Y","升序-接受",
                            ifelse(version=="d_Y","降序-接受",
                            ifelse(version=="a_s_Y","升序-接受-强调",
                                   "降序-接受-强调")))))),
                  vjust=-1,size=5)+
        ylab("Organs%")+
        geom_line(size=2,color=dfm_N.Organs_summary$sequColor,
                  linetype=as.numeric(dfm_N.Organs_summary$default3))+
        geom_point(aes(
          shape=ifelse(dfm_N.Organs_summary$sepr==0,"不强调","强调")),size=4)+
        theme_bw()
      
      
      
      ins<-dfm_N.Organs_summary[dfm_N.Organs_summary$version %in% c("a_N","a_Y","d_N","d_Y"),]
      ggplot(ins,
             aes(x=appearance,y=N_OrgansHL.mean,group=version))+
        geom_text(aes(label=ifelse(version=="d_N","降序-拒绝",
                                   ifelse(version=="a_N","升序-拒绝",
                                          ifelse(version=="a_Y","升序-接受","降序-接受"
                                                 )))),
                  vjust=-1,size=5)+
        ylab("Organs%")+
        geom_line(size=2,color=ins$sequColor,linetype=as.numeric(ins$default3)
                  )+
        theme_bw()
      
      
 
      
      

  
      
      
      
      
      
      
      
  #3.2所有被试：选择愿意捐全部的认为是捐献器官比例为1，不捐比例为0
      data_study3_s2merged$N_Organs_t[data_study3_s2merged$DONATE==0]<-0
      data_study3_s2merged$N_Organs_t[data_study3_s2merged$DONATE==1]<-1
      data_study3_s2merged$N_Organs_t[data_study3_s2merged$DONATE==2]<-data_study3_s2merged$N_Organs[data_study3_s2merged$DONATE==2]
      
      data_study3_s2merged$N_OrgansH_t[data_study3_s2merged$DONATE==0]<-0
      data_study3_s2merged$N_OrgansH_t[data_study3_s2merged$DONATE==1]<-1
      data_study3_s2merged$N_OrgansH_t[data_study3_s2merged$DONATE==2]<-data_study3_s2merged$N_OrgansH[data_study3_s2merged$DONATE==2]
      
      data_study3_s2merged$N_OrgansL_t[data_study3_s2merged$DONATE==0]<-0
      data_study3_s2merged$N_OrgansL_t[data_study3_s2merged$DONATE==1]<-1
      data_study3_s2merged$N_OrgansL_t[data_study3_s2merged$DONATE==2]<-data_study3_s2merged$N_OrgansL[data_study3_s2merged$DONATE==2]
      
      
    #3.2.1全部器官
      summaryBy(N_Organs_t~version,data=data_study3_s2merged,FUN=fct,na.rm=T)
      
      aov_NO_t<-aov(N_Organs_t~version,data=data_study3_s2merged)
      summary(aov_NO_t)
      TukeyHSD(aov_NO_t)
      
      ggplot(data_study3_s2merged,aes(x=version,y=N_Organs_t))+geom_boxplot(na.rm=T,fill="grey")+geom_jitter(width = 0.2,na.rm=T)
      
      summary(lm(N_Organs~version+knowledge,data=data_study3_s2merged))
      
    
    #3.2.3分两类
      summaryBy(N_OrgansH_t+N_OrgansL_t~version,data=data_study3_s2merged,FUN=fct,na.rm=T) [-c(4,7)]
      dfm_N.Organs_t<-melt(data_study3_s2merged,
                         id=names(data_study3_s2merged)[!names(data_study3_s2merged) %in% c("N_OrgansH_t","N_OrgansL_t")],
                         variable.name = "appearance",value.name = "N_OrgansHL_t")
      
      ggplot(dfm_N.Organs_t,aes(x=version,y=N_OrgansHL_t))+geom_boxplot(na.rm=T,aes(color=appearance))+geom_jitter(width = 0.2,na.rm=T)
      
      aov_NOHL_t<-aov(N_OrgansHL_t~version+appearance+Error(ID/(appearance)),data=dfm_N.Organs_t)
      summary(aov_NOHL_t)
      if(F){
      with(dfm_N.Organs_t,interaction.plot(appearance,version,N_OrgansHL_t,
                                         type="b",col=rainbow(7),pch=1:7,
                                         fun=function(x){mean(x, na.rm = TRUE)},
                                         ylab="N_OrgansHL_t"))
      }
      #baseline再收一下
      #study1-3 大纲：需要报告什么，结果怎么样

#尝试对捐献划分为3个水平：全部、部分不捐      
    library(nnet)
    data_study3_s2merged$DONATE<-relevel(data_study3_s2merged$DONATE,ref="0",order=T)
    Donate.model_study3<-multinom(DONATE~ #. -scale,
                                    #version+
                                    a_Y+a_N+d_Y+d_N+a_s_Y+d_s_Y+
                                    gender+irreligion+age,
                                  data=data_study3_s2merged[data_study3_s2merged$baseline==0,],
                                  maxit=1000)
    summary(Donate.model_study3)
    z <- summary(Donate.model_study3)$coefficients/summary(Donate.model_study3)$standard.errors
    (p <- (1 - pnorm(abs(z), 0, 1)) * 2)
    
    require(MASS)
    Donate.model_study3<-polr(DONATE~ #. -scale,
            #version+
            a_Y+a_N+d_Y+d_N+#a_s_Y+d_s_Y+
            #default3+sequ+
            gender+irreligion+age,
          data=data_study3_s2merged[data_study3_s2merged$baseline==0&data_study3_s2merged$sepr==0,],Hess=T)
    summary(Donate.model_study3)
    coef(summary(Donate.model_study3))[, "t value"] %>% abs %>% pnorm(.,lower.tail=F)*2    # p-value
    
    
    #calculate the se
    ins.matrix<-data.frame(diag(3));names(ins.matrix)=c("a_Y","a_N","d_Y")
    polr(DONATE~a_Y+a_N+d_Y+d_N,
         data=data_study3_s2merged[data_study3_s2merged$baseline==0&
                                     data_study3_s2merged$sepr==0,],Hess=T) %>%
    predict(.,
            ins.matrix,se.fit=T,type="response")
    
    
    
    predict(Donate.model_study3,data=data_study3_s2merged[data_study3_s2merged$baseline==0,],type="p")
    
    
    table(predicted=predict(Donate.model_study3,data_study3_s2merged),
          actual=data_study3_s2merged$DONATE)
    
    
    mean(predict(Donate.model_study3,data_study3_s2merged) == data_study3_s2merged$DONATE_combinedPart)
    
    
    table(predicted=predict(logi.study3,data_study3_s2merged),
           actual=data_study3_s2merged$DONATE_combinedPart)
    
    
    library(mlogit)
    data_DONATE_study3<-mlogit.data(subset(data_study3_s2merged,
                                           select=c("ID","DONATE",
                                                    "default3","gender","irreligion")),
                                    shape="wide",choice = "DONATE")
    mlogit(DONATE ~ default3+
                     gender+irreligion, data = data_DONATE_study3)
    