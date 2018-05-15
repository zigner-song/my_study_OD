#study2
#
summaryBy(knowledge    ~ version,data=data_study2,FUN=fct)
summaryBy(DONATE       ~ version,data=data_study2,FUN=fct)

summaryBy(DONATE_Total ~ version,data=data_study2,
          FUN=function(x){c(sum=sum(x),n=length(x),percentage=sum(x)/length(x))} )
fct(data_study2$age)

ins<-subset(data_study2,(T13+T14+T15+T16==8))
summaryBy(DONATE_Total ~ version,data=ins,
          FUN=function(x){c(sum=sum(x),n=length(x),percentage=sum(x)/length(x))} )
summaryBy(DONATE_Total ~ version,data=ins,
          FUN=function(x){c(percentage=sum(x)/length(x))})[-1] %>% chisq.test()

xtabs(~DONATE_Total+version,data=ins) %>% chisq.test()


#ins$DONATE_Total<-factor(ins$DONATE_Total,levels=0:1,labels=c("不捐","捐"))
summaryBy(age ~ version,data=data_study2,
          FUN=fct)
table(ins$gender,ins$version) %>% chisq.test()
#%>% prop.table(.,2)

aov(age~version,data=data_study2) %>% TukeyHSD()

write.csv(ins,"ins.csv")



#1. 比较Opt-In各组差异
  data_study2.OptIn<-subset(data_study2,default=="OptIn" & (T13+T14+T15+T16==8) )
  data_study2.OptIn<-subset(data_study2.OptIn,
                            select=c("ID","study","version","default","DONATE","DONATE_2ndChoice","DONATE_Total",
                                     scale,"knowledge",demography,experience,
                                     Texas,organs_study2,NewYork,"Welsh_Reason"))
  data_study2.OptIn<-cbind(data_study2.OptIn,dummy.code(data_study2.OptIn$version))
  
  #1.1 describe the distribution
    # knowledge
  summaryBy(knowledge    ~ version,data=data_study2.OptIn,FUN=fct)
  summaryBy(DONATE       ~ version,data=data_study2.OptIn,FUN=fct)
  summaryBy(DONATE_Total ~ version,data=data_study2.OptIn,FUN=fct)
      
  data_study2$DONATE_Total<-factor(data_study2$DONATE_Total)
  data_study2$DONATE<-factor(data_study2$DONATE)
  
  xtabs(~DONATE_Total+ version,
        data=data_study2.OptIn)[,2:3] %>% 
    chisq.test()
  
  #1.2  推断统计
    logi.study2_OptIn<-glm(DONATE_Total~Texas+NewYork+JapanY+OptInBaseline+
                             gender+irreligion+age,
                           data=data_study2.OptIn,family = binomial(link = "logit"))
    summary(logi.study2_OptIn)
    exp(coef(logi.study2_OptIn)) %>% t %>% t
    
    library(rms)
    lrm(DONATE_Total~Texas+NewYork+JapanY+OptInBaseline+
          gender+irreligion+age,
        data=data_study2.OptIn)
    
      #Cox-Snell R^2
    (Cox.Snell.R2_study2_OptIn<- 1-exp((logi.study2_OptIn$deviance-logi.study2_OptIn$null.deviance)/nrow(data_study2.OptIn)))
    #Nagelkerke R^2
    (Nagelkerke.R2_study2_OptIn<-Cox.Snell.R2_study2_OptIn/(1-exp((-logi.study2_OptIn$null.deviance)/nrow(data_study2.OptIn))))
    #print(c(Cox.Snell.R2_study2_OptIn,Nagelkerke.R2_study2_OptIn))
    
    anova(logi.study2_OptIn)
    regTermTest(logi.study2_OptIn,"Texas")
    
    (OR_study2_OptIn<-data.frame(exp(confint((logi.study2_OptIn)))))
    #OR_study2_OptIn<-cbind(logi.study2_OptIn,data.frame(exp(coef(logi.study2_OptIn))))
    names(OR_study2_OptIn)<-c("2.5%","97.5%","OR")
    
  #1.3日本 器官数量
    data_study2.Japan<-subset(data_study2.OptIn,version %in% c("JapanY","JapanN"),
                              select=c("ID","study","version","default",
                                       "DONATE","DONATE_2ndChoice","DONATE_Total",organs_study2,
                                       "knowledge",demography,experience))
    #计算数量
    data_study2.Japan$N_Organs<-apply(data_study2.Japan[organs_study2],1,sum)
    summary_NOrgans.study2<-summaryBy(N_Organs~version,data=data_study2.Japan,FUN=fct)
    t_NOrgans.study2<-t.test(N_Organs~version,data=data_study2.Japan)
    cohen.d(N_Organs~version,data=data_study2.Japan)
    
    lm_NOrgans.study2<-lm(N_Organs~version+gender+irreligion,data=data_study2.Japan)
    summary(lm_NOrgans.study2)
    summaryBy(knowledge~version,data=data_study2.Japan,FUN=fct)
    lm_NOrgans.study2<-lm(N_Organs~version+knowledge,data=data_study2.Japan)
  
    p_NOrgans.study2<-ggplot(summary_NOrgans.study2,aes(x=version,y=N_Organs.mean))+geom_bar(stat="identity")
    
    #calculate the se
    glm(N_Organs/8~version,data=data_study2.Japan) %>%
      predict(.,data.frame(version=c("JapanY","JapanN")),
              se.fit=T,type="response")
    
    #write.csv(data_study2.Japan,"data_study2.Japan.csv")
    
#2.Opt-Out组
    data_study2.OptOut<-subset(data_study2,default=="OptOut" & (T13+T14+T15+T16==8),
                               select=c("ID","study","version","default","DONATE","DONATE_2ndChoice","DONATE_Total",
                                       scale,"knowledge",demography,experience,
                                       "Welsh_Reason"))
    data_study2.OptOut<-cbind(data_study2.OptOut,dummy.code(data_study2.OptOut$version))
    
    summaryBy(knowledge    ~ version,data=data_study2.OptOut,FUN=fct)
    summaryBy(DONATE       ~ version,data=data_study2.OptOut,FUN=fct)
    summaryBy(DONATE_Total ~ version,data=data_study2.OptOut,FUN=fct)
    xtabs(~DONATE_Total+ version,
          data=data_study2.OptOut)[,c(6,7,8)] %>% 
              chisq.test()
    
    
    logi.study2_OptOut<-glm(DONATE_Total~Cyprus+Welsh+
                             gender+irreligion+age,
                            data=data_study2.OptOut,family = binomial(link = "logit"))
    summary(logi.study2_OptOut)     #基本都不显著
        


#3. Opt-Out vs. Opt-In
    #全部，直接比
    
    
    data_study2$version<-relevel(data_study2$version,ref="JapanN")
    logi.study2_In.V.Out<-glm(DONATE_Total~default*version/default+
                              gender+irreligion+age,
                            data=data_study2,
                            family = binomial(link = "logit"))
    ins<-glm(DONATE_Total~version+
          gender+irreligion+age,
        data=data_study2,
        family = binomial(link = "logit"))
    
   1/( exp(coef(ins))%>%t%>%t)
    
   #Cox-Snell R^2
   (1-exp((ins$deviance-ins$null.deviance)/nrow(data_study2)))
   #Nagelkerke R^2
   ((1-exp((ins$deviance-ins$null.deviance)/nrow(data_study2)))/(1-exp((-ins$null.deviance)/nrow(data_study2))))
   
    
    
    summary(logi.study2_In.V.Out)
    exp(coef(logi.study2_In.V.Out))%>%t%>%t
    
    contrasts(data_study2$version)
    
    wald.test(b = coef(logi.study2_In.V.Out), Sigma = vcov(logi.study2_In.V.Out), Terms = 2)
    
    #Cox-Snell R^2
    (Cox.Snell.R2_study2_In.V.Out<- 1-exp((logi.study2_In.V.Out$deviance-logi.study2_In.V.Out$null.deviance)/nrow(data_study2)))
    #Nagelkerke R^2
    (Nagelkerke.R2_study2_In.V.Out<-Cox.Snell.R2_study2_In.V.Out/(1-exp((-logi.study2_In.V.Out$null.deviance)/nrow(data_study2))))
    
    
    #ins
    data_study2<-cbind(data_study2,dummy.code(data_study2$version))
    
    logi.
    glm(DONATE_Total~OptInBaseline+JapanY+Texas+NewYork+OptOutBaseline+Cyprus+Welsh+
          age+gender+irreligion,
        data = data_study2,family = binomial(link = "logit"))
    
    library(sjstats)
    
    ?R2
    
    
    
    (OR_study2_In.V.Out<-data.frame(exp(confint((logi.study2_In.V.Out)))))
    
    
    #3.1 graph
    
    ggbarplot(data=data_study2,x="version",y="DONATE")
    
    
    
    #只比baseline
    
    logi.study2_In.V.Out_baseline<-glm(DONATE_Total~version+
          gender+irreligion,
        data=data_study2[data_study2$version %in% c("OptInBaseline","OptOutBaseline"),],
        family = binomial(link = "logit"))
    summary(logi.study2_In.V.Out_baseline)
        #不显著
    
    
    
    
    
#decision tree
    
  library(tree)
  
    tree.study2<-tree(DONATE~default+gender+irreligion+health+minority+knowledge,data=data_study2,mincut=0)
    tree.study2
    plot(tree.study2);text(tree.study2,digits=3)
    #tree.study2<-prune.tree(tree.study2,best=2)
    
    predict(tree.study2,data_study2,type="class")
    
    
#calculate the se
    dummy_code_levels<-rbind(diag(length(levels(data_study2$version))-1),rep(0,length(levels(data_study2$version))-1))
    dummy_code_levels<-data.frame(dummy_code_levels)
    names(dummy_code_levels)<-levels(data_study2$version)[1:7]
    glm(DONATE_Total~OptInBaseline+JapanY+JapanN+Texas+NewYork+OptOutBaseline+Cyprus,
        data=data_study2,family = binomial(link = "logit")) %>%
      predict(.,dummy_code_levels,
              se.fit=T,type="response")
    rm(dummy_code_levels)
    
mean(data_study2$knowledge)
    
    