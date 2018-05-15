

tabel_study1<-xtabs(~default+DONATE,data=data_study1)
addmargins(tabel_study1,2)
prop.table(tabel_study1,1) 

chisq.test(tabel_study1)

#s1
nrow(data_study1)
table(data_study1$ID>200)
table(data_study1$gender)
sd(data_study1$age)


#bygroup

summaryBy(age~default,FUN=function(x){c(mean(x),sd(x))},data=data_study1)
xtabs(~default+irreligion,data=data_study1) %>% prop.table(1)
xtabs(~default+gender,data=data_study1) %>% prop.table(1)
xtabs(~default+DONATE,data=data_study1) %>% prop.table(1)




xtabs(~default+irreligion,data=data_study1) %>% chisq.test()#prop.table(.,1)

logi.study1<-glm(DONATE~Neutral+
                        OptIn+
                         gender+irreligion+age,#+knowledge,
                          data=data_study1
                 ,family = binomial(link = "logit"))

summary(logi.study1)

(1-exp((logi.study1$deviance-logi.study1$null.deviance)/nrow(data_study1)))
(1-exp((logi.study1$deviance-logi.study1$null.deviance)/nrow(data_study1)))/(1-exp((-logi.study1$null.deviance)/nrow(data_study1)))

exp(coef(logi.study1))



testdata_study1<-distinct(data_study1,gender,irreligion,age,Neutral,OptOut)
(testdata_study1<-summaryBy(age+as.numeric(gender)+as.numeric(irreligion)~Neutral+OptOut,data=testdata_study1,
                           FUN=function(x,...){mean(x,na.rm=T,...)}))
names(testdata_study1)[3:5]<-c("age","gender","irreligion")

#caculate the se 
predict(glm(DONATE~Neutral+OptOut,data=data_study1,family = binomial(link = "logit")),
        data.frame(Neutral=c(1,0,0),OptOut=c(0,1,0)),
        se.fit=T,type="response")[c("fit","se.fit")] %>% cbind(data.frame(Neutral=c(1,0,0),OptOut=c(0,1,0)),.)

predict(logi.study1,testdata_study1,se=T)

predict(glm(DONATE~Neutral+OptOut+age+as.numeric(gender)+as.numeric(irreligion),data=data_study1,family = binomial(link = "logit"))
  ,testdata_study1,se.fit=T)

#(testdata_study1<-cbind(testdata_study1,predict(glm(DONATE~Neutral+OptOut+age+as.numeric(gender)+as.numeric(irreligion),data=data_study1,family = binomial(link = "logit"))
#                                                ,testdata_study1,se.fit=T)[c("fit","se.fit")]))

















#IN和对照组比
logi.OptIn<-glm(DONATE~OptIn+
      gender+irreligion+age,#+knowledge,
    data=data_study1[data_study1$OptOut==0,]#subset(data_study1,data_study1$OptOut==0,select=c(DONATE,OptIn,gender,irreligion,age))
    ,family = binomial(link = "logit")) 
summary(logi.OptIn)
wald.test(b=coef(logi.OptIn),Sigma=vcov(logi.OptIn),Terms=2)
coef(logi.OptIn)
(1-exp((logi.OptIn$deviance-logi.OptIn$null.deviance)/nrow(data_study1[data_study1$OptOut==0,])))
(1-exp((logi.OptIn$deviance-logi.OptIn$null.deviance)/nrow(data_study1[data_study1$OptOut==0,])))/(1-exp((-logi.OptIn$null.deviance)/nrow(data_study1[data_study1$OptOut==0,])))

logi.OptOut<-glm(DONATE~OptOut+
                  gender+irreligion+age,#+knowledge,
                data=data_study1[data_study1$OptIn==0,]
                ,family = binomial(link = "logit")) 
summary(logi.OptOut)
wald.test(b=coef(logi.OptOut),Sigma=vcov(logi.OptOut),Terms=2)
coef(logi.OptOut)

#Cox-Snell R^2
(1-exp((logi.OptIn$deviance-logi.OptOut$null.deviance)/nrow(data_study1[data_study1$OptIn==0,])))
(1-exp((logi.OptIn$deviance-logi.OptOut$null.deviance)/nrow(data_study1[data_study1$OptIn==0,])))/(1-exp((-logi.OptOut$null.deviance)/nrow(data_study1[data_study1$OptIn==0,])))

#Cox-Snell R^2
Cox.Snell.R2_study1<- 1-exp((logi.study1$deviance-logi.study1$null.deviance)/nrow(data_study1))
#Nagelkerke R^2
Nagelkerke.R2_study1<-Cox.Snell.R2_study1/(1-exp((-logi.study1$null.deviance)/nrow(data_study1)))
print(c(Cox.Snell.R2_study2_OptIn,Nagelkerke.R2_study2_OptIn))


(OR_study1<-data.frame(exp(confint((logi.study1)))))
(exp((logi.study1$coefficients
      )))


#plot
  ggplot(data=data_study1,aes(x=default))+geom_bar(aes(weight=ifelse(data_study1$DONATE=="0",0,1)))+
    stat_compare_means(DONATE~default,data=data_study1)

  ggbarplot(data_study1, x="default", y="DONATE",label = TRUE)+
    stat_compare_means(ref.group = "Neutral")

  
