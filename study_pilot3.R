


data_pilot.study3<-read.csv("data_pilot.study3.csv")
data_pilot.study3<-subset(data_pilot.study3,data_pilot.study3$valid==T)
data_pilot.study3$study<-rep("3.0",nrow(data_pilot.study3))

###################################
#        appearance               #
###################################

#appearance
data_appearance<-subset(data_pilot.study3,select = c("ID",
                                                     "appearance_kidney",
                                                     "appearance_liver",
                                                     "appearance_heart",
                                                     "appearance_lung",
                                                     "appearance_spleen",
                                                     "appearance_pancreas",
                                                     "appearance_thymus" ,
                                                     "appearance_small.intestine",
                                                     "appearance_cornea",
                                                     "appearance_marrow",
                                                     "appearance_cardiac.valves",
                                                     "appearance_bones",
                                                     "appearance_skin",
                                                     "appearance_vein",
                                                     "appearance_tendon",
                                                     "appearance_eye"
))
names(data_appearance)<-c("ID","kidney","liver","heart","lung","spleen","pancreas","thymus","small.intestine","cornea","marrow","cardiac.valves","bones","skin","vein","tendon","eye")




#data_appearance<-melt(data=data_appearance,id="ID")

data_appearance2<-melt(data=data_appearance,id="ID")
names(data_appearance2)<-c("ID","organ","appearance")

#fa

library(psych);library("GPArotation")
ins<-fa(cov(data_appearance[-1]),nfactors=2,rotate="promax")#%>% fa.diagram(.,simple=F)
ins$loadings


#anova
anova_appearance<-aov(value~variable+Error(ID/variable),data=data_appearance)
summary(anova_appearance)

data_appearance2$organ<-toupper(data_appearance2$organ)
#
data_appearance2$appearance.code<-ifelse(data_appearance2$organ %in% organsH,1,0)
m_ins<-lmerTest::lmer(appearance~appearance.code+(appearance.code|ID),data=data_appearance2)
summary(m_ins)
summaryBy(appearance~appearance.code,data=data_appearance2)

summaryBy(appearance~ID+appearance.code,data=data_appearance2) %>%
  t.test(appearance.mean~appearance.code,paired=T,data=.)

summaryBy(appearance~ID+appearance.code,data=data_appearance2) %>% 
  summaryBy(appearance.mean~appearance.code,data=.)

cohen.d(appearance.mean~appearance.code,paired=T,data=summaryBy(appearance~ID+appearance.code,data=data_appearance2))


#calculate the se
glm(value~variable,data=data_appearance) %>%
  predict(.,data.frame(variable=levels(data_appearance$variable)),
          se.fit=T,type="response")


#summarize
data_appearance<-summaryBy(value~variable,data=data_appearance,FUN=fct)

#organ_color


data_appearance<-data_appearance[order(data_appearance$value.mean),]


p_appearance<-
  ggplot(data=data_appearance,mapping=aes(x=reorder(variable,value.mean),y=value.mean),fill=variable)
p_appearance<-p_appearance+geom_bar(stat="identity",fill = data_appearance$color)
p_appearance<-p_appearance+xlab("appearance")+theme(axis.text.x = element_text(size=10,angle=45))


p_appearance

###################################
#        importance               #
###################################
#importance
data_importance<-subset(data_pilot.study3,select = c("ID",
                                                     "importance_kidney",
                                                     "importance_liver",
                                                     "importance_heart",
                                                     "importance_lung",
                                                     "importance_spleen",
                                                     "importance_pancreas",
                                                     "importance_thymus" ,
                                                     "importance_small.intestine",
                                                     "importance_cornea",
                                                     "importance_marrow",
                                                     "importance_cardiac.valves",
                                                     "importance_bones",
                                                     "importance_skin",
                                                     "importance_vein",
                                                     "importance_tendon",
                                                     "importance_eye"))
names(data_importance)<-c("ID","kidney","liver","heart","lung","spleen","pancreas","thymus","small.intestine","cornea","marrow","cardiac.valves","bones","skin","vein","tendon","eye")



data_importance<-melt(data=data_importance,id="ID")
#anova
anova_importance<-aov(value~variable+Error(ID/variable),data=data_importance)
summary(anova_importance)

#summarize
data_importance<-summaryBy(value~variable,data=data_importance,FUN=fct)
data_importance<-merge(data_importance,organ_color,id="variable")
data_importance<-data_importance[order(data_importance$value.mean),]


p_importance<-
  ggplot(data=data_importance,mapping=aes(x=reorder(variable,value.mean),y=value.mean),
         fill=variable)+
  geom_bar(stat="identity",fill=data_importance$color)+
  xlab("importance")+
  theme(axis.text.x = element_text(size=10,angle=45))

#p_importance<-p_importance+geom_errorbar(aes(ymin=value.mean-value.sd/sqrt(value.len), ymax=value.mean+value.sd/sqrt(value.len)),width=0.1)

p_importance

###################################
#        necessity                #
###################################
#necessity
data_necessity<-subset(data_pilot.study3,select = c("ID",
                                                    "necessity_kidney",
                                                    "necessity_liver",
                                                    "necessity_heart",
                                                    "necessity_lung",
                                                    "necessity_spleen",
                                                    "necessity_pancreas",
                                                    "necessity_thymus" ,
                                                    "necessity_small.intestine",
                                                    "necessity_cornea",
                                                    "necessity_marrow",
                                                    "necessity_cardiac.valves",
                                                    "necessity_bones",
                                                    "necessity_skin",
                                                    "necessity_vein",
                                                    "necessity_tendon",
                                                    "necessity_eye"))
names(data_necessity)<-c("ID",
                         "kidney","liver","heart","lung","spleen","pancreas","thymus","small.intestine","cornea","marrow","cardiac.valves","bones","skin","vein","tendon","eye")



data_necessity<-melt(data=data_necessity,id="ID")
#anova
anova_necessity<-aov(value~variable+Error(ID/variable),data=data_necessity)
summary(anova_necessity)

#summarize
data_necessity<-summaryBy(value~variable,data=data_necessity,FUN=fct)
data_necessity<-merge(data_necessity,organ_color,id="variable")
data_necessity<-data_necessity[order(data_necessity$value.mean),]


p_necessity<-
  ggplot(data=data_necessity,mapping=aes(x=reorder(variable,value.mean),y=value.mean),fill=variable)+geom_bar(stat="identity",fill=data_necessity$color)+xlab("necessity")+theme(axis.text.x = element_text(size=10,angle=45))


p_necessity

data_organ<-data.frame(organ=data_appearance$variable,
                       appearance.mean=data_appearance$value.mean#,
                       #appearance.sd=data_appearance$value.sd,
                       #importance.mean=data_importance$value.mean,
                       #importance.sd=data_importance$value.sd,
                      # necessity.mean=data_necessity$value.mean
                       #necessity.sd=data_necessity$value.sd
)
row.names(data_organ)<-data_organ$organ
data_organ<-data_organ[-1]
#z_score of
data_organ.z<-data.frame(scale(data_organ))

library(cluster)
data_organ.clu<-agnes(data_organ.z,stand=TRUE,method="ward")
plot(data_organ.clu)

data_organ$clu<-cutree(data_organ.clu,k=2)

#kmeans
kmeans_organ<-kmeans(data_organ.z,2)
data_organ$cluster.all<-kmeans_organ$cluster

kmeans_organ.appearance<-kmeans(data_organ.z[1],2)
data_organ$cluster.appearance<-kmeans_organ.appearance$cluster

kmeans_organ.appearance.necessity<-kmeans(data_organ.z[-2],3)
data_organ$cluster.appearance.necessity<-kmeans_organ.appearance.necessity$cluster

kmeans_organ.necessity<-kmeans(data_organ.z[3],3)
data_organ$cluster.necessity<-kmeans_organ.necessity$cluster
order(data_organ$necessity.mean)


data_organ[c(3,7)]
data_organ$rank<-16:1


#t.test

data_appearance

