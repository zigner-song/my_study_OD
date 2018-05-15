###step1


data_study3<-read.csv("data_study3.csv",stringsAsFactors = F)
data_study3_round2<-read.csv("data_study3_round2.csv",stringsAsFactors = F)
dim(data_study3_round2)
dim(data_study3)
data_study3$round<-1
data_study3_round2$round<-2
data_study3_round2$ID<-data_study3_round2$ID+rep(1000,nrow(data_study3_round2))

data_study3<-rbind(data_study3,data_study3_round2)

organs<-c("MARROW","SMALL.INTESTINE","PANCREAS",
          "CARDIAC.VALVES","THYMUS","LIVER","KIDNEY","SPLEEN","LUNG","VEIN","HEART","TENDON","CORNEA",
          "BONES","SKIN","EYE")
organsH<-c("BONES","SKIN","EYE")
organsL<-c("MARROW","SMALL.INTESTINE","PANCREAS",
           "CARDIAC.VALVES","THYMUS","LIVER","KIDNEY","SPLEEN","LUNG","VEIN","HEART","TENDON","CORNEA")

demography<-c("gender","age","ethnic","education","school","major","grade","religion","health")
scale<-paste("item",trimws(1:23),sep="")
experience<-c("How_familiar","learning","HeardOfOrNot",
              "from媒体","from宣传手册","from学校","from医院或医护人员","from朋友","from家人","from其他",
              "T13","T14","T15","T16")





#valid
data_study3<-subset(data_study3,
                    IsParticipant==2 & IsAtSchool==1 & V1==2 & V2==3 & (VALID1.1==2 | VALID1.2==2 | VALID1.3==2 | VALID1.4==2 | VALID1.5==2 | VALID1.6==2 | VALID1.7==2))

#merge
VarName<-paste("VALID1.",trimws(1:7),sep="")

  #version Name
  data_study3[VarName]<-is.na(data_study3[VarName])==F
  data_study3$version<-as.matrix(data_study3[VarName]) %*% as.matrix(1:7,nrow=7,ncol=1)
  
  #merge the organs
  for(i in 1:16){
    VarName<-paste(organs[i],trimws(1:6),sep="")
    data_study3[organs[i]]<-apply(data_study3[VarName],na.rm=T,MARGIN = 1,FUN = mean)
  }
  data_study3$N_Organs<-apply(data_study3[organs],MARGIN = 1, FUN = mean)
  data_study3$N_OrgansH<-apply(data_study3[organsH],MARGIN = 1, FUN = mean)
  data_study3$N_OrgansL<-apply(data_study3[organsL],MARGIN = 1, FUN = mean)
  
  #merge donate
  VarName<-paste("DONATE1.",trimws(1:7),sep="")
  data_study3$DONATE<-apply(data_study3[VarName],na.rm=T,MARGIN = 1,FUN = mean)
  

data_study3<-subset(data_study3,select = c("ID","IP","version",
                                           "DONATE",organs,"N_Organs","N_OrgansH","N_OrgansL",
                                           scale,demography,experience))
data_study3$study<-3  
  
  

  
#coding
  #coding version  -  dummy
  dummy.version<-data.frame(dummy.code(data_study3$version))
  names(dummy.version)<-paste("version",1:7,sep="")
    data_study3<-cbind(data_study3,dummy.version)
  
  
  #democraphy
    #PROVINCE
    data_study3$province<-
      matrix(unlist(strsplit(sub("(","-",data_study3$IP,fixed=T),split="-",fixed=T)),ncol=3,byrow = T)[,2]
    demography<-c(demography,"province")
    
    #gender
    data_study3$gender<-factor(data_study3$gender,levels=c(1,2),labels=c("Male","Female"))
    
    #ethnic --> minority
    data_study3$minority<-ifelse(data_study3$ethnic %in% c("汉","汉族","汗"),0,1)
    data_study3$minority<-factor(data_study3$minority,levels=c(0,1),labels=c("HAN","MINORITY"))
    demography<-c(demography,"minority")
    
    #religion --> irreligious
    data_study3$irreligion<-ifelse(data_study3$religion==1,0,1)
    demography<-c(demography,"irreligion")
    
  #统一和study2的数据类型
    data_study3$study<-factor(data_study3$study)
    #data_study3$version<-factor(data_study3$version)
    data_study3$DONATE[data_study3$DONATE==3]<-0
    data_study3$DONATE[data_study3$DONATE==2 & data_study3$version==7]<-0
    
    
    versionName_study3<-c("a_Y","a_N","d_Y","d_N","a_s_Y","d_s_Y","baseline")
    study3_version<-data.frame(version=1:7,versionName_study3)
    
    
  
# data write
    c("ID","study","IP","version",
      "DONATE",organs,"N_Organs","N_OrgansH","N_OrgansL",
      scale,demography,experience,names(dummy.version)) %in% names(data_study3)
    
    
    data_study3<-subset(data_study3,select = c("ID","study","IP","version",
                                               "DONATE",organs,"N_Organs","N_OrgansH","N_OrgansL",
                                               scale,demography,experience,names(dummy.version)))
   
    
    
    write.csv(data_study3,paste("data_study3_",Sys.Date(),".csv",sep=""))
  