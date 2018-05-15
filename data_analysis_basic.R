library(doBy)
library(ggplot2)
library(reshape2)
library(psych)
library(dplyr)
library(lme4)

library(ggpubr)

library(car)
library(ggthemes)

library(effsize)
library(lsr)

#library(aod)
#library(survey) #caculate the wald

library(lsmeans)

setwd("E:/project/OrganDonation/Nudge/data_analysis/data_analysis")

fct<-function(x,...){c(mean=mean(x,...),sd=sd(x,...),n=length(x))}


dstats<-function(x,...){c(mean=mean(x,...)-1,length=length(x,...))}

#source('E:/R/transProvince.R', encoding = 'UTF-8')

my.lmer.nagelkerke <- function(f,d) {
  lmer.full= glmer(formula= as.formula(paste(f, collapse="+")), d, family="binomial")
  logLik.lmer.full= as.numeric(logLik(lmer.full))
  N.lmer.full= nrow(lmer.full@X)
  cat(paste("Full mixed model: L=", logLik.lmer.full, ", N=", N.lmer.full, "\n", sep=""))
  
  lmer.intercept= glmer(formula= as.formula(paste(unlist(strsplit(f[1], "~"))[1], paste("1", f[2], sep=" + "), sep="~ ")), data= d, family="binomial")
  logLik.lmer.intercept= as.numeric(logLik(lmer.intercept))
  N.lmer.intercept= nrow(lmer.intercept@X)
  cat(paste("Intercept mixed model: L=", logLik.lmer.intercept, ", N=", N.lmer.intercept, "\n", sep=""))
  
  lrm.full= lrm(formula= as.formula(f[1]), data= d)
  logLik.lrm.intercept= as.numeric(deviance(lrm.full)[1] / - 2)
  N.lrm.intercept= as.numeric(lrm.full$stats[1])
  cat(paste("Intercept ordinary model: L=", logLik.lrm.intercept, ", N=", N.lrm.intercept, "\n", sep=""))
  
  coxsnell.lmer= 1 - exp((logLik.lmer.intercept - logLik.lmer.full) * (2/N.lmer.full))
  nagelkerke.lmer= coxsnell.lmer / (1 - exp(logLik.lmer.intercept * (2/N.lmer.full)))
  cat(paste("Full model evaluated against mixed intercept model:\n\tCoxSnell R2: ", 
            coxsnell.lmer, "\n\tNagelkerke R2: ", nagelkerke.lmer,"\np="))
            coxsnell.lrm= 1 - exp((logLik.lrm.intercept - logLik.lmer.full) * (2/N.lmer.full))
            nagelkerke.lrm= coxsnell.lrm / (1 - exp(logLik.lrm.intercept * (2/N.lmer.full)))
            cat(paste("Full model evaluated against ordinary intercept model:\n\tCoxSnell R2: ",
coxsnell.lrm, "\n\tNagelkerke R2: ", nagelkerke.lrm,"\np="))
  coxsnell.lrm.2= 1 - exp((logLik.lrm.intercept - logLik.lmer.intercept) * (2/N.lmer.full))
  nagelkerke.lrm.2= coxsnell.lrm.2 / (1 - exp(logLik.lrm.intercept * (2/N.lmer.full)))
  cat(paste("Mixed intercept model evaluated against ordinary intercept model:\n\tCoxSnell R2: ", 
coxsnell.lrm.2, "\n\tNagelkerke R2: ", nagelkerke.lrm.2,"\np="))
}
