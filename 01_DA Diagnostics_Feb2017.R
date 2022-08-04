### Diagnostics and MLE for parameters optimized via data assimilation ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com) ###
### Last updated: July 23, 2017 ###
rm(list=ls())
setwd("C:\\Users\\Kevin.Wilcox\\Desktop\\fromOldComp\\FULL_ANALYSES_July2017\\")
source("Scripts\\00_Data read in and prep.R")

# Workspace setup ------------------------------------------------------------------
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(stats4)
library(coda)
library(ggthemes)
library(MASS)
require(gridExtra)
#setwd( "C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_July2017\\")


# MLE parameter estimates -------------------------------------------------
  #*Note: Sometimes standard deviations from weibull distributions don't work --
    # Because of this, I am only including MLE's from this section, I'll include CI's from HPD section below
param.mle <- data.frame(Site=c("sevblu","sevblk","sgs","chy","hys","knz"),
                        c1=0, c2=0, c3=0, c4=0, c5=0, c6=0,
                        f1=0, f2=0, f3=0, f4=0, f5=0, f6=0, f7=0,
                        mscut=0, Q10=0)
param.sd <- data.frame(Site=c("sevblu","sevblk","sgs","chy","hys","knz"),
                        c1=0, c2=0, c3=0, c4=0, c5=0, c6=0,
                        f1=0, f2=0, f3=0, f4=0, f5=0, f6=0, f7=0,
                        mscut=0, Q10=0)
### sevblu ###
sevblu.c1.hats <- fitdistr(sevblu.all$c1,"weibull", start=list(shape=15,scale=0.009))
sevblu.c2.hats <- fitdistr(sevblu.all$c2,"weibull", start=list(shape=15,scale=0.009))
sevblu.c3.hats <- fitdistr(sevblu.all$c3,"weibull", start=list(shape=15,scale=0.019))
sevblu.c4.hats <- fitdistr(sevblu.all$c4,"lognormal")
sevblu.c5.hats <- fitdistr(sevblu.all$c5,"lognormal")
sevblu.c6.hats <- fitdistr(sevblu.all$c6,"normal")
sevblu.f1.hats <- fitdistr(sevblu.all$f1,"weibull", start=list(shape=15,scale=0.65))
sevblu.f2.hats <- fitdistr(sevblu.all$f2,"normal")
sevblu.f3.hats <- fitdistr(sevblu.all$f3,"lognormal")
sevblu.f4.hats <- fitdistr(sevblu.all$f4,"normal")
sevblu.f5.hats <- fitdistr(sevblu.all$f5,"lognormal")
sevblu.f6.hats <- fitdistr(sevblu.all$f6,"normal")
sevblu.f7.hats <- fitdistr(sevblu.all$f7,"normal")
sevblu.mscut.hats <- fitdistr(sevblu.all$mscut, "normal")
sevblu.Q10.hats <- fitdistr(sevblu.all$Q10, "lognormal")

param.mle[1,2] <- sevblu.c1.hats$estimate[2]
param.mle[1,3] <- sevblu.c2.hats$estimate[2]
param.mle[1,4] <- sevblu.c3.hats$estimate[2]
param.mle[1,5] <- exp(sevblu.c4.hats$estimate[1])
param.mle[1,6] <- exp(sevblu.c5.hats$estimate[1])
param.mle[1,7] <- sevblu.c6.hats$estimate[1]
param.mle[1,8] <- sevblu.f1.hats$estimate[2]
param.mle[1,9] <- sevblu.f2.hats$estimate[1]
param.mle[1,10] <- exp(sevblu.f3.hats$estimate[1])
param.mle[1,11] <- sevblu.f4.hats$estimate[1]
param.mle[1,12] <- exp(sevblu.f5.hats$estimate[1])
param.mle[1,13] <- sevblu.f6.hats$estimate[1]
param.mle[1,14] <- sevblu.f7.hats$estimate[1]
param.mle[1,15] <- sevblu.mscut.hats$estimate[1]
param.mle[1,16] <- exp(sevblu.Q10.hats$estimate[1])

param.sd[1,2] <- sevblu.c1.hats$sd[2]
param.sd[1,3] <- sevblu.c2.hats$sd[2]
param.sd[1,4] <- sevblu.c3.hats$sd[2]
param.sd[1,5] <- sevblu.c4.hats$sd[1]
param.sd[1,6] <- sevblu.c5.hats$sd[1]
param.sd[1,7] <- sevblu.c6.hats$estimate[2]
param.sd[1,8] <- sevblu.f1.hats$sd[2]
param.sd[1,9] <- sevblu.f2.hats$estimate[2]
param.sd[1,10] <- sevblu.f3.hats$sd[1]
param.sd[1,11] <- sevblu.f4.hats$estimate[2]
param.sd[1,12] <- sevblu.f5.hats$sd[2]
param.sd[1,13] <- sevblu.f6.hats$estimate[2]
param.sd[1,14] <- sevblu.f7.hats$estimate[2]
param.sd[1,15] <- sevblu.mscut.hats$estimate[2]
param.sd[1,16] <- sevblu.Q10.hats$sd[1]

### sevblk ###
sevblk.c1.hats <- fitdistr(sevblk.c2$c1,"weibull", start=list(shape=15,scale=0.008))
sevblk.c2.hats <- fitdistr(sevblk.c2$c2,"weibull", start=list(shape=15,scale=0.01))
sevblk.c3.hats <- fitdistr(sevblk.c2$c3,"lognormal")
sevblk.c4.hats <- fitdistr(sevblk.c2$c4,"lognormal")
sevblk.c5.hats <- fitdistr(sevblk.c2$c5,"lognormal")
sevblk.c6.hats <- fitdistr(sevblk.c2$c6,"normal")
sevblk.f1.hats <- fitdistr(sevblk.f2$f1,"weibull", start=list(shape=15,scale=0.65))
sevblk.f2.hats <- fitdistr(sevblk.f2$f2,"normal")
sevblk.f3.hats <- fitdistr(sevblk.f2$f3,"lognormal")
sevblk.f4.hats <- fitdistr(sevblk.f2$f4,"normal")
sevblk.f5.hats <- fitdistr(sevblk.f2$f5,"normal")
sevblk.f6.hats <- fitdistr(sevblk.f2$f6,"normal")
sevblk.f7.hats <- fitdistr(sevblk.f2$f7,"normal")
sevblk.mscut.hats <- fitdistr(sevblk.mscut2$mscut, "normal")
sevblk.Q10.hats <- fitdistr(sevblk.Q102$Q10, "normal")

param.mle[2,2] <- sevblk.c1.hats$estimate[2]
param.mle[2,3] <- sevblk.c2.hats$estimate[2]
param.mle[2,4] <- exp(sevblk.c3.hats$estimate[1])
param.mle[2,5] <- exp(sevblk.c4.hats$estimate[1])
param.mle[2,6] <- exp(sevblk.c5.hats$estimate[1])
param.mle[2,7] <- sevblk.c6.hats$estimate[1]
param.mle[2,8] <- sevblk.f1.hats$estimate[2]
param.mle[2,9] <- sevblk.f2.hats$estimate[1]
param.mle[2,10] <- exp(sevblk.f3.hats$estimate[1])
param.mle[2,11] <- sevblk.f4.hats$estimate[1]
param.mle[2,12] <- sevblk.f5.hats$estimate[1]
param.mle[2,13] <- sevblk.f6.hats$estimate[1]
param.mle[2,14] <- sevblk.f7.hats$estimate[1]
param.mle[2,15] <- sevblk.mscut.hats$estimate[1]
param.mle[2,16] <- sevblk.Q10.hats$estimate[1]

param.sd[2,2] <- sevblk.c1.hats$sd[2]
param.sd[2,3] <- sevblk.c2.hats$sd[2]
param.sd[2,4] <- sevblk.c3.hats$sd[1]
param.sd[2,5] <- sevblk.c4.hats$sd[1]
param.sd[2,6] <- sevblk.c5.hats$sd[1]
param.sd[2,7] <- sevblk.c6.hats$estimate[2]
param.sd[2,8] <- sevblk.f1.hats$sd[2]
param.sd[2,9] <- sevblk.f2.hats$estimate[2]
param.sd[2,10] <- sevblk.f3.hats$sd[1]
param.sd[2,11] <- sevblk.f4.hats$estimate[2]
param.sd[2,12] <- sevblk.f5.hats$estimate[2]
param.sd[2,13] <- sevblk.f6.hats$estimate[2]
param.sd[2,14] <- sevblk.f7.hats$estimate[2]
param.sd[2,15] <- sevblk.mscut.hats$estimate[2]
param.sd[2,16] <- sevblk.Q10.hats$estimate[2]

### sgs ###
sgs.c1.hats <- fitdistr(sgs.c2$c1,"lognormal")
sgs.c2.hats <- fitdistr(sgs.c2$c2,"normal")
sgs.c3.hats <- fitdistr(sgs.c2$c3,"lognormal")
sgs.c4.hats <- fitdistr(sgs.c2$c4,"lognormal")
sgs.c5.hats <- fitdistr(sgs.c2$c5,"lognormal")
sgs.c6.hats <- fitdistr(sgs.c2$c6,"normal")
sgs.f1.hats <- fitdistr(sgs.f2$f1,"weibull", start=list(shape=15,scale=0.65))
sgs.f2.hats <- fitdistr(sgs.f2$f2,"weibull", start=list(shape=15,scale=0.13))
sgs.f3.hats <- fitdistr(sgs.f2$f3,"weibull", start=list(shape=15,scale=0.6))
sgs.f4.hats <- fitdistr(sgs.f2$f4,"normal")
sgs.f5.hats <- fitdistr(sgs.f2$f5,"normal")
sgs.f6.hats <- fitdistr(sgs.f2$f6,"normal")
sgs.f7.hats <- fitdistr(sgs.f2$f7,"normal")
sgs.mscut.hats <- fitdistr(sgs.mscut2$mscut, "normal")
sgs.Q10.hats <- fitdistr(sgs.Q102$Q10, "normal")

param.mle[3,2] <- exp(sgs.c1.hats$estimate[1])
param.mle[3,3] <- sgs.c2.hats$estimate[1]
param.mle[3,4] <- exp(sgs.c3.hats$estimate[1])
param.mle[3,5] <- exp(sgs.c4.hats$estimate[1])
param.mle[3,6] <- exp(sgs.c5.hats$estimate[1])
param.mle[3,7] <- sgs.c6.hats$estimate[1]
param.mle[3,8] <- sgs.f1.hats$estimate[2]
param.mle[3,9] <- sgs.f2.hats$estimate[2]
param.mle[3,10] <- sgs.f3.hats$estimate[2]
param.mle[3,11] <- sgs.f4.hats$estimate[1]
param.mle[3,12] <- sgs.f5.hats$estimate[1]
param.mle[3,13] <- sgs.f6.hats$estimate[1]
param.mle[3,14] <- sgs.f7.hats$estimate[1]
param.mle[3,15] <- exp(sgs.mscut.hats$estimate[1])
param.mle[3,16] <- sgs.Q10.hats$estimate[1]

param.sd[3,2] <- sgs.c1.hats$sd[1]
param.sd[3,3] <- sgs.c2.hats$estimate[2]
param.sd[3,4] <- sgs.c3.hats$sd[1]
param.sd[3,5] <- sgs.c4.hats$sd[1]
param.sd[3,6] <- sgs.c5.hats$sd[1]
param.sd[3,7] <- sgs.c6.hats$estimate[2]
param.sd[3,8] <- sgs.f1.hats$sd[2]
param.sd[3,9] <-  sgs.f2.hats$sd[2]
param.sd[3,10] <-  sgs.f3.hats$sd[2]
param.sd[3,11] <- sgs.f4.hats$estimate[2]
param.sd[3,12] <- sgs.f5.hats$estimate[2]
param.sd[3,13] <- sgs.f6.hats$estimate[2]
param.sd[3,14] <- sgs.f7.hats$estimate[2]
param.sd[3,15] <- sgs.mscut.hats$sd[1]
param.sd[3,16] <- sgs.Q10.hats$estimate[2]

### chy ###
chy.c1.hats <- fitdistr(chy.c2$c1,"weibull", start=list(shape=15,scale=0.008))
chy.c2.hats <- fitdistr(chy.c2$c2,"normal")
chy.c3.hats <- fitdistr(chy.c2$c3,"weibull", start=list(shape=15,scale=0.012))
chy.c4.hats <- fitdistr(chy.c2$c4,"weibull", start=list(shape=15,scale=0.04))
chy.c5.hats <- fitdistr(chy.c2$c5,"lognormal")
chy.c6.hats <- fitdistr(chy.c2$c6,"normal")
chy.f1.hats <- fitdistr(chy.f2$f1,"normal")
chy.f2.hats <- fitdistr(chy.f2$f2,"normal")
chy.f3.hats <- fitdistr(chy.f2$f3,"weibull", start=list(shape=15,scale=0.6))
chy.f4.hats <- fitdistr(chy.f2$f4,"normal")
chy.f5.hats <- fitdistr(chy.f2$f5,"normal")
chy.f6.hats <- fitdistr(chy.f2$f6,"normal")
chy.f7.hats <- fitdistr(chy.f2$f7,"normal")
chy.mscut.hats <- fitdistr(chy.mscut2$mscut, "normal")
chy.Q10.hats <- fitdistr(chy.Q102$Q10,"weibull", start=list(shape=15,scale=3.0))

param.mle[4,2] <- chy.c1.hats$estimate[2]
param.mle[4,3] <- chy.c2.hats$estimate[1]
param.mle[4,4] <- chy.c3.hats$estimate[2]
param.mle[4,5] <- chy.c4.hats$estimate[2]
param.mle[4,6] <- exp(chy.c5.hats$estimate[1])
param.mle[4,7] <- chy.c6.hats$estimate[1]
param.mle[4,8] <- chy.f1.hats$estimate[1]
param.mle[4,9] <- chy.f2.hats$estimate[1]
param.mle[4,10] <- chy.f3.hats$estimate[2]
param.mle[4,11] <- chy.f4.hats$estimate[1]
param.mle[4,12] <- chy.f5.hats$estimate[1]
param.mle[4,13] <- chy.f6.hats$estimate[1]
param.mle[4,14] <- chy.f7.hats$estimate[1]
param.mle[4,15] <- chy.mscut.hats$estimate[1]
param.mle[4,16] <- chy.Q10.hats$estimate[2]

param.sd[4,2] <- chy.c1.hats$sd[2]
param.sd[4,3] <- chy.c2.hats$estimate[2]
param.sd[4,4] <- chy.c3.hats$sd[2]
param.sd[4,5] <- chy.c4.hats$sd[2]
param.sd[4,6] <- chy.c5.hats$sd[1]
param.sd[4,7] <- chy.c6.hats$estimate[2]
param.sd[4,8] <- chy.f1.hats$estimate[2]
param.sd[4,9] <- chy.f2.hats$estimate[2]
param.sd[4,10] <- chy.f3.hats$sd[2]
param.sd[4,11] <- chy.f4.hats$estimate[2]
param.sd[4,12] <- chy.f5.hats$estimate[2]
param.sd[4,13] <- chy.f6.hats$estimate[2]
param.sd[4,14] <- chy.f7.hats$estimate[2]
param.sd[4,15] <- chy.mscut.hats$sd[1]
param.sd[4,16] <- chy.Q10.hats$sd[2]

### hys ###
hys.c1.hats <- fitdistr(hys.c2$c1,"lognormal")
hys.c2.hats <- fitdistr(hys.c2$c2,"lognormal")
hys.c3.hats <- fitdistr(hys.c2$c3,"lognormal")
hys.c4.hats <- fitdistr(hys.c2$c4,"normal")
hys.c5.hats <- fitdistr(hys.c2$c5,"lognormal")
hys.c6.hats <- fitdistr(hys.c2$c6,"normal")
hys.f1.hats <- fitdistr(hys.f2$f1,"normal")
hys.f2.hats <- fitdistr(hys.f2$f2,"normal")
hys.f3.hats <- fitdistr(hys.f2$f3,"weibull", start=list(shape=15,scale=0.55))
hys.f4.hats <- fitdistr(hys.f2$f4,"normal")
hys.f5.hats <- fitdistr(hys.f2$f5,"normal")
hys.f6.hats <- fitdistr(hys.f2$f6,"normal")
hys.f7.hats <- fitdistr(hys.f2$f7,"normal")
hys.mscut.hats <- fitdistr(hys.mscut2$mscut, "normal")
hys.Q10.hats <- fitdistr(hys.Q102$Q10,"normal")

param.mle[5,2] <- exp(hys.c1.hats$estimate[1])
param.mle[5,3] <- exp(hys.c2.hats$estimate[1])
param.mle[5,4] <- exp(hys.c3.hats$estimate[1])
param.mle[5,5] <- hys.c4.hats$estimate[1]
param.mle[5,6] <- exp(hys.c5.hats$estimate[1])
param.mle[5,7] <- hys.c6.hats$estimate[1]
param.mle[5,8] <- hys.f1.hats$estimate[1]
param.mle[5,9] <- hys.f2.hats$estimate[1]
param.mle[5,10] <- hys.f3.hats$estimate[2]
param.mle[5,11] <- hys.f4.hats$estimate[1]
param.mle[5,12] <- hys.f5.hats$estimate[1]
param.mle[5,13] <- hys.f6.hats$estimate[1]
param.mle[5,14] <- hys.f7.hats$estimate[1]
param.mle[5,15] <- hys.mscut.hats$estimate[1]
param.mle[5,16] <- hys.Q10.hats$estimate[1]

param.sd[5,2] <- hys.c1.hats$sd[1]
param.sd[5,3] <- hys.c2.hats$sd[1]
param.sd[5,4] <- hys.c3.hats$sd[1]
param.sd[5,5] <- hys.c4.hats$estimate[2]
param.sd[5,6] <- hys.c5.hats$sd[1]
param.sd[5,7] <- hys.c6.hats$estimate[2]
param.sd[5,8] <- hys.f1.hats$estimate[2]
param.sd[5,9] <- hys.f2.hats$estimate[2]
param.sd[5,10] <- hys.f3.hats$sd[2]
param.sd[5,11] <- hys.f4.hats$estimate[2]
param.sd[5,12] <- hys.f5.hats$estimate[2]
param.sd[5,13] <- hys.f6.hats$estimate[2]
param.sd[5,14] <- hys.f7.hats$estimate[2]
param.sd[5,15] <- hys.mscut.hats$estimate[2]
param.sd[5,16] <- hys.Q10.hats$estimate[2]

### knz ###
knz.c1.hats <- fitdistr(knz.c2$c1,"lognormal")
knz.c2.hats <- fitdistr(knz.c2$c2,"lognormal")
knz.c3.hats <- fitdistr(knz.c2$c3,"weibull", start=list(shape=15,scale=0.018))
knz.c4.hats <- fitdistr(knz.c2$c4,"normal")
knz.c5.hats <- fitdistr(knz.c2$c5,"lognormal")
knz.c6.hats <- fitdistr(knz.c2$c6,"normal")
knz.f1.hats <- fitdistr(knz.f2$f1,"normal")
knz.f2.hats <- fitdistr(knz.f2$f2,"normal")
knz.f3.hats <- fitdistr(knz.f2$f3,"weibull", start=list(shape=15,scale=0.55))
knz.f4.hats <- fitdistr(knz.f2$f4,"normal")
knz.f5.hats <- fitdistr(knz.f2$f5,"normal")
knz.f6.hats <- fitdistr(knz.f2$f6,"normal")
knz.f7.hats <- fitdistr(knz.f2$f7,"normal")
knz.mscut.hats <- fitdistr(knz.mscut2$mscut, "lognormal")
knz.Q10.hats <- fitdistr(knz.Q102$Q10,"weibull", start=list(shape=15,scale=3.0))

param.mle[6,2] <- exp(knz.c1.hats$estimate[1])
param.mle[6,3] <- exp(knz.c2.hats$estimate[1])
param.mle[6,4] <- knz.c3.hats$estimate[2]
param.mle[6,5] <- knz.c4.hats$estimate[1]
param.mle[6,6] <- exp(knz.c5.hats$estimate[1])
param.mle[6,7] <- knz.c6.hats$estimate[1]
param.mle[6,8] <- knz.f1.hats$estimate[1]
param.mle[6,9] <- knz.f2.hats$estimate[1]
param.mle[6,10] <- knz.f3.hats$estimate[2]
param.mle[6,11] <- knz.f4.hats$estimate[1]
param.mle[6,12] <- knz.f5.hats$estimate[1]
param.mle[6,13] <- knz.f6.hats$estimate[1]
param.mle[6,14] <- knz.f7.hats$estimate[1]
param.mle[6,15] <- exp(knz.mscut.hats$estimate[1])
param.mle[6,16] <- knz.Q10.hats$estimate[2]

param.sd[6,2] <- knz.c1.hats$sd[1]
param.sd[6,3] <- knz.c2.hats$sd[1]
param.sd[6,4] <- knz.c3.hats$sd[2]
param.sd[6,5] <- knz.c4.hats$sd[1]
param.sd[6,6] <- knz.c5.hats$sd[1]
param.sd[6,7] <- knz.c6.hats$sd[1]
param.sd[6,8] <- knz.f1.hats$sd[1]
param.sd[6,9] <- knz.f2.hats$sd[1]
param.sd[6,10] <- knz.f3.hats$sd[2]
param.sd[6,11] <- knz.f4.hats$sd[1]
param.sd[6,12] <- knz.f5.hats$sd[1]
param.sd[6,13] <- knz.f6.hats$sd[1]
param.sd[6,14] <- knz.f7.hats$sd[1]
param.sd[6,15] <- knz.mscut.hats$sd[1]
param.sd[6,16] <- knz.Q10.hats$sd[2]

write.csv(param.mle, file="DA Diagnostics and MLEs//MLE values all parameters all plots_4 north sites_July2017.csv",row.names=FALSE)
write.csv(param.sd, file="DA Diagnostics and MLEs//MLE sd all parameters all plots_4 north sites_July2017.csv",row.names=FALSE)



# Parameter confidence intervals -------------------------------------------
## all parameters ##
sevblu.HPD <- HPDinterval(sevblu.all.mcmc, prob=0.95)
sevblu.HPD.2 <- data.frame(site="a.sevblu", chain=c(sort(rep(1:4,15))),
                          param=c(paste("c",1:6,sep=""),paste("f",1:7,sep=""),"mscut","Q10"),
                          lower=c(sevblu.HPD[[1]][1:15],sevblu.HPD[[2]][1:15],sevblu.HPD[[3]][1:15],sevblu.HPD[[4]][1:15]),
                        	upper=c(sevblu.HPD[[1]][16:30],sevblu.HPD[[2]][16:30],sevblu.HPD[[3]][16:30],sevblu.HPD[[4]][16:30]))

sevblk.HPD <- HPDinterval(sevblk.all.mcmc, prob=0.95)
sevblk.HPD.2 <- data.frame(site="a.sevblk", chain=c(sort(rep(1:4,15))),
                           param=c(paste("c",1:6,sep=""),paste("f",1:7,sep=""),"mscut","Q10"),
                           lower=c(sevblk.HPD[[1]][1:15],sevblk.HPD[[2]][1:15],sevblk.HPD[[3]][1:15],sevblk.HPD[[4]][1:15]),
                           upper=c(sevblk.HPD[[1]][16:30],sevblk.HPD[[2]][16:30],sevblk.HPD[[3]][16:30],sevblk.HPD[[4]][16:30]))

sgs.HPD <- HPDinterval(sgs.all.mcmc, prob=0.95)
sgs.HPD.2 <- data.frame(site="a.sgs", chain=c(sort(rep(1:3,15))),
                           param=c(paste("c",1:6,sep=""),paste("f",1:7,sep=""),"mscut","Q10"),
                           lower=c(sgs.HPD[[1]][1:15],sgs.HPD[[2]][1:15],sgs.HPD[[3]][1:15]),
                           upper=c(sgs.HPD[[1]][16:30],sgs.HPD[[2]][16:30],sgs.HPD[[3]][16:30]))

chy.HPD <- HPDinterval(chy.all.mcmc, prob=0.95)
chy.HPD.2 <- data.frame(site="a.chy", chain=c(sort(rep(1:3,15))),
                           param=c(paste("c",1:6,sep=""),paste("f",1:7,sep=""),"mscut","Q10"),
                           lower=c(chy.HPD[[1]][1:15],chy.HPD[[2]][1:15],chy.HPD[[3]][1:15]),
                           upper=c(chy.HPD[[1]][16:30],chy.HPD[[2]][16:30],chy.HPD[[3]][16:30]))

hys.HPD <- HPDinterval(hys.all.mcmc, prob=0.95)
hys.HPD.2 <- data.frame(site="a.hys", chain=c(sort(rep(1:3,15))),
                           param=c(paste("c",1:6,sep=""),paste("f",1:7,sep=""),"mscut","Q10"),
                           lower=c(hys.HPD[[1]][1:15],hys.HPD[[2]][1:15],hys.HPD[[3]][1:15]),
                           upper=c(hys.HPD[[1]][16:30],hys.HPD[[2]][16:30],hys.HPD[[3]][16:30]))

knz.HPD <- HPDinterval(knz.all.mcmc, prob=0.95)
knz.HPD.2 <- data.frame(site="a.knz", chain=c(sort(rep(1:3,15))),
                           param=c(paste("c",1:6,sep=""),paste("f",1:7,sep=""),"mscut","Q10"),
                           lower=c(knz.HPD[[1]][1:15],knz.HPD[[2]][1:15],knz.HPD[[3]][1:15]),
                           upper=c(knz.HPD[[1]][16:30],knz.HPD[[2]][16:30],knz.HPD[[3]][16:30]))

HPD.all <- rbind(sgs.HPD.2,chy.HPD.2,hys.HPD.2,knz.HPD.2)	
write.csv(HPD.all, file="HPD 95 CI all chains_4 north sites_July2017.csv",row.names=FALSE)	
	
HPD.means <- ddply(HPD.all, .(site, param), summarize,
  upper = mean(upper),
	lower=mean(lower))
HPD.means <- ddply(sevblu.HPD.2, .(site, param), summarize,
  upper = mean(upper),
	lower=mean(lower))
write.csv(HPD.means, file="HPD 95 CI_avg across chains_4 north sites_July2017.csv",row.names=FALSE)	

# Gelman diagnostics ------------------------------------------------
# sevblu #
sevblu.gf <- gelman.diag(sevblu.all.mcmc)
sevblu.gelman <-data.frame(site="a.sevblu",
                             param=c(paste("c",1:6,sep=""),paste("f",1:7,sep=""),"mscut","Q10"),
                             GR.est=sevblu.gf$psrf[,1],
                             GR.uCI=sevblu.gf$psrf[,2])

sevblk.gf <- gelman.diag(sevblk.all.mcmc)
sevblk.gelman <-data.frame(site="b.sevblk",
                           param=c(paste("c",1:6,sep=""),paste("f",1:7,sep=""),"mscut","Q10"),
                           GR.est=sevblk.gf$psrf[,1],
                           GR.uCI=sevblk.gf$psrf[,2])

sgs.gf <- gelman.diag(sgs.all.mcmc)
sgs.gelman <-data.frame(site="c.sgs",
                           param=c(paste("c",1:6,sep=""),paste("f",1:7,sep=""),"mscut","Q10"),
                           GR.est=sgs.gf$psrf[,1],
                           GR.uCI=sgs.gf$psrf[,2])

chy.gf <- gelman.diag(chy.all.mcmc)
chy.gelman <-data.frame(site="d.chy",
                           param=c(paste("c",1:6,sep=""),paste("f",1:7,sep=""),"mscut","Q10"),
                           GR.est=chy.gf$psrf[,1],
                           GR.uCI=chy.gf$psrf[,2])

hys.gf <- gelman.diag(hys.all.mcmc)
hys.gelman <-data.frame(site="e.hys",
                           param=c(paste("c",1:6,sep=""),paste("f",1:7,sep=""),"mscut","Q10"),
                           GR.est=hys.gf$psrf[,1],
                           GR.uCI=hys.gf$psrf[,2])

knz.gf <- gelman.diag(knz.all.mcmc)
knz.gelman <-data.frame(site="f.knz",
                           param=c(paste("c",1:6,sep=""),paste("f",1:7,sep=""),"mscut","Q10"),
                           GR.est=knz.gf$psrf[,1],
                           GR.uCI=knz.gf$psrf[,2])
gelman.all <- rbind(sgs.gelman, rbind(chy.gelman,rbind(hys.gelman,knz.gelman)))

write.csv(gelman.all, file="DA Diagnostics and MLEs\\gelman statistics_4 north sites_July2017.csv", row.names=F)

# Cross correlation diagnostics -------------------------------------------
#sevblu.xcorr.plot <- crosscorr.plot(sevblu.all.mcmc)
sevblu.xcorr <- crosscorr(sevblu.all.mcmc)
sevblu.xcorr.df <- as.data.frame(sevblu.xcorr)
sevblu.xcorr.df$site<-"sevblu"
sevblu.xcorr.df$param<-rownames(sevblu.xcorr.df)

sevblk.xcorr <- crosscorr(sevblk.all.mcmc)
sevblk.xcorr.df <- as.data.frame(sevblk.xcorr)
sevblk.xcorr.df$site<-"sevblk"
sevblk.xcorr.df$param<-rownames(sevblk.xcorr.df)

sgs.xcorr <- crosscorr(sgs.all.mcmc)
sgs.xcorr.df <- as.data.frame(sgs.xcorr)
sgs.xcorr.df$site<-"sgs"
sgs.xcorr.df$param<-rownames(sgs.xcorr.df)

chy.xcorr <- crosscorr(chy.all.mcmc)
chy.xcorr.df <- as.data.frame(chy.xcorr)
chy.xcorr.df$site<-"chy"
chy.xcorr.df$param<-rownames(chy.xcorr.df)

hys.xcorr <- crosscorr(hys.all.mcmc)
hys.xcorr.df <- as.data.frame(hys.xcorr)
hys.xcorr.df$site<-"hys"
hys.xcorr.df$param<-rownames(hys.xcorr.df)

knz.xcorr <- crosscorr(knz.all.mcmc)
knz.xcorr.df <- as.data.frame(knz.xcorr)
knz.xcorr.df$site<-"knz"
knz.xcorr.df$param<-rownames(knz.xcorr.df)

xcorr.all <- rbind(sgs.xcorr.df,rbind(chy.xcorr.df,rbind(hys.xcorr.df,knz.xcorr.df)))
write.csv(xcorr.all, file="DA Diagnostics and MLEs\\Cross correlation_all parameters_4 north sites_July2017.csv",row.names=FALSE)

crosscorr.plot(sevblu.all.mcmc)
crosscorr.plot(sevblk.all.mcmc)
crosscorr.plot(sgs.all.mcmc)
crosscorr.plot(chy.all.mcmc)
crosscorr.plot(hys.all.mcmc)
crosscorr.plot(knz.all.mcmc)

	
	
	
	
	
	
	
	
	


