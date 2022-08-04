### Bootstrapping residence times ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com) ###
### Last updated: Feb. 8, 2017 ###

rm(list=ls())
#source("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_July2017\\Scripts\\00_Data read in and prep.R")
setwd( "C:\\Users\\Kevin.Wilcox\\Desktop\\fromOldComp\\FULL_ANALYSES_July2017\\")
source("Scripts\\00_Data read in and prep_4north.R") ### Reads in parameter strings from MCMC

library(fitdistrplus)
library(plyr)
library(ggplot2)
library(data.table)
library(stats4)
library(coda)
library(ggthemes)

# Tau function ------------------------------------------------------------

tau.fxn <- function(Nt, temp, moist, mscut, Q10){ # Function to create tau for each day in simulation
  product <- numeric(Nt+1)
  for(i in 1:Nt+1){ # Creates "product" which is the combined effect of temperature and moisture on C turnover
    # Temperature effect on tau #
    sumtemp <- 0
    tmp <- temp[i]
    if(i > 10){ # Loop that calculates moving 10 day average # 
      for(j in (i-9):i){
        sumtemp <- sumtemp+temp[j]
      }
      tmp = sumtemp/10
    }
    # old variables in model were R10=0.65 and Q10=2.2; I've changed these to reflect those estimated by Zheng in his Ecosphere paper
    tmp <- 0.58*Q10^((tmp-10)/10) ## Could designate some estimable parameters for R10 and Q10 here... the .65 number is R10 and the 2.2 is Q10 I think #
    # Moisture effect on tau #
    moisture = 1
    if(moist[i] < mscut){
      moisture <- 1.0 - 5.0*(mscut-moist[i])
    }
    product[i] <- tmp*moisture
  }
  return(product)
}	


# Data import for calculating daily tau's -------------------------------------------------------
## Read in soil moisture and temp data ##
sevblu.moist.raw <- read.csv("DA input\\sevblu\\moist5cm_blue_sev13_14_fromSite_tao.csv")
sevblu.temp.raw <- read.csv("DA input\\sevblu\\temp5cm_blue_sev13_14_fromSite_tao.csv") 
sevblk.moist.raw <- read.csv("DA input\\sevblk\\moist5cm_black_sev13_14_fromSite_tao.csv")
sevblk.temp.raw <- read.csv("DA input\\sevblk\\temp5cm_black_sev13_14_fromSite_tao.csv") 
sgs.moist.raw <- read.csv("DA input\\sgs\\sgs_smoist_tau_13-15.csv")
sgs.temp.raw <- read.csv("DA input\\sgs\\sgs_stemp_tau_13-15.csv") 
chy.moist.raw <- read.csv("DA input\\chy\\chy_smoist_tau_13-15.csv")
chy.temp.raw <- read.csv("DA input\\chy\\chy_stemp_tau_13-15.csv") 
hys.moist.raw <- read.csv("DA input\\hys\\hys_smoist_tau_13-15.csv")
hys.temp.raw <- read.csv("DA input\\hys\\hys_stemp_tau_13-15.csv") 
knz.moist.raw <- read.csv("DA input\\knz\\knz_smoist_tau_13-15.csv")
knz.temp.raw <- read.csv("DA input\\knz\\knz_stemp_tau_13-15.csv") 

sevblk.moist <- sevblk.moist.raw$Smoist
sevblk.temp <- sevblk.temp.raw$Stemp
sevblu.moist <- sevblu.moist.raw$Smoist
sevblu.temp <- sevblu.temp.raw$Stemp
sgs.moist <- sgs.moist.raw$Smoist
sgs.temp <- sgs.temp.raw$temp
chy.moist <- chy.moist.raw$Smoist
chy.temp <- chy.temp.raw$temp
hys.moist <- hys.moist.raw$Smoist
hys.temp <- hys.temp.raw$temp
knz.moist <- knz.moist.raw$Smoist
knz.temp <- knz.temp.raw$temp

## Sample size ##
sevblu.Nt <- length(sevblu.temp)-1
sevblk.Nt <- length(sevblk.temp)-1
sgs.Nt <- length(sgs.temp)-1
chy.Nt <- length(chy.temp)-1
hys.Nt <- length(hys.temp)-1
knz.Nt <- length(knz.temp)-1

## GPP partitioning parameters ##
sevblu.b <- c(0.18/.55,0.37/.55,0,0,0,0)
sevblk.b <- c(0.18/.55,0.37/.55,0,0,0,0)
sgs.b <- c(0.25/.55,0.30/.55,0,0,0,0)
chy.b <- c(0.20/.55,0.35/.55,0,0,0,0)
hys.b <- c(0.39/.55,0.16/.55,0,0,0,0)
knz.b <- c(0.34/.55,0.21/.55,0,0,0,0)

# Function for calculating residence time using daily tau -----------------

Res_time <- function(tau.,c_new,f_new,b){
  AC <- matrix(0,nrow=length(c_new), ncol=length(c_new))
  AC[1,] <- c(    -c_new[1],    0,             0,               0,                   0,                0)
  AC[2,] <- c(	0,           -c_new[2],      0,               0,                   0,                0) ## 
  AC[3,] <- c(	c_new[1],  	 c_new[2],    -c_new[3],      	  0,                   0,                0) # I'm not sure that all of c2 should go into litter... if plant respiration is occurring, this should detract from the amount of C in this pool... and since we put GPP into the model, this should be accounted for... ask Yiqi Zheng about this.
  AC[4,] <- c(	0,  		  0,   	   f_new[1]*c_new[3],   -c_new[4],       f_new[5]*c_new[5], f_new[7]*c_new[6])
  AC[5,] <- c(	0,            0,       f_new[2]*c_new[3], f_new[3]*c_new[4],   -c_new[5],            0)
  AC[6,] <- c(    0,      	  0,             0,           f_new[4]*c_new[4], f_new[6]*c_new[5],     -c_new[6]) #the constant matrix A
  t_pools.daily <-  data.frame(Day=0,lf_Rt=0,rt_Rt=0, 
                               lt_Rt=0,mC_Rt=0, 
                               sC_Rt=0,pC_Rt=0,
                               Eco_Rt=0,stringsAsFactors=FALSE)
  
  for(i in 1:length(tau.)){
    t_pools.temp <- (solve(AC*tau.[i])%*%b)/-365
    tE.temp <-  colSums(t_pools.temp)
    t_pools.daily=rbind(t_pools.daily, c(i,t(t_pools.temp),tE.temp))
  }
  #  t_both <- list(t_pools=t_pools, tEcos=tE)
  #return(t_pools)
  return(t_pools.daily[-1,])
  
}


# Bootstrapping residence times -------------------------------------------
boot.nums <- sample(x=1:nrow(sgs.all), size=1000, replace=TRUE)

## sevblu ##
sevblu.Res.times.boot <- data.frame(run=1:length(boot.nums),lf_Rt=0,rt_Rt=0,lt_Rt=0,
                                    mC_Rt=0,sC_Rt=0,pC_Rt=0,Eco_Rt=0)
pb = txtProgressBar(min = 0, max = length(boot.nums), initial = 0) 
for(i in 1:length(boot.nums)){
  setTxtProgressBar(pb,i)
  tau.temp <- tau.fxn(Nt=sevblu.Nt, 
                      moist=sevblu.moist, 
                      temp=sevblu.temp,
                      mscut=sevblu.all$mscut[boot.nums[i]], 
                      Q10=sevblu.all$Q10[boot.nums[i]])
  
  sevblu.Rt <- Res_time(tau.=tau.temp[-1], 
                        c_new=as.numeric(sevblu.all[boot.nums[i],3:8]), 
                        f_new=as.numeric(sevblu.all[boot.nums[i],9:15]), 
                        b=sevblu.b)
  sevblu.Res.times.boot[i,2:ncol(sevblu.Res.times.boot)] <- colMeans(sevblu.Rt[2:ncol(sevblu.Rt)])
}

hist(subset(sevblu.Res.times.boot, Eco_Rt<100)$Eco_Rt, breaks=30)
sevblu.hats<-fitdistr(subset(sevblu.Res.times.boot,Eco_Rt<100)$Eco_Rt, "lognormal")
#sevblu.hats<-fitdistr(sevblu.Res.times.boot$Eco_Rt, "lognormal")
sevblu.ci <- qlnorm(c(0.025,0.05,0.25,0.5,0.75,0.95,0.975),meanlog=sevblu.hats$estimate[1],sdlog=sevblu.hats$estimate[2])
write.csv(sevblu.Res.times.boot, file="Residence times\\sevblu bootstrapped residence times 1000 runs.csv",row.names=FALSE)
sevblu.Res.times.boot <- read.csv("Residence times\\sevblu bootstrapped residence times 1000 runs.csv")


## sevblk ##
boot.nums <- sample(x=1:nrow(sevblk.all), size=1000, replace=TRUE)
sevblk.Res.times.boot <- data.frame(run=1:length(boot.nums),lf_Rt=0,rt_Rt=0,lt_Rt=0,
                                    mC_Rt=0,sC_Rt=0,pC_Rt=0,Eco_Rt=0)
for(i in 1:length(boot.nums)){
  tau.temp <- tau.fxn(Nt=sevblk.Nt, 
                      moist=sevblk.moist, 
                      temp=sevblk.temp,
                      mscut=sevblk.all$mscut[boot.nums[i]], 
                      Q10=sevblk.all$Q10[boot.nums[i]])
  
  sevblk.Rt <- Res_time(tau.=tau.temp[-1], 
                        c_new=as.numeric(sevblk.all[boot.nums[i],3:8]), 
                        f_new=as.numeric(sevblk.all[boot.nums[i],9:15]), 
                        b=sevblk.b)
  sevblk.Res.times.boot[i,2:ncol(sevblk.Res.times.boot)] <- colMeans(sevblk.Rt[2:ncol(sevblk.Rt)])
}

hist(subset(sevblk.Res.times.boot, Eco_Rt<100)$Eco_Rt, breaks=30)
sevblk.hats<-fitdistr(subset(sevblk.Res.times.boot,Eco_Rt<100)$Eco_Rt, "lognormal")
#sevblk.hats<-fitdistr(sevblk.Res.times.boot$Eco_Rt, "lognormal")
sevblk.ci <- qlnorm(c(0.025,0.05,0.25,0.5,0.75,0.95,0.975),meanlog=sevblk.hats$estimate[1],sdlog=sevblk.hats$estimate[2])
write.csv(sevblk.Res.times.boot, file="Residence times\\sevblk bootstrapped residence times 1000 runs.csv",row.names=FALSE)
sevblk.Res.times.boot <- read.csv("Residence times\\sevblk bootstrapped residence times 1000 runs.csv")

## sgs ##
boot.nums <- sample(x=1:nrow(sgs.all), size=1000, replace=TRUE)
sgs.Res.times.boot <- data.frame(run=1:length(boot.nums),lf_Rt=0,rt_Rt=0,lt_Rt=0,
                                    mC_Rt=0,sC_Rt=0,pC_Rt=0,Eco_Rt=0)
for(i in 1:length(boot.nums)){
  tau.temp <- tau.fxn(Nt=sgs.Nt, 
                      moist=sgs.moist, 
                      temp=sgs.temp,
                      mscut=sgs.all$mscut[boot.nums[i]], 
                      Q10=sgs.all$Q10[boot.nums[i]])
  
  sgs.Rt <- Res_time(tau.=tau.temp[-1], 
                        c_new=as.numeric(sgs.all[boot.nums[i],3:8]), 
                        f_new=as.numeric(sgs.all[boot.nums[i],9:15]), 
                        b=sgs.b)
  sgs.Res.times.boot[i,2:ncol(sgs.Res.times.boot)] <- colMeans(sgs.Rt[2:ncol(sgs.Rt)])
}

hist(subset(sgs.Res.times.boot, Eco_Rt<150)$Eco_Rt, breaks=30)
#sgs.hats<-fitdistr(subset(sgs.Res.times.boot,Eco_Rt<100)$Eco_Rt, "lognormal")
sgs.hats<-fitdistr(sgs.Res.times.boot$Eco_Rt, "lognormal")
sgs.ci <- qlnorm(c(0.025,0.05,0.25,0.5,0.75,0.95,0.975),meanlog=sgs.hats$estimate[1],sdlog=sgs.hats$estimate[2])
write.csv(sgs.Res.times.boot, file="Residence times\\sgs bootstrapped residence times 1000 runs.csv",row.names=FALSE)
sgs.Res.times.boot <- read.csv("Residence times\\sgs bootstrapped residence times 1000 runs.csv")

## chy ##
chy.Res.times.boot <- data.frame(run=1:length(boot.nums),lf_Rt=0,rt_Rt=0,lt_Rt=0,
                                    mC_Rt=0,sC_Rt=0,pC_Rt=0,Eco_Rt=0)
pb = txtProgressBar(min = 0, max = length(boot.nums), initial = 0) 
for(i in 1:length(boot.nums)){
    setTxtProgressBar(pb,i)
    tau.temp <- tau.fxn(Nt=chy.Nt, 
                      moist=chy.moist, 
                      temp=chy.temp,
                      mscut=chy.all$mscut[boot.nums[i]], 
                      Q10=chy.all$Q10[boot.nums[i]])
  
  chy.Rt <- Res_time(tau.=tau.temp[-1], 
                        c_new=as.numeric(chy.all[boot.nums[i],3:8]), 
                        f_new=as.numeric(chy.all[boot.nums[i],9:15]), 
                        b=chy.b)
  chy.Res.times.boot[i,2:ncol(chy.Res.times.boot)] <- colMeans(chy.Rt[2:ncol(chy.Rt)])
}

hist(subset(chy.Res.times.boot, Eco_Rt<100)$Eco_Rt, breaks=30)
#chy.hats<-fitdistr(subset(chy.Res.times.boot,Eco_Rt<100)$Eco_Rt, "lognormal")
chy.hats<-fitdistr(chy.Res.times.boot$Eco_Rt, "lognormal")
chy.ci <- qlnorm(c(0.025,0.05,0.25,0.5,0.75,0.95,0.975),meanlog=chy.hats$estimate[1],sdlog=chy.hats$estimate[2])
write.csv(chy.Res.times.boot, file="Residence times\\chy bootstrapped residence times 1000 runs.csv",row.names=FALSE)
chy.Res.times.boot <- read.csv("Residence times\\chy bootstrapped residence times 1000 runs.csv")

## hys ##
hys.Res.times.boot <- data.frame(run=1:length(boot.nums),lf_Rt=0,rt_Rt=0,lt_Rt=0,
                                    mC_Rt=0,sC_Rt=0,pC_Rt=0,Eco_Rt=0)
pb = txtProgressBar(min = 0, max = length(boot.nums), initial = 0) 
for(i in 1:length(boot.nums)){
  setTxtProgressBar(pb,i)
  tau.temp <- tau.fxn(Nt=hys.Nt, 
                      moist=hys.moist, 
                      temp=hys.temp,
                      mscut=hys.all$mscut[boot.nums[i]], 
                      Q10=hys.all$Q10[boot.nums[i]])
  
  hys.Rt <- Res_time(tau.=tau.temp[-1], 
                        c_new=as.numeric(hys.all[boot.nums[i],3:8]), 
                        f_new=as.numeric(hys.all[boot.nums[i],9:15]), 
                        b=hys.b)
  hys.Res.times.boot[i,2:ncol(hys.Res.times.boot)] <- colMeans(hys.Rt[2:ncol(hys.Rt)])
}

hist(subset(hys.Res.times.boot, Eco_Rt<100)$Eco_Rt, breaks=30)
#hys.hats<-fitdistr(subset(hys.Res.times.boot,Eco_Rt<100)$Eco_Rt, "lognormal")
hys.hats<-fitdistr(hys.Res.times.boot$Eco_Rt, "lognormal")
hys.ci <- qlnorm(c(0.025,0.05,0.25,0.5,0.75,0.95,0.975),meanlog=hys.hats$estimate[1],sdlog=hys.hats$estimate[2])
write.csv(hys.Res.times.boot, file="Residence times\\hys bootstrapped residence times 1000 runs.csv",row.names=FALSE)
hys.Res.times.boot <- read.csv("Residence times\\hys bootstrapped residence times 1000 runs.csv")

## knz ##
knz.Res.times.boot <- data.frame(run=1:length(boot.nums),lf_Rt=0,rt_Rt=0,lt_Rt=0,
                                    mC_Rt=0,sC_Rt=0,pC_Rt=0,Eco_Rt=0)
pb = txtProgressBar(min = 0, max = length(boot.nums), initial = 0) 
for(i in 1:length(boot.nums)){
  setTxtProgressBar(pb,i)
  tau.temp <- tau.fxn(Nt=knz.Nt, 
                      moist=knz.moist, 
                      temp=knz.temp,
                      mscut=knz.all$mscut[boot.nums[i]], 
                      Q10=knz.all$Q10[boot.nums[i]])
  
  knz.Rt <- Res_time(tau.=tau.temp[-1], 
                        c_new=as.numeric(knz.all[boot.nums[i],3:8]), 
                        f_new=as.numeric(knz.all[boot.nums[i],9:15]), 
                        b=knz.b)
  knz.Res.times.boot[i,2:ncol(knz.Res.times.boot)] <- colMeans(knz.Rt[2:ncol(knz.Rt)])
}

hist(subset(knz.Res.times.boot, Eco_Rt<100)$Eco_Rt, breaks=30)
#knz.hats<-fitdistr(subset(knz.Res.times.boot,Eco_Rt<100)$Eco_Rt, "lognormal")
knz.hats<-fitdistr(knz.Res.times.boot$Eco_Rt, "lognormal")
knz.ci <- qlnorm(c(0.025,0.05,0.25,0.5,0.75,0.95,0.975),meanlog=knz.hats$estimate[1],sdlog=knz.hats$estimate[2])
write.csv(knz.Res.times.boot, file="Residence times\\knz bootstrapped residence times 1000 runs.csv",row.names=FALSE)
knz.Res.times.boot <- read.csv("Residence times\\knz bootstrapped residence times 1000 runs.csv")

ci.all <- as.data.frame(rbind(sgs.ci,
                                rbind(chy.ci,
                                      rbind(hys.ci,knz.ci))))
colnames(ci.all) <- c("q2.5","q5","q25","mean","q75","q95","q97.25")
write.csv(ci.all, file="Residence times\\mean and ci from 1000 bootstrap runs.csv",row.names=F)

# Calculate standard deviation from bootstrap runs ------------------------

sevblu.Rt.raw <- read.csv("Residence times\\sevblu bootstrapped residence times 1000 runs.csv")
sevblk.Rt.raw <- read.csv("Residence times\\sevblk bootstrapped residence times 1000 runs.csv")
sgs.Rt.raw <- read.csv("Residence times\\sgs bootstrapped residence times 1000 runs.csv")
chy.Rt.raw <- read.csv("Residence times\\chy bootstrapped residence times 1000 runs.csv")
hys.Rt.raw <- read.csv("Residence times\\hys bootstrapped residence times 1000 runs.csv")
knz.Rt.raw <- read.csv("Residence times\\knz bootstrapped residence times 1000 runs.csv")

sevblu.Rt <- with(sevblu.Rt.raw, data.frame(site="a.sevblu", run=run, Eco_Rt=Eco_Rt))
sevblk.Rt <- with(sevblk.Rt.raw, data.frame(site="b.sevblk", run=run, Eco_Rt=Eco_Rt))
sgs.Rt <- with(sgs.Rt.raw, data.frame(site="c.sgs", run=run, Eco_Rt=Eco_Rt))
chy.Rt <- with(chy.Rt.raw, data.frame(site="d.chy", run=run, Eco_Rt=Eco_Rt))
hys.Rt <- with(hys.Rt.raw, data.frame(site="e.hys", run=run, Eco_Rt=Eco_Rt))
knz.Rt <- with(knz.Rt.raw, data.frame(site="f.knz", run=run, Eco_Rt=Eco_Rt))

all.Rt <- rbind(sevblu.Rt, sevblk.Rt, sgs.Rt, chy.Rt, hys.Rt, knz.Rt)

Rt.sd <- ddply(subset(all.Rt, Eco_Rt<150), .(site), summarize,
                    Rt.sd = sd(Eco_Rt))

ci.all <- read.csv("Residence times\\mean and ci from 1000 bootstrap runs.csv")
ci.all.sd <- merge(ci.all, Rt.sd, by.x="Site", by.y="site", all=T)

write.csv(ci.all.sd, file="Residence times\\mean and ci from 1000 bootstrap runs_with sd_July2017.csv",row.names=F)





