### Data read in and prep ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com) ###
### Last updated: Feb. 22, 2017 ###

# Workspace setup ------------------------------------------------------------------

rm(list=ls())
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(stats4)
library(coda)
library(ggthemes)
library(MASS)
require(gridExtra)
setwd("C:\\Users\\Kevin.Wilcox\\Desktop\\fromOldComp\\FULL_ANALYSES_July2017\\DA chains\\")


# Read in and prep data ---------------------------------------------------

sevblu_read_path <- "sevblu\\Feb22\\"
sevblk_read_path <- "sevblk\\Feb22\\"
sgs_read_path <- "sgs\\Feb22\\"
chy_read_path <- "chy\\Feb22\\"
hys_read_path <- "hys\\Feb22\\"
knz_read_path <- "knz\\Feb22\\"

### sevblu all parameters (raw file already has all parameters) ###
#sevblu.all <- read.csv("sevblu\\sevblu_all params_360000 4chains_13-15.csv")

### C turnover parameters ###
### Full sets ###
sevblu.c <- read.csv(paste0(sevblu_read_path,"sevblu_c params_360000 4chains_13-15.csv"))
sevblk.c <- read.csv(paste0(sevblk_read_path,"sevblk_c params_360000 4chains_13-15.csv"))
sgs.c <- read.csv(paste0(sgs_read_path,"sgs_c params_360000 4chains_13-15.csv"))
chy.c <- read.csv(paste0(chy_read_path,"chy_c params_360000 4chains_13-15.csv"))
hys.c <- read.csv(paste0(hys_read_path,"hys_c params_360000 4chains_13-15.csv"))
knz.c <- read.csv(paste0(knz_read_path,"knz_c params_360000 4chains_13-15.csv"))

c.names <- c("sim"   ,   "chain" ,   "c1"    ,   "c2"  ,     "c3"   ,    "c4"   ,    "c5"   ,    "c6"   ,    "J_record")

colnames(sevblu.c) <- c.names
colnames(sevblk.c) <- c.names
colnames(sgs.c) <- c.names
colnames(chy.c) <- c.names
colnames(hys.c) <- c.names
colnames(knz.c) <- c.names

## Remove first 20,000 chains ##
sevblu.c2 <- subset(sevblu.c, sim >= 20000)
sevblk.c2 <- subset(sevblk.c, sim >= 20000)
sgs.c2 <- subset(sgs.c, sim >= 20000)
chy.c2 <- subset(chy.c, sim >= 20000)
hys.c2 <- subset(hys.c, sim >= 20000)
knz.c2 <- subset(knz.c, sim >= 20000)

sevblu.c2$site <- "a.sevblu"
sevblk.c2$site <- "b.sevblk"
sgs.c2$site <- "c.sgs"
chy.c2$site <- "d.chy"
hys.c2$site <- "e.hys"
knz.c2$site <- "f.knz"

### f's - C transfer parameters ###
### Full sets ###
sevblu.f <- read.csv(paste0(sevblu_read_path,"sevblu_f params_360000 4chains_13-15.csv"))
sevblk.f <- read.csv(paste0(sevblk_read_path,"sevblk_f params_360000 4chains_13-15.csv"))
sgs.f <- read.csv(paste0(sgs_read_path,"sgs_f params_360000 4chains_13-15.csv"))
chy.f <- read.csv(paste0(chy_read_path,"chy_f params_360000 4chains_13-15.csv"))
hys.f <- read.csv(paste0(hys_read_path,"hys_f params_360000 4chains_13-15.csv"))
knz.f <- read.csv(paste0(knz_read_path,"knz_f params_360000 4chains_13-15.csv"))

f.names <- c("sim"   ,   "chain" ,   "f1"    ,   "f2"  ,     "f3"   ,    "f4"   ,    "f5"   ,    "f6"   , "f7")

colnames(sevblu.f) <- f.names
colnames(sevblk.f) <- f.names
colnames(sgs.f) <- f.names
colnames(chy.f) <- f.names
colnames(hys.f) <- f.names
colnames(knz.f) <- f.names

## Remove first 20,000 chains ##
sevblu.f2 <- subset(sevblu.f, sim >= 20000)
sevblk.f2 <- subset(sevblk.f, sim >= 20000)
sgs.f2 <- subset(sgs.f, sim >= 20000)
chy.f2 <- subset(chy.f, sim >= 20000)
hys.f2 <- subset(hys.f, sim >= 20000)
knz.f2 <- subset(knz.f, sim >= 20000)

sevblu.f2$site <- "a.sevblu"
sevblk.f2$site <- "b.sevblk"
sgs.f2$site <- "c.sgs"
chy.f2$site <- "d.chy"
hys.f2$site <- "e.hys"
knz.f2$site <- "f.knz"

### mscut parameters ###
sevblu.mscut <- read.csv(paste0(sevblu_read_path,"sevblu_mscut params_360000 4chains_13-15.csv"))
sevblk.mscut <- read.csv(paste0(sevblk_read_path,"sevblk_mscut params_360000 4chains_13-15.csv"))
sgs.mscut <- read.csv(paste0(sgs_read_path,"sgs_mscut params_360000 4chains_13-15.csv"))
chy.mscut <- read.csv(paste0(chy_read_path,"chy_mscut params_360000 4chains_13-15.csv"))
hys.mscut <- read.csv(paste0(hys_read_path,"hys_mscut params_360000 4chains_13-15.csv"))
knz.mscut <- read.csv(paste0(knz_read_path,"knz_mscut params_360000 4chains_13-15.csv"))

sevblu.mscut2 <- subset(sevblu.mscut, sim >= 20000)
sevblk.mscut2 <- subset(sevblk.mscut, sim >= 20000)
sgs.mscut2 <- subset(sgs.mscut, sim >= 20000)
chy.mscut2 <- subset(chy.mscut, sim >= 20000) 
hys.mscut2 <- subset(hys.mscut, sim >= 20000)
knz.mscut2 <- subset(knz.mscut, sim >= 20000)

sevblu.mscut2$site <- "a.sevblu"
sevblk.mscut2$site <- "b.sevblk"
sgs.mscut2$site <- "c.sgs"
chy.mscut2$site <- "d.chy"
hys.mscut2$site <- "e.hys"
knz.mscut2$site <- "f.knz"

### Q10 ###
sevblu.Q10 <- read.csv(paste0(sevblu_read_path,"sevblu_Q10 params_360000 4chains_13-15.csv"))
sevblk.Q10 <- read.csv(paste0(sevblk_read_path,"sevblk_Q10 params_360000 4chains_13-15.csv"))
sgs.Q10 <- read.csv(paste0(sgs_read_path,"sgs_Q10 params_360000 4chains_13-15.csv"))
chy.Q10 <- read.csv(paste0(chy_read_path,"chy_Q10 params_360000 4chains_13-15.csv"))
hys.Q10 <- read.csv(paste0(hys_read_path,"hys_Q10 params_360000 4chains_13-15.csv"))
knz.Q10 <- read.csv(paste0(knz_read_path,"knz_Q10 params_360000 4chains_13-15.csv"))

# Trim off beginning simulations
sevblu.Q102 <- subset(sevblu.Q10, sim >= 20000)
sevblk.Q102 <- subset(sevblk.Q10, sim >= 20000)
sgs.Q102 <- subset(sgs.Q10, sim >= 20000)
chy.Q102 <- subset(chy.Q10, sim >= 20000)
hys.Q102 <- subset(hys.Q10, sim >= 20000)
knz.Q102 <- subset(knz.Q10, sim >= 20000)

sevblu.Q102$site <- "a.sevblu"
sevblk.Q102$site <- "b.sevblk"
sgs.Q102$site <- "c.sgs"
chy.Q102$site <- "d.chy"
hys.Q102$site <- "e.hys"
knz.Q102$site <- "f.knz"


# Set up mcmc objects  ---------------------------------------------------------
## sevblu ##
#sevblu.all.mcmc <- mcmc.list(as.mcmc(subset(sevblu.all,chain==1)[,3:17]),as.mcmc(subset(sevblu.all,chain==2)[,3:17]),as.mcmc(subset(sevblu.all,chain==3)[,3:17]),as.mcmc(subset(sevblu.all,chain==4)[,3:17]))
sevblu.all <- data.frame(sevblu.c2[,1:8], 
                         sevblu.f2[,3:9],
                         mscut=sevblu.mscut2[,3],
                         Q10=sevblu.Q102[,3])
sevblu.all.mcmc <- mcmc.list(as.mcmc(subset(sevblu.all,chain==1)[,3:17]),as.mcmc(subset(sevblu.all,chain==2)[,3:17]),as.mcmc(subset(sevblu.all,chain==3)[,3:17]),as.mcmc(subset(sevblu.all,chain==4)[,3:17]))

## sevblk ##
sevblk.all <- data.frame(sevblk.c2[,1:8], 
                         sevblk.f2[,3:9],
                         mscut=sevblk.mscut2[,3],
                         Q10=sevblk.Q102[,3])
sevblk.all.mcmc <- mcmc.list(as.mcmc(subset(sevblk.all,chain==1)[,3:17]),as.mcmc(subset(sevblk.all,chain==2)[,3:17]),as.mcmc(subset(sevblk.all,chain==3)[,3:17]),as.mcmc(subset(sevblk.all,chain==4)[,3:17]))

## sgs ##
sgs.all <- data.frame(sgs.c2[,1:8], 
                      sgs.f2[,3:9],
                      mscut=sgs.mscut2[,3],
                      Q10=sgs.Q102[,3])
sgs.all.mcmc <- mcmc.list(as.mcmc(subset(sgs.all,chain==1)[,3:17]),as.mcmc(subset(sgs.all,chain==2)[,3:17]),as.mcmc(subset(sgs.all,chain==3)[,3:17]),as.mcmc(subset(sgs.all,chain==4)[,3:17]))

## chy ##
chy.all <- data.frame(chy.c2[,1:8], 
                      chy.f2[,3:9],
                      mscut=chy.mscut2[,3],
                      Q10=chy.Q102[,3])
chy.all.mcmc <- mcmc.list(as.mcmc(subset(chy.all,chain==1)[,3:17]),as.mcmc(subset(chy.all,chain==2)[,3:17]),as.mcmc(subset(chy.all,chain==3)[,3:17]),as.mcmc(subset(chy.all,chain==4)[,3:17]))

## hys ##
hys.all <- data.frame(hys.c2[,1:8], 
                      hys.f2[,3:9],
                      mscut=hys.mscut2[,3],
                      Q10=hys.Q102[,3])
hys.all.mcmc <- mcmc.list(as.mcmc(subset(hys.all,chain==1)[,3:17]),as.mcmc(subset(hys.all,chain==2)[,3:17]),as.mcmc(subset(hys.all,chain==3)[,3:17]),as.mcmc(subset(hys.all,chain==4)[,3:17]))

## knz ##
knz.all <- data.frame(knz.c2[,1:8], 
                      knz.f2[,3:9],
                      mscut=knz.mscut2[,3],
                      Q10=knz.Q102[,3])
knz.all.mcmc <- mcmc.list(as.mcmc(subset(knz.all,chain==1)[,3:17]),as.mcmc(subset(knz.all,chain==2)[,3:17]),as.mcmc(subset(knz.all,chain==3)[,3:17]),as.mcmc(subset(knz.all,chain==4)[,3:17]))




#### sgs only #####

sgs.c <- read.csv("sgs\\sgs_c params_360000 4chains_13-15_7e-5 c5v2 min.csv")
c.names <- c("sim"   ,   "chain" ,   "c1"    ,   "c2"  ,     "c3"   ,    "c4"   ,    "c5"   ,    "c6"   ,    "J_record")
colnames(sgs.c) <- c.names
sgs.c2 <- subset(sgs.c, sim >= 20000)
sgs.c2$site <- "c.sgs"

sgs.f <- read.csv("sgs\\sgs_f params_360000 4chains_13-15_7e-5 c5v2 min.csv")
f.names <- c("sim"   ,   "chain" ,   "f1"    ,   "f2"  ,     "f3"   ,    "f4"   ,    "f5"   ,    "f6"   , "f7")
colnames(sgs.f) <- f.names
sgs.f2 <- subset(sgs.f, sim >= 20000)
sgs.f2$site <- "c.sgs"

sgs.mscut <- read.csv("sgs\\sgs_mscut params_360000 4chains_13-15_7e-5 c5v2 min.csv")
sgs.mscut2 <- subset(sgs.mscut, sim >= 20000)
sgs.mscut2$site <- "c.sgs"

sgs.Q10 <- read.csv("sgs\\sgs_Q10 params_360000 4chains_13-15_7e-5 c5v2 min.csv")
sgs.Q102 <- subset(sgs.Q10, sim >= 20000)
sgs.Q102$site <- "c.sgs"

sgs.all <- data.frame(sgs.c2[,1:8],
                      sgs.f2[,3:9],
                      mscut=sgs.mscut2[,3],
                      Q10=sgs.Q102[,3])
sgs.all.mcmc <- mcmc.list(as.mcmc(subset(sgs.all,chain==1)[,3:17]),
                          as.mcmc(subset(sgs.all,chain==2)[,3:17]))#,
#                          as.mcmc(subset(sgs.all,chain==3)[,3:17]),
#                          as.mcmc(subset(sgs.all,chain==4)[,3:17]))

sgs.all.sub <- rbind(sample_n(subset(sgs.all, chain==1), 10000),
                     sample_n(subset(sgs.all, chain==2), 10000))
#                     sample_n(subset(sgs.all, chain==3), 10000))#,
#                     sample_n(subset(sgs.all, chain==4), 10000))

ggplot(sgs.all.sub, aes(x=c1))+geom_density() + xlim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(sgs.all.sub, aes(x=c2))+geom_density() + xlim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(sgs.all.sub, aes(x=c3))+geom_density() + xlim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(sgs.all.sub, aes(x=c4))+geom_density() + xlim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(sgs.all.sub, aes(x=c5))+geom_density() + xlim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(sgs.all.sub, aes(x=c6))+geom_density() + xlim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
f1<-ggplot(sgs.all.sub, aes(x=f1))+geom_density() + xlim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f2<-ggplot(sgs.all.sub, aes(x=f2))+geom_density() + xlim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f3<-ggplot(sgs.all.sub, aes(x=f3))+geom_density() + xlim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f4<-ggplot(sgs.all.sub, aes(x=f4))+geom_density() + xlim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f5<-ggplot(sgs.all.sub, aes(x=f5))+geom_density() + xlim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f6<-ggplot(sgs.all.sub, aes(x=f6))+geom_density() + xlim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f7<-ggplot(sgs.all.sub, aes(x=f7))+geom_density() + xlim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
ggplot(sgs.all.sub, aes(x=mscut))+geom_density() + xlim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(sgs.all.sub, aes(x=Q10))+geom_density() + xlim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

# trace plots #
ggplot(sgs.all, aes(x=sim, y=c1, colour=factor(chain)))+geom_line() + ylim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(sgs.all, aes(x=sim, y=c2, colour=factor(chain)))+geom_line() + ylim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(sgs.all, aes(x=sim, y=c3, colour=factor(chain)))+geom_line() + ylim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(sgs.all, aes(x=sim, y=c4, colour=factor(chain)))+geom_line() + ylim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(sgs.all, aes(x=sim, y=c5, colour=factor(chain)))+geom_line() + ylim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(sgs.all, aes(x=sim, y=c6, colour=factor(chain)))+geom_line() + ylim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

ggplot(sgs.all, aes(x=sim, y=mscut, colour=factor(chain)))+geom_line() + ylim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(sgs.all, aes(x=sim, y=Q10, colour=factor(chain)))+geom_line() + ylim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))



#bootstrap res time #
boot.nums <- sample(x=1:nrow(sgs.all), size=100, replace=TRUE)
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

hist(subset(sgs.Res.times.boot, Eco_Rt<100)$Eco_Rt, breaks=30)
sgs.hats<-fitdistr(subset(sgs.Res.times.boot,Eco_Rt<100)$Eco_Rt, "lognormal")
#sgs.hats<-fitdistr(sgs.Res.times.boot$Eco_Rt, "lognormal")
sgs.ci <- qlnorm(c(0.025,0.05,0.25,0.5,0.75,0.95,0.975),meanlog=sgs.hats$estimate[1],sdlog=sgs.hats$estimate[2])
write.csv(sgs.Res.times.boot, file="Residence times\\sgs bootstrapped residence times 1000 runs.csv",row.names=FALSE)
sgs.Res.times.boot <- read.csv("Residence times\\sgs bootstrapped residence times 1000 runs.csv")



#### hys only #####

hys.c <- read.csv("hys\\Feb22\\hys_c params_360000 4chains_13-15.csv")
c.names <- c("sim"   ,   "chain" ,   "c1"    ,   "c2"  ,     "c3"   ,    "c4"   ,    "c5"   ,    "c6"   ,    "J_record")
colnames(hys.c) <- c.names
hys.c2 <- subset(hys.c, sim >= 20000)
hys.c2$site <- "c.hys"

hys.f <- read.csv("hys\\Feb22\\hys_f params_360000 4chains_13-15.csv")
f.names <- c("sim"   ,   "chain" ,   "f1"    ,   "f2"  ,     "f3"   ,    "f4"   ,    "f5"   ,    "f6"   , "f7")
colnames(hys.f) <- f.names
hys.f2 <- subset(hys.f, sim >= 20000)
hys.f2$site <- "c.hys"

hys.mscut <- read.csv("hys\\Feb22\\hys_mscut params_360000 4chains_13-15.csv")
hys.mscut2 <- subset(hys.mscut, sim >= 20000)
hys.mscut2$site <- "c.hys"

hys.Q10 <- read.csv("hys\\Feb22\\hys_Q10 params_360000 4chains_13-15.csv")
hys.Q102 <- subset(hys.Q10, sim >= 20000)
hys.Q102$site <- "c.hys"

hys.all <- data.frame(hys.c2[,1:8],
                      hys.f2[,3:9],
                      mscut=hys.mscut2[,3],
                      Q10=hys.Q102[,3])
hys.all.mcmc <- mcmc.list(as.mcmc(subset(hys.all,chain==1)[,3:17]),
                          as.mcmc(subset(hys.all,chain==2)[,3:17]))#,
#                          as.mcmc(subset(hys.all,chain==3)[,3:17]),
#                          as.mcmc(subset(hys.all,chain==4)[,3:17]))

hys.all.sub <- rbind(sample_n(subset(hys.all, chain==1), 10000),
                     sample_n(subset(hys.all, chain==2), 10000))
#                     sample_n(subset(hys.all, chain==3), 10000))#,
#                     sample_n(subset(hys.all, chain==4), 10000))

ggplot(hys.all.sub, aes(x=c1))+geom_density() + xlim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(hys.all.sub, aes(x=c2))+geom_density() + xlim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(hys.all.sub, aes(x=c3))+geom_density() + xlim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(hys.all.sub, aes(x=c4))+geom_density() + xlim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(hys.all.sub, aes(x=c5))+geom_density() + xlim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(hys.all.sub, aes(x=c6))+geom_density() + xlim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
f1<-ggplot(hys.all.sub, aes(x=f1))+geom_density() + xlim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f2<-ggplot(hys.all.sub, aes(x=f2))+geom_density() + xlim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f3<-ggplot(hys.all.sub, aes(x=f3))+geom_density() + xlim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f4<-ggplot(hys.all.sub, aes(x=f4))+geom_density() + xlim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f5<-ggplot(hys.all.sub, aes(x=f5))+geom_density() + xlim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f6<-ggplot(hys.all.sub, aes(x=f6))+geom_density() + xlim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f7<-ggplot(hys.all.sub, aes(x=f7))+geom_density() + xlim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
ggplot(hys.all.sub, aes(x=mscut))+geom_density() + xlim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(hys.all.sub, aes(x=Q10))+geom_density() + xlim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

# trace plots #
ggplot(hys.all, aes(x=sim, y=c1, colour=factor(chain)))+geom_line() + ylim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(hys.all, aes(x=sim, y=c2, colour=factor(chain)))+geom_line() + ylim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(hys.all, aes(x=sim, y=c3, colour=factor(chain)))+geom_line() + ylim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(hys.all, aes(x=sim, y=c4, colour=factor(chain)))+geom_line() + ylim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(hys.all, aes(x=sim, y=c5, colour=factor(chain)))+geom_line() + ylim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(hys.all, aes(x=sim, y=c6, colour=factor(chain)))+geom_line() + ylim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

ggplot(hys.all, aes(x=sim, y=mscut, colour=factor(chain)))+geom_line() + ylim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
ggplot(hys.all, aes(x=sim, y=Q10, colour=factor(chain)))+geom_line() + ylim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))



#bootstrap res time #
boot.nums <- sample(x=1:nrow(hys.all), size=100, replace=TRUE)
hys.Res.times.boot <- data.frame(run=1:length(boot.nums),lf_Rt=0,rt_Rt=0,lt_Rt=0,
                                 mC_Rt=0,sC_Rt=0,pC_Rt=0,Eco_Rt=0)
for(i in 1:length(boot.nums)){
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
hys.hats<-fitdistr(subset(hys.Res.times.boot,Eco_Rt<100)$Eco_Rt, "lognormal")
#hys.hats<-fitdistr(hys.Res.times.boot$Eco_Rt, "lognormal")
hys.ci <- qlnorm(c(0.025,0.05,0.25,0.5,0.75,0.95,0.975),meanlog=hys.hats$estimate[1],sdlog=hys.hats$estimate[2])
#write.csv(hys.Res.times.boot, file="Residence times\\hys bootstrapped residence times 1000 runs.csv",row.names=FALSE)
hys.Res.times.boot <- read.csv("Residence times\\hys bootstrapped residence times 1000 runs.csv")













