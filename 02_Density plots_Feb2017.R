### Density figures for parameters from data assimilation ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com) ###
### Last updated: July 23rd, 2017 ###

rm(list=ls())

## Read in and format data ##
source("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_July2017\\Scripts\\00_Data read in and prep.R")

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
setwd( "C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_July2017\\Figures\\")


# Set max and min c turnover rates for plotting limits --------------------
cmin <- numeric(6)
cmin[1] <- 1.0e-4 
cmin[2] <- 1.0e-4 
cmin[3] <- 5.0e-4 
cmin[4] <- 5.0e-3 
cmin[5] <- 1.0e-4 
cmin[6] <- 1.0e-8

# Set maximum limits of c flux ranges #
cmax <- numeric(6)
cmax[1] <- 1.0e-2
cmax[2] <- 1.0e-2 
cmax[3] <- 2.0e-2 
cmax[4] <- 5.0e-2 
cmax[5] <- 2.0e-3 
cmax[6] <- 3.0e-5

fmin <- numeric(7)
fmin[1] <- 3.0e-1 
fmin[2] <- 5.0e-2 
fmin[3] <- 2.5e-1 
fmin[4] <- 1.0e-3 
fmin[5] <- 1.0e-1
fmin[6] <- 0.2e-2 
fmin[7] <- 3.0e-1
# Max C transfer #
fmax <- numeric(7)
fmax[1] <- 7.0e-1 
fmax[2] <- 1.5e-1 
fmax[3] <- 6.5e-1  
fmax[4] <- 8.0e-3 
fmax[5] <- 6.0e-1 
fmax[6] <- 7.0e-2 
fmax[7] <- 7.0e-1
## soil moisture cutoff max and min ##
mscut_min <- .01 
mscut_max <- .40
## soil temperature tau modifier Q10 -- max and min ##
Q10_min <- 1.0
Q10_max <- 4.0

  

# Subsample for easier computing ------------------------------------------
#sevblu.all.sub <- rbind(sample_n(subset(sevblu.all, chain==1), 10000),
#                        sample_n(subset(sevblu.all, chain==2), 10000),
#                        sample_n(subset(sevblu.all, chain==3), 10000),
#                        sample_n(subset(sevblu.all, chain==4), 10000))

#sevblk.all.sub <- rbind(sample_n(subset(sevblk.all, chain==1), 10000),
#                        sample_n(subset(sevblk.all, chain==2), 10000),
#                        sample_n(subset(sevblk.all, chain==3), 10000),
#                        sample_n(subset(sevblk.all, chain==4), 10000))

sgs.all.sub <- rbind(sample_n(subset(sgs.all, chain==1), 10000),
                     sample_n(subset(sgs.all, chain==2), 10000),
                     sample_n(subset(sgs.all, chain==3), 10000))

chy.all.sub <- rbind(sample_n(subset(chy.all, chain==1), 10000),
                     sample_n(subset(chy.all, chain==3), 10000),
                     sample_n(subset(chy.all, chain==4), 10000))

hys.all.sub <- rbind(sample_n(subset(hys.all, chain==1), 10000),
                     sample_n(subset(hys.all, chain==2), 10000),
                     sample_n(subset(hys.all, chain==3), 10000))

knz.all.sub <- rbind(sample_n(subset(knz.all, chain==1), 10000),
                     sample_n(subset(knz.all, chain==2), 10000),
                     sample_n(subset(knz.all, chain==3), 10000))



# Density Plots  -----------------------------------------------------

## sevblu no standing crop root update to sev sites, see FULL ANALYSIS_Feb2017 for sev code##
# c #
#c1<-ggplot(sevblu.all.sub, aes(x=c1))+geom_density() + xlim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#c2<-ggplot(sevblu.all.sub, aes(x=c2))+geom_density() + xlim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#c3<-ggplot(sevblu.all.sub, aes(x=c3))+geom_density() + xlim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#c4<-ggplot(sevblu.all.sub, aes(x=c4))+geom_density() + xlim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#c5<-ggplot(sevblu.all.sub, aes(x=c5))+geom_density() + xlim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#c6<-ggplot(sevblu.all.sub, aes(x=c6))+geom_density() + xlim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
#f1<-ggplot(sevblu.all.sub, aes(x=f1))+geom_density() + xlim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#f2<-ggplot(sevblu.all.sub, aes(x=f2))+geom_density() + xlim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#f3<-ggplot(sevblu.all.sub, aes(x=f3))+geom_density() + xlim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#f4<-ggplot(sevblu.all.sub, aes(x=f4))+geom_density() + xlim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#f5<-ggplot(sevblu.all.sub, aes(x=f5))+geom_density() + xlim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#f6<-ggplot(sevblu.all.sub, aes(x=f6))+geom_density() + xlim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#f7<-ggplot(sevblu.all.sub, aes(x=f7))+geom_density() + xlim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
#mscut.<-ggplot(sevblu.all.sub, aes(x=mscut))+geom_density() + xlim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#Q10.<-ggplot(sevblu.all.sub, aes(x=Q10))+geom_density() + xlim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

#ggsave(filename="Density panels\\density_sevblu_c1.png",plot=c1,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_c2.png",plot=c2,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_c3.png",plot=c3,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_c4.png",plot=c4,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_c5.png",plot=c5,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_c6.png",plot=c6,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_f1.png",plot=f1,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_f2.png",plot=f2,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_f3.png",plot=f3,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_f4.png",plot=f4,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_f5.png",plot=f5,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_f6.png",plot=f6,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_f7.png",plot=f7,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_mscut.png",plot=mscut.,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblu_Q10.png",plot=Q10.,width=2.0,height=1.5,units="in",dpi=300)
#rm(c1,c2,c3,c4,c5,c6,f1,f2,f3,f4,f5,f6,f7,mscut.,Q10.)

## sevblk - no standing crop root update to sev sites, see FULL ANALYSIS_Feb2017 for sev code##
# c #
#c1<-ggplot(sevblk.all.sub, aes(x=c1))+geom_density() + xlim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#c2<-ggplot(sevblk.all.sub, aes(x=c2))+geom_density() + xlim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#c3<-ggplot(sevblk.all.sub, aes(x=c3))+geom_density() + xlim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#c4<-ggplot(sevblk.all.sub, aes(x=c4))+geom_density() + xlim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#c5<-ggplot(sevblk.all.sub, aes(x=c5))+geom_density() + xlim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#c6<-ggplot(sevblk.all.sub, aes(x=c6))+geom_density() + xlim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
#f1<-ggplot(sevblk.all.sub, aes(x=f1))+geom_density() + xlim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#f2<-ggplot(sevblk.all.sub, aes(x=f2))+geom_density() + xlim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#f3<-ggplot(sevblk.all.sub, aes(x=f3))+geom_density() + xlim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#f4<-ggplot(sevblk.all.sub, aes(x=f4))+geom_density() + xlim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#f5<-ggplot(sevblk.all.sub, aes(x=f5))+geom_density() + xlim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#f6<-ggplot(sevblk.all.sub, aes(x=f6))+geom_density() + xlim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#f7<-ggplot(sevblk.all.sub, aes(x=f7))+geom_density() + xlim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
#mscut.<-ggplot(sevblk.all.sub, aes(x=mscut))+geom_density() + xlim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
#Q10.<-ggplot(sevblk.all.sub, aes(x=Q10))+geom_density() + xlim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

#ggsave(filename="Density panels\\density_sevblk_c1.png",plot=c1,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_c2.png",plot=c2,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_c3.png",plot=c3,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_c4.png",plot=c4,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_c5.png",plot=c5,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_c6.png",plot=c6,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_f1.png",plot=f1,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_f2.png",plot=f2,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_f3.png",plot=f3,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_f4.png",plot=f4,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_f5.png",plot=f5,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_f6.png",plot=f6,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_f7.png",plot=f7,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_mscut.png",plot=mscut.,width=2.0,height=1.5,units="in",dpi=300)
#ggsave(filename="Density panels\\density_sevblk_Q10.png",plot=Q10.,width=2.0,height=1.5,units="in",dpi=300)
#rm(c1,c2,c3,c4,c5,c6,f1,f2,f3,f4,f5,f6,f7,mscut.,Q10.)

## sgs ##
# c #
c1<-ggplot(sgs.all.sub, aes(x=c1))+geom_density() + xlim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c2<-ggplot(sgs.all.sub, aes(x=c2))+geom_density() + xlim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c3<-ggplot(sgs.all.sub, aes(x=c3))+geom_density() + xlim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c4<-ggplot(sgs.all.sub, aes(x=c4))+geom_density() + xlim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c5<-ggplot(sgs.all.sub, aes(x=c5))+geom_density() + xlim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c6<-ggplot(sgs.all.sub, aes(x=c6))+geom_density() + xlim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
f1<-ggplot(sgs.all.sub, aes(x=f1))+geom_density() + xlim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f2<-ggplot(sgs.all.sub, aes(x=f2))+geom_density() + xlim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f3<-ggplot(sgs.all.sub, aes(x=f3))+geom_density() + xlim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f4<-ggplot(sgs.all.sub, aes(x=f4))+geom_density() + xlim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f5<-ggplot(sgs.all.sub, aes(x=f5))+geom_density() + xlim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f6<-ggplot(sgs.all.sub, aes(x=f6))+geom_density() + xlim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f7<-ggplot(sgs.all.sub, aes(x=f7))+geom_density() + xlim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
mscut.<-ggplot(sgs.all.sub, aes(x=mscut))+geom_density() + xlim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
Q10.<-ggplot(sgs.all.sub, aes(x=Q10))+geom_density() + xlim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

ggsave(filename="Density panels\\density_sgs_c1.png",plot=c1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_c2.png",plot=c2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_c3.png",plot=c3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_c4.png",plot=c4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_c5.png",plot=c5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_c6.png",plot=c6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_f1.png",plot=f1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_f2.png",plot=f2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_f3.png",plot=f3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_f4.png",plot=f4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_f5.png",plot=f5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_f6.png",plot=f6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_f7.png",plot=f7,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_mscut.png",plot=mscut.,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_sgs_Q10.png",plot=Q10.,width=2.0,height=1.5,units="in",dpi=300)
rm(c1,c2,c3,c4,c5,c6,f1,f2,f3,f4,f5,f6,f7,mscut.,Q10.)


## chy ##
# c #
c1<-ggplot(chy.all.sub, aes(x=c1))+geom_density() + xlim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c2<-ggplot(chy.all.sub, aes(x=c2))+geom_density() + xlim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c3<-ggplot(chy.all.sub, aes(x=c3))+geom_density() + xlim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c4<-ggplot(chy.all.sub, aes(x=c4))+geom_density() + xlim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c5<-ggplot(chy.all.sub, aes(x=c5))+geom_density() + xlim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c6<-ggplot(chy.all.sub, aes(x=c6))+geom_density() + xlim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
f1<-ggplot(chy.all.sub, aes(x=f1))+geom_density() + xlim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f2<-ggplot(chy.all.sub, aes(x=f2))+geom_density() + xlim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f3<-ggplot(chy.all.sub, aes(x=f3))+geom_density() + xlim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f4<-ggplot(chy.all.sub, aes(x=f4))+geom_density() + xlim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f5<-ggplot(chy.all.sub, aes(x=f5))+geom_density() + xlim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f6<-ggplot(chy.all.sub, aes(x=f6))+geom_density() + xlim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f7<-ggplot(chy.all.sub, aes(x=f7))+geom_density() + xlim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
mscut.<-ggplot(chy.all.sub, aes(x=mscut))+geom_density() + xlim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
Q10.<-ggplot(chy.all.sub, aes(x=Q10))+geom_density() + xlim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

ggsave(filename="Density panels\\density_chy_c1.png",plot=c1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_c2.png",plot=c2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_c3.png",plot=c3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_c4.png",plot=c4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_c5.png",plot=c5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_c6.png",plot=c6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_f1.png",plot=f1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_f2.png",plot=f2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_f3.png",plot=f3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_f4.png",plot=f4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_f5.png",plot=f5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_f6.png",plot=f6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_f7.png",plot=f7,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_mscut.png",plot=mscut.,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_chy_Q10.png",plot=Q10.,width=2.0,height=1.5,units="in",dpi=300)
rm(c1,c2,c3,c4,c5,c6,f1,f2,f3,f4,f5,f6,f7,mscut.,Q10.)


## hys ##
# c #
c1<-ggplot(hys.all.sub, aes(x=c1))+geom_density() + xlim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c2<-ggplot(hys.all.sub, aes(x=c2))+geom_density() + xlim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c3<-ggplot(hys.all.sub, aes(x=c3))+geom_density() + xlim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c4<-ggplot(hys.all.sub, aes(x=c4))+geom_density() + xlim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c5<-ggplot(hys.all.sub, aes(x=c5))+geom_density() + xlim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c6<-ggplot(hys.all.sub, aes(x=c6))+geom_density() + xlim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
f1<-ggplot(hys.all.sub, aes(x=f1))+geom_density() + xlim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f2<-ggplot(hys.all.sub, aes(x=f2))+geom_density() + xlim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f3<-ggplot(hys.all.sub, aes(x=f3))+geom_density() + xlim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f4<-ggplot(hys.all.sub, aes(x=f4))+geom_density() + xlim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f5<-ggplot(hys.all.sub, aes(x=f5))+geom_density() + xlim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f6<-ggplot(hys.all.sub, aes(x=f6))+geom_density() + xlim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f7<-ggplot(hys.all.sub, aes(x=f7))+geom_density() + xlim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
mscut.<-ggplot(hys.all.sub, aes(x=mscut))+geom_density() + xlim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
Q10.<-ggplot(hys.all.sub, aes(x=Q10))+geom_density() + xlim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

ggsave(filename="Density panels\\density_hys_c1.png",plot=c1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_c2.png",plot=c2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_c3.png",plot=c3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_c4.png",plot=c4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_c5.png",plot=c5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_c6.png",plot=c6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_f1.png",plot=f1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_f2.png",plot=f2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_f3.png",plot=f3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_f4.png",plot=f4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_f5.png",plot=f5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_f6.png",plot=f6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_f7.png",plot=f7,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_mscut.png",plot=mscut.,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_hys_Q10.png",plot=Q10.,width=2.0,height=1.5,units="in",dpi=300)
rm(c1,c2,c3,c4,c5,c6,f1,f2,f3,f4,f5,f6,f7,mscut.,Q10.)


## knz ##
# c #
c1<-ggplot(knz.all.sub, aes(x=c1))+geom_density() + xlim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c2<-ggplot(knz.all.sub, aes(x=c2))+geom_density() + xlim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c3<-ggplot(knz.all.sub, aes(x=c3))+geom_density() + xlim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c4<-ggplot(knz.all.sub, aes(x=c4))+geom_density() + xlim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c5<-ggplot(knz.all.sub, aes(x=c5))+geom_density() + xlim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c6<-ggplot(knz.all.sub, aes(x=c6))+geom_density() + xlim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
f1<-ggplot(knz.all.sub, aes(x=f1))+geom_density() + xlim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f2<-ggplot(knz.all.sub, aes(x=f2))+geom_density() + xlim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f3<-ggplot(knz.all.sub, aes(x=f3))+geom_density() + xlim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f4<-ggplot(knz.all.sub, aes(x=f4))+geom_density() + xlim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f5<-ggplot(knz.all.sub, aes(x=f5))+geom_density() + xlim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f6<-ggplot(knz.all.sub, aes(x=f6))+geom_density() + xlim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f7<-ggplot(knz.all.sub, aes(x=f7))+geom_density() + xlim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
mscut.<-ggplot(knz.all.sub, aes(x=mscut))+geom_density() + xlim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
Q10.<-ggplot(knz.all.sub, aes(x=Q10))+geom_density() + xlim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

#multiplot(c1,c2,c3,c4,c5,c6,mscut.,Q10.,cols=3)
#multiplot(f1,f2,f3,f4,f5,f6,f7,cols=3)

ggsave(filename="Density panels\\density_knz_c1.png",plot=c1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_c2.png",plot=c2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_c3.png",plot=c3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_c4.png",plot=c4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_c5.png",plot=c5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_c6.png",plot=c6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_f1.png",plot=f1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_f2.png",plot=f2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_f3.png",plot=f3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_f4.png",plot=f4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_f5.png",plot=f5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_f6.png",plot=f6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_f7.png",plot=f7,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_mscut.png",plot=mscut.,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="Density panels\\density_knz_Q10.png",plot=Q10.,width=2.0,height=1.5,units="in",dpi=300)
rm(c1,c2,c3,c4,c5,c6,f1,f2,f3,f4,f5,f6,f7,mscut.,Q10.)

# Trace plots -------------------------------------------------------------
### NOT UPDATED IN FEBRUARY 2017
## SEVblu
# c #
c1<-ggplot(sevblu.c2, aes(x=sim, y=c1, colour=as.factor(chain)))+geom_line() + ylim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c2<-ggplot(sevblu.c2, aes(x=sim, y=c2, colour=as.factor(chain)))+geom_line() + ylim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c3<-ggplot(sevblu.c2, aes(x=sim, y=c3, colour=as.factor(chain)))+geom_line() + ylim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c4<-ggplot(sevblu.c2, aes(x=sim, y=c4, colour=as.factor(chain)))+geom_line() + ylim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c5<-ggplot(sevblu.c2, aes(x=sim, y=c5, colour=as.factor(chain)))+geom_line() + ylim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c6<-ggplot(sevblu.c2, aes(x=sim, y=c6, colour=as.factor(chain)))+geom_line() + ylim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
f1<-ggplot(sevblu.f2, aes(x=sim, y=f1, colour=as.factor(chain)))+geom_line() + ylim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f2<-ggplot(sevblu.f2, aes(x=sim, y=f2, colour=as.factor(chain)))+geom_line() + ylim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f3<-ggplot(sevblu.f2, aes(x=sim, y=f3, colour=as.factor(chain)))+geom_line() + ylim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f4<-ggplot(sevblu.f2, aes(x=sim, y=f4, colour=as.factor(chain)))+geom_line() + ylim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f5<-ggplot(sevblu.f2, aes(x=sim, y=f5, colour=as.factor(chain)))+geom_line() + ylim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f6<-ggplot(sevblu.f2, aes(x=sim, y=f6, colour=as.factor(chain)))+geom_line() + ylim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f7<-ggplot(sevblu.f2, aes(x=sim, y=f7, colour=as.factor(chain)))+geom_line() + ylim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
mscut.<-ggplot(sevblu.mscut2, aes(x=sim, y=mscut, colour=factor(chain)))+geom_line()+ylim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
Q10.<-ggplot(sevblu.Q102, aes(x=sim, y=Q10, colour=factor(chain)))+geom_line()+ylim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\Figures\\Trace and Density plots\\")
ggsave(filename="trace_sevblu_c1.png",plot=c1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_c2.png",plot=c2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_c3.png",plot=c3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_c4.png",plot=c4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_c5.png",plot=c5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_c6.png",plot=c6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_f1.png",plot=f1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_f2.png",plot=f2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_f3.png",plot=f3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_f4.png",plot=f4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_f5.png",plot=f5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_f6.png",plot=f6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_f7.png",plot=f7,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_mscut.png",plot=mscut.,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblu_Q10.png",plot=Q10.,width=2.0,height=1.5,units="in",dpi=300)

## sevblk
# c #
c1<-ggplot(sevblk.c2, aes(x=sim, y=c1, colour=as.factor(chain)))+geom_line() + ylim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c2<-ggplot(sevblk.c2, aes(x=sim, y=c2, colour=as.factor(chain)))+geom_line() + ylim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c3<-ggplot(sevblk.c2, aes(x=sim, y=c3, colour=as.factor(chain)))+geom_line() + ylim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c4<-ggplot(sevblk.c2, aes(x=sim, y=c4, colour=as.factor(chain)))+geom_line() + ylim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c5<-ggplot(sevblk.c2, aes(x=sim, y=c5, colour=as.factor(chain)))+geom_line() + ylim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c6<-ggplot(sevblk.c2, aes(x=sim, y=c6, colour=as.factor(chain)))+geom_line() + ylim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
f1<-ggplot(sevblk.f2, aes(x=sim, y=f1, colour=as.factor(chain)))+geom_line() + ylim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f2<-ggplot(sevblk.f2, aes(x=sim, y=f2, colour=as.factor(chain)))+geom_line() + ylim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f3<-ggplot(sevblk.f2, aes(x=sim, y=f3, colour=as.factor(chain)))+geom_line() + ylim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f4<-ggplot(sevblk.f2, aes(x=sim, y=f4, colour=as.factor(chain)))+geom_line() + ylim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f5<-ggplot(sevblk.f2, aes(x=sim, y=f5, colour=as.factor(chain)))+geom_line() + ylim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f6<-ggplot(sevblk.f2, aes(x=sim, y=f6, colour=as.factor(chain)))+geom_line() + ylim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f7<-ggplot(sevblk.f2, aes(x=sim, y=f7, colour=as.factor(chain)))+geom_line() + ylim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
mscut.<-ggplot(sevblk.mscut2, aes(x=sim, y=mscut, colour=factor(chain)))+geom_line()+ylim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
Q10.<-ggplot(sevblk.Q102, aes(x=sim, y=Q10, colour=factor(chain)))+geom_line()+ylim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\Figures\\Trace and Density plots\\")
ggsave(filename="trace_sevblk_c1.png",plot=c1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_c2.png",plot=c2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_c3.png",plot=c3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_c4.png",plot=c4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_c5.png",plot=c5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_c6.png",plot=c6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_f1.png",plot=f1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_f2.png",plot=f2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_f3.png",plot=f3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_f4.png",plot=f4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_f5.png",plot=f5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_f6.png",plot=f6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_f7.png",plot=f7,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_mscut.png",plot=mscut.,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sevblk_Q10.png",plot=Q10.,width=2.0,height=1.5,units="in",dpi=300)
## sgs
# c #
c1<-ggplot(sgs.all.sub, aes(x=sim, y=c1, colour=as.factor(chain)))+geom_line() + ylim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c2<-ggplot(sgs.all.sub, aes(x=sim, y=c2, colour=as.factor(chain)))+geom_line() + ylim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c3<-ggplot(sgs.all.sub, aes(x=sim, y=c3, colour=as.factor(chain)))+geom_line() + ylim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c4<-ggplot(sgs.all.sub, aes(x=sim, y=c4, colour=as.factor(chain)))+geom_line() + ylim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c5<-ggplot(sgs.all.sub, aes(x=sim, y=c5, colour=as.factor(chain)))+geom_line() + ylim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c6<-ggplot(sgs.all.sub, aes(x=sim, y=c6, colour=as.factor(chain)))+geom_line() + ylim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
f1<-ggplot(sgs.f2, aes(x=sim, y=f1, colour=as.factor(chain)))+geom_line() + ylim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f2<-ggplot(sgs.f2, aes(x=sim, y=f2, colour=as.factor(chain)))+geom_line() + ylim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f3<-ggplot(sgs.f2, aes(x=sim, y=f3, colour=as.factor(chain)))+geom_line() + ylim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f4<-ggplot(sgs.f2, aes(x=sim, y=f4, colour=as.factor(chain)))+geom_line() + ylim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f5<-ggplot(sgs.f2, aes(x=sim, y=f5, colour=as.factor(chain)))+geom_line() + ylim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f6<-ggplot(sgs.f2, aes(x=sim, y=f6, colour=as.factor(chain)))+geom_line() + ylim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f7<-ggplot(sgs.f2, aes(x=sim, y=f7, colour=as.factor(chain)))+geom_line() + ylim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
mscut.<-ggplot(sgs.mscut2, aes(x=sim, y=mscut, colour=factor(chain)))+geom_line()+ylim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
Q10.<-ggplot(sgs.Q102, aes(x=sim, y=Q10, colour=factor(chain)))+geom_line()+ylim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))


ggsave(filename="trace_sgs_c1.png",plot=c1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_c2.png",plot=c2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_c3.png",plot=c3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_c4.png",plot=c4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_c5.png",plot=c5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_c6.png",plot=c6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_f1.png",plot=f1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_f2.png",plot=f2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_f3.png",plot=f3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_f4.png",plot=f4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_f5.png",plot=f5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_f6.png",plot=f6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_f7.png",plot=f7,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_mscut.png",plot=mscut.,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_sgs_Q10.png",plot=Q10.,width=2.0,height=1.5,units="in",dpi=300)

## chy
# c #
c1<-ggplot(chy.all.sub, aes(x=sim, y=c1, colour=as.factor(chain)))+geom_line() + ylim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c2<-ggplot(chy.all.sub, aes(x=sim, y=c2, colour=as.factor(chain)))+geom_line() + ylim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c3<-ggplot(chy.all.sub, aes(x=sim, y=c3, colour=as.factor(chain)))+geom_line() + ylim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c4<-ggplot(chy.all.sub, aes(x=sim, y=c4, colour=as.factor(chain)))+geom_line() + ylim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c5<-ggplot(chy.all.sub, aes(x=sim, y=c5, colour=as.factor(chain)))+geom_line() + ylim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c6<-ggplot(chy.all.sub, aes(x=sim, y=c6, colour=as.factor(chain)))+geom_line() + ylim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
f1<-ggplot(chy.all.sub, aes(x=sim, y=f1, colour=as.factor(chain)))+geom_line() + ylim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f2<-ggplot(chy.all.sub, aes(x=sim, y=f2, colour=as.factor(chain)))+geom_line() + ylim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f3<-ggplot(chy.all.sub, aes(x=sim, y=f3, colour=as.factor(chain)))+geom_line() + ylim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f4<-ggplot(chy.all.sub, aes(x=sim, y=f4, colour=as.factor(chain)))+geom_line() + ylim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f5<-ggplot(chy.all.sub, aes(x=sim, y=f5, colour=as.factor(chain)))+geom_line() + ylim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f6<-ggplot(chy.all.sub, aes(x=sim, y=f6, colour=as.factor(chain)))+geom_line() + ylim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f7<-ggplot(chy.all.sub, aes(x=sim, y=f7, colour=as.factor(chain)))+geom_line() + ylim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
mscut.<-ggplot(chy.mscut2, aes(x=sim, y=mscut, colour=factor(chain)))+geom_line()+ylim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
Q10.<-ggplot(chy.Q102, aes(x=sim, y=Q10, colour=factor(chain)))+geom_line()+ylim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\Figures\\Trace and Density plots\\")
ggsave(filename="trace_chy_c1.png",plot=c1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_c2.png",plot=c2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_c3.png",plot=c3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_c4.png",plot=c4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_c5.png",plot=c5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_c6.png",plot=c6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_f1.png",plot=f1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_f2.png",plot=f2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_f3.png",plot=f3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_f4.png",plot=f4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_f5.png",plot=f5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_f6.png",plot=f6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_f7.png",plot=f7,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_mscut.png",plot=mscut.,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_chy_Q10.png",plot=Q10.,width=2.0,height=1.5,units="in",dpi=300)

## hys
# c #
c1<-ggplot(hys.all.sub, aes(x=sim, y=c1, colour=as.factor(chain)))+geom_line() + ylim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c2<-ggplot(hys.all.sub, aes(x=sim, y=c2, colour=as.factor(chain)))+geom_line() + ylim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c3<-ggplot(hys.all.sub, aes(x=sim, y=c3, colour=as.factor(chain)))+geom_line() + ylim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c4<-ggplot(hys.all.sub, aes(x=sim, y=c4, colour=as.factor(chain)))+geom_line() + ylim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c5<-ggplot(hys.all.sub, aes(x=sim, y=c5, colour=as.factor(chain)))+geom_line() + ylim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c6<-ggplot(hys.all.sub, aes(x=sim, y=c6, colour=as.factor(chain)))+geom_line() + ylim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
f1<-ggplot(hys.f2, aes(x=sim, y=f1, colour=as.factor(chain)))+geom_line() + ylim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f2<-ggplot(hys.f2, aes(x=sim, y=f2, colour=as.factor(chain)))+geom_line() + ylim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f3<-ggplot(hys.f2, aes(x=sim, y=f3, colour=as.factor(chain)))+geom_line() + ylim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f4<-ggplot(hys.f2, aes(x=sim, y=f4, colour=as.factor(chain)))+geom_line() + ylim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f5<-ggplot(hys.f2, aes(x=sim, y=f5, colour=as.factor(chain)))+geom_line() + ylim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f6<-ggplot(hys.f2, aes(x=sim, y=f6, colour=as.factor(chain)))+geom_line() + ylim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f7<-ggplot(hys.f2, aes(x=sim, y=f7, colour=as.factor(chain)))+geom_line() + ylim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
mscut.<-ggplot(hys.mscut2, aes(x=sim, y=mscut, colour=factor(chain)))+geom_line()+ylim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
Q10.<-ggplot(hys.Q102, aes(x=sim, y=Q10, colour=factor(chain)))+geom_line()+ylim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\Figures\\Trace and Density plots\\")
ggsave(filename="trace_hys_c1.png",plot=c1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_c2.png",plot=c2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_c3.png",plot=c3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_c4.png",plot=c4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_c5.png",plot=c5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_c6.png",plot=c6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_f1.png",plot=f1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_f2.png",plot=f2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_f3.png",plot=f3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_f4.png",plot=f4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_f5.png",plot=f5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_f6.png",plot=f6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_f7.png",plot=f7,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_mscut.png",plot=mscut.,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_hys_Q10.png",plot=Q10.,width=2.0,height=1.5,units="in",dpi=300)

## knz
# c #
c1<-ggplot(knz.all.sub, aes(x=sim, y=c1, colour=as.factor(chain)))+geom_line() + ylim(cmin[1],cmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c2<-ggplot(knz.all.sub, aes(x=sim, y=c2, colour=as.factor(chain)))+geom_line() + ylim(cmin[2],cmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c3<-ggplot(knz.all.sub, aes(x=sim, y=c3, colour=as.factor(chain)))+geom_line() + ylim(cmin[3],cmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c4<-ggplot(knz.all.sub, aes(x=sim, y=c4, colour=as.factor(chain)))+geom_line() + ylim(cmin[4],cmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c5<-ggplot(knz.all.sub, aes(x=sim, y=c5, colour=as.factor(chain)))+geom_line() + ylim(cmin[5],cmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
c6<-ggplot(knz.all.sub, aes(x=sim, y=c6, colour=as.factor(chain)))+geom_line() + ylim(cmin[6],cmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# f #
f1<-ggplot(knz.f2, aes(x=sim, y=f1, colour=as.factor(chain)))+geom_line() + ylim(fmin[1],fmax[1])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f2<-ggplot(knz.f2, aes(x=sim, y=f2, colour=as.factor(chain)))+geom_line() + ylim(fmin[2],fmax[2])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f3<-ggplot(knz.f2, aes(x=sim, y=f3, colour=as.factor(chain)))+geom_line() + ylim(fmin[3],fmax[3])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f4<-ggplot(knz.f2, aes(x=sim, y=f4, colour=as.factor(chain)))+geom_line() + ylim(fmin[4],fmax[4])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f5<-ggplot(knz.f2, aes(x=sim, y=f5, colour=as.factor(chain)))+geom_line() + ylim(fmin[5],fmax[5])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f6<-ggplot(knz.f2, aes(x=sim, y=f6, colour=as.factor(chain)))+geom_line() + ylim(fmin[6],fmax[6])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
f7<-ggplot(knz.f2, aes(x=sim, y=f7, colour=as.factor(chain)))+geom_line() + ylim(fmin[7],fmax[7])+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
# mscut and Q10 #
mscut.<-ggplot(knz.mscut2, aes(x=sim, y=mscut, colour=factor(chain)))+geom_line()+ylim(mscut_min,mscut_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))
Q10.<-ggplot(knz.Q102, aes(x=sim, y=Q10, colour=factor(chain)))+geom_line()+ylim(Q10_min,Q10_max)+theme_few()+theme(legend.position="none")+theme(text = element_text(size=10))

setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\Figures\\Trace and Density plots\\")
ggsave(filename="trace_knz_c1.png",plot=c1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_c2.png",plot=c2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_c3.png",plot=c3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_c4.png",plot=c4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_c5.png",plot=c5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_c6.png",plot=c6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_f1.png",plot=f1,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_f2.png",plot=f2,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_f3.png",plot=f3,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_f4.png",plot=f4,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_f5.png",plot=f5,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_f6.png",plot=f6,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_f7.png",plot=f7,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_mscut.png",plot=mscut.,width=2.0,height=1.5,units="in",dpi=300)
ggsave(filename="trace_knz_Q10.png",plot=Q10.,width=2.0,height=1.5,units="in",dpi=300)




