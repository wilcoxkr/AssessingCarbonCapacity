### Regressions of Carbon capacity, ecoRT, and NPP with MAP ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com) ###
### Last updated: July 27th, 2017 ###

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
require(vegan)
library(scatterplot3d)
setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_July2017\\")

# NPP and EcoRt data
eco.rt <- read.csv("Residence times\\mean and ci from 1000 bootstrap runs_with sd_July2017.csv")
data1 <- read.csv("NPP\\NPP means and error_from TECO.csv")
npp <- data.frame(Site=data1$site, npp=data1$NPP.u, npp.sd=data1$NPP.sd)

## Modify konza npp to just bnpp since the site is annually burned ##
npp$npp_fullKNZ <- npp$npp
npp$npp.sd_fullKNZ <- npp$npp.sd
knz.npp <- subset(npp, Site=="f.knz")$npp
knz.npp.sd <- subset(npp, Site=="f.knz")$npp.sd
npp[npp$Site=="f.knz",2] <- knz.npp * 0.38
npp[npp$Site=="f.knz",3] <- knz.npp.sd * 0.38

## merge ecoRt and npp ##
capacity <- merge(eco.rt, npp, by="Site")
capacity$c.cap <- capacity$mean*capacity$npp
capacity$c.cap.sd <- with(capacity, c.cap*sqrt(((npp.sd/npp)^2 + (Rt.sd/mean)^2)))
capacity$c.cap_fullKNZ <- capacity$mean*capacity$npp_fullKNZ


## Merge with site characteristic data
data.full <- merge(capacity[,c("Site","mean","npp","c.cap","npp_fullKNZ","c.cap_fullKNZ")],
                   pca.all,
                   by.x="Site", by.y="site")
char.carbon <- merge(capacity[,c("Site","mean","npp","c.cap","npp_fullKNZ","c.cap_fullKNZ")],
                     site.char.all,
                     by.x="Site", by.y="site")

colnames(data.full)[2] <- "eco.rt"
colnames(char.carbon)[2] <- "eco.rt"

# Calculate grass:forb, c3:c4, c3g:c4g #
char.carbon$g.f.ratio <- with(char.carbon, (C3g+C4g)/F)
char.carbon$c3.c4.ratio <- with(char.carbon, ((F+C3g+W)/C4g))
char.carbon$c3g.c4g.ratio <- with(char.carbon, (C3g/C4g))

cap_map_logModel <- lm(c.cap~log(annual.ppt), 
                       data=subset(char.carbon, Site!="f.knz"))
summary(cap_map_logModel)
cap_map_xhat <- with(subset(char.carbon, Site!="f.knz"),
                     seq(min(annual.ppt),max(annual.ppt),length=100))

cap_map_yhat <- predict(cap_map_logModel, list(annual.ppt=cap_map_xhat))
cap_map_hats <- data.frame(xhat=cap_map_xhat,yhat=cap_map_yhat)

ggplot(subset(char.carbon, Site!="f.knz"), aes(x=annual.ppt, y=c.cap))+
  geom_point(size=3)+
  geom_line(data=cap_map_hats, aes(x=xhat, y=yhat))+
  theme_classic()+
  ylim(0,10000)+
  xlab("Mean annual precipitation (mm)")+
  ylab("Carbon capacity (g/m2)")

ggsave("Figures//MAP vs C capacity_July2017.pdf", width=3.5, height=3.25,units="in")
ggplot(subset(char.carbon), aes(x=annual.temp, y=eco.rt))+
  geom_point(size=3)
  

cap_map_logModel <- lm(c.cap~log(annual.ppt), 
                       data=subset(char.carbon, Site!="f.knz"))
summary(cap_map_logModel)
cap_map_xhat <- with(subset(char.carbon, Site!="f.knz"),
                     seq(min(annual.ppt),max(annual.ppt),length=100))

cap_map_yhat <- predict(cap_map_logModel, list(annual.ppt=cap_map_xhat))
cap_map_hats <- data.frame(xhat=cap_map_xhat,yhat=cap_map_yhat)

ggplot(subset(char.carbon, Site!="f.knz"), aes(x=annual.ppt, y=c.cap))+
  geom_point(size=3)+
  geom_line(data=cap_map_hats, aes(x=xhat, y=yhat))+
  theme_classic()+
  ylim(0,10000)+
  xlab("Mean annual precipitation (mm)")+
  ylab("Carbon capacity (g/m2)")

ggsave("Figures//MAP vs C capacity_July2017.pdf", width=3.5, height=3.25,units="in")

# full KNZ
cap_map_logModel <- lm(c.cap_fullKNZ~log(annual.ppt), 
                       data=subset(char.carbon))
summary(cap_map_logModel)
cap_map_xhat <- with(subset(char.carbon),
                     seq(min(annual.ppt),max(annual.ppt),length=100))

cap_map_yhat <- predict(cap_map_logModel, list(annual.ppt=cap_map_xhat))
cap_map_hats <- data.frame(xhat=cap_map_xhat,yhat=cap_map_yhat)

ggplot(subset(char.carbon), aes(x=annual.ppt, y=c.cap_fullKNZ))+
  geom_point(size=3)+
  geom_line(data=cap_map_hats, aes(x=xhat, y=yhat))+
  theme_classic()+
  ylim(0,12500)+
  xlab("Mean annual precipitation (mm)")+
  ylab("Carbon capacity (g/m2)")

ggsave("Figures//MAP vs C capacity_July2017.pdf", width=3.5, height=3.25,units="in")

cap.varpart <- varpart(char.carbon$c.cap_fullKNZ, ~annual.ppt, ~annual.temp, data=char.carbon)
plot(cap.varpart, bg=c("darkgrey","green"),Xnames=c("MAP","MAT"))

#########
######### To show: 1. C capicity in all sites (full KNZ), 2. C storage potential vs MAP, MAT, 3. variance partitioning.


