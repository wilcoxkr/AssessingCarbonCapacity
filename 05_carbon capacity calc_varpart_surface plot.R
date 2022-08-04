### Calculating Carbon capacity, Carbon Saturation, partitioning variance, and surface plot ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com) ###
### Last updated: Dec 19th, 2017 ###

# Workspace setup ------------------------------------------------------------------

rm(list=ls())
library(plyr)
library(tidyr)
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
#setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_July2017\\")
setwd("C:\\Users\\Kevin.Wilcox\\Desktop\\fromOldComp\\FULL_ANALYSES_July2017\\")

eco.rt <- read.csv("Residence times\\mean and ci from 1000 bootstrap runs_with sd_July2017.csv")
data1 <- read.csv("NPP\\NPP means and error_from TECO.csv")
npp <- data.frame(Site=data1$site, npp=data1$NPP.u, npp.sd=data1$NPP.sd)

## Modify konza npp to just bnpp since the site is annually burned ##
knz.npp <- subset(npp, Site=="f.knz")$npp
knz.npp.sd <- subset(npp, Site=="f.knz")$npp.sd
npp[npp$Site=="f.knz",2] <- knz.npp * 0.38
npp[npp$Site=="f.knz",3] <- knz.npp.sd * 0.38

## merge ecoRt and npp ##
capacity <- merge(eco.rt, npp, by="Site")
capacity$c.cap <- capacity$mean*capacity$npp
capacity$c.cap.sd <- with(capacity, c.cap*sqrt(((npp.sd/npp)^2 + (Rt.sd/mean)^2)))

# with Konza #
cap.varpart <- varpart(capacity$c.cap, ~mean, ~npp, data=capacity)
plot(cap.varpart, bg=c("darkgrey","green"),Xnames=c("Eco_Rt","NPP"))

## 3d plot of npp, eco_rt, and carbon capacity ##
with(subset(capacity), {
  scatterplot3d(mean, npp, c.cap,        # x y and z axis
                color="blue", pch=19, # filled blue circles
                type="h",             # lines to the horizontal plane
                main="3-D Scatterplot Example 2",
                xlab="Eco_rt",
                ylab="NPP",
                zlab="Carbon Capacity")
})


# without Konza #
cap.varpart.noKNZ <- varpart(subset(capacity, Site!="f.knz")$c.cap, ~mean, ~npp, data=subset(capacity, Site!="f.knz"))
plot(cap.varpart.noKNZ, bg=c("darkgrey","green"),Xnames=c("Eco_Rt","NPP"))

## 3d plot of npp, eco_rt, and carbon capacity ##
with(subset(capacity, Site!="f.knz"), {
  scatterplot3d(mean, npp, c.cap,        # x y and z axis
                color="blue", pch=19, # filled blue circles
                type="h",             # lines to the horizontal plane
                main="3-D Scatterplot Example 2",
                xlab="Eco_rt",
                ylab="NPP",
                zlab="Carbon Capacity")
})

# Calculate measured soil C and standard deviation ------------------------
C_content.4north <- read.csv("SoilC//EDGE_north_sites_soilC.csv")
C_content.sev <- read.csv("SoilC//Ladwig_soil C sev.csv")  

#head(C_content.4north)  
C.u.4north <- ddply(C_content.4north, .(site), summarize,
      C_gm2 = mean(C_gm2))
C.sd.4north <- ddply(C_content.4north, .(site), summarize,
      C_gm2.sd = sd(C_gm2))
C.se.4north <- ddply(C_content.4north, .(site), summarize,
      C_gm2.se = sd(C_gm2)/sqrt(length(C_gm2)))
C.4north <- C.u.4north %>%
  full_join(C.sd.4north, by="site") %>%
  full_join(C.se.4north, by="site")

test <- subset(C_content.4north, site=="hys")
levels(factor(C_content.4north$site))

#head(C_content.sev)
C.u.sev <- ddply(C_content.sev, .(plant_under), summarize,
                       C_gm2 = mean(C_gm2))
C.sd.sev <- ddply(C_content.sev, .(plant_under), summarize,
                       C_gm2.sd = sd(C_gm2,na.rm=T))
C.se.sev <- ddply(C_content.sev, .(plant_under), summarize,
                     C_gm2.se = sd(C_gm2)/sqrt(length(C_gm2)))
C.sev <- C.u.sev %>%
  full_join(C.sd.sev, by="plant_under") %>%
  full_join(C.se.sev, by="plant_under")
  
colnames(C.sev) <- colnames(C.4north)
C_content <- rbind(C.4north, C.sev)
C_content$site <- c("HPG","HAR","KNZ","CPER","SBK","SBL")

## add in vegetative C
veg_C <- read.csv("soilC//vegC_all sites_measured.csv") %>%
  group_by(site, C_type) %>%
  summarise(C_mass=mean(C_mass)) %>%
  group_by(site) %>%
  summarise(tot_veg_C = sum(C_mass))
  
C_content_with_veg <- C_content %>%
  full_join(veg_C, by="site") %>%
  mutate(all_C_gm2 = C_gm2+tot_veg_C)

### read in C content, extrapolated to 20cm with veg C
C_content_with_veg <- read.csv("soilC//veg plus soil C_extrapolated to 20 cm.csv")  %>%
  dplyr::rename(Site=site)


capacity$Site <- c("SBL","SBK","CPER","HPG","HAR","KNZ")
capacity$Site = factor(capacity$Site, levels=c("KNZ","HAR","HPG","CPER","SBK","SBL"))
C_content_with_veg$Site = factor(C_content_with_veg$Site, levels=c("KNZ","HAR","HPG","CPER","SBK","SBL"))
#C_content_with_veg$Site = factor(C_content$Site, levels=c("KNZ","HAR","HPG","CPER","SBK","SBL"))


c_fig <- ggplot(capacity, aes(x=c.cap, y=Site))+
  geom_tile(aes(x=c.cap, y=Site, width=c.cap.sd, height=0.5), fill="lightgrey") +
  geom_point(size=0.75, col="darkgrey")+
  geom_point(data=C_content_with_veg, aes(x=C_to_20cm_plus_veg, y=Site),size=0.75)+
  geom_errorbarh(data=C_content_with_veg, aes(x=C_to_20cm_plus_veg, xmin=C_to_20cm_plus_veg-C_to_20cm_sd, xmax=C_to_20cm_plus_veg+C_to_20cm_sd, y=Site),height=0, size=0.3)+
#  xlim(-100, 11000)+
  theme_few() +
  theme(text=element_text(size=10),
        axis.text=element_text(size=8, color="black"),
        axis.ticks=element_line(size=0.3, color="black"),
        panel.border=element_rect(size=0.3,color="black") ) +
  xlab(expression(Carbon~(g%.%m^-2)))

pdf("Figures//C capacity vs content_with vegC_MAR2018.pdf", height=2.3622, width=3.14961, useDingbats = F)
print(c_fig)
dev.off()

png("Figures//C capacity vs content_with vegC_MAR2018.png", height=2.3622, width=3.14961, units="in", res=600)
print(c_fig)
dev.off()


### Calculate C saturation
c_sat <- capacity %>%
  dplyr::select(Site, c.cap) %>%
  full_join(dplyr::select(C_content_with_veg, Site, C_to_20cm_plus_veg), by="Site") %>%
  mutate(c_sat = C_to_20cm_plus_veg/c.cap)
c_sat$Site = factor(c_sat$Site, levels=c("SBL","SBK","CPER","HPG","HAR","KNZ"))

### Plot C saturation
c_sat_fig <- ggplot(c_sat, aes(x=Site, y=c_sat*100))+
  geom_bar( stat="identity", fill="white", col="black", size=0.15)+
  geom_hline(yintercept=100, lty=2, size=0.15, col="black")+
  theme_few() +
  theme(text=element_text(size=5),
        axis.text=element_text(size=4, color="black"),
        axis.ticks=element_line(size=0.15, color="black"),
        axis.ticks.length = unit(.01,"in"),
        panel.border=element_rect(size=0.3,color="black") ) +
  ylab("C sat (%)")+
  xlab("") +
  ylim(0,200)

png("Figures//C saturation inset_MAR2018.png", height=.9, width=1.4, units="in", res=600)
print(c_sat_fig)
dev.off()

pdf("Figures//C saturation inset_MAR2018.pdf", height=0.9, width=1.4, useDingbats = F)
print(c_sat_fig)
dev.off()


### Figure of C storage potential vs MAP
C_potential_ppt <- read.csv("Site characteristics//Site characteristics_soil veg and climate_all sites.csv") %>%
  dplyr::select(site, annual.ppt) %>%
  dplyr::rename(Site=site) %>%
  left_join(capacity[,c("Site","c.cap","c.cap.sd")], by="Site") %>%
  left_join(C_content[,c("Site","C_gm2","C_gm2.se")], by="Site")

C_potential_ppt$c_poten <- with(C_potential_ppt, (c.cap-C_gm2))

cpot_logmod <- lm(c_poten~log(annual.ppt),data=C_potential_ppt)
xhat <- data.frame(annual.ppt=with(C_potential_ppt, seq(min(annual.ppt),max(annual.ppt),length=10)))
yhat <- predict(cpot_logmod, newdata=xhat)


with(subset(C_potential_ppt, Site!="f.knz"), plot(annual.ppt,c_poten))
lines(xhat$annual.ppt, yhat)


ggplot(subset(C_potential_ppt, Site!="f.knz"), aes(x=annual.ppt, y=c_poten))+
  geom_point()+
  theme_few()





