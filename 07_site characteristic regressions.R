### Comparing site characteristics with NPP, Eco_Rt, and model parameters ###
### Author: Kevin wilcox (wilcoxkr@gmail.com) ###
### Last updated: July 27th ###

rm(list=ls())
#setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_July2017\\")
setwd("C:\\Users\\Kevin.Wilcox\\Desktop\\fromOldComp\\FULL_ANALYSES_July2017\\")
source("scripts\\05_carbon capacity calc_varpart_surface plot.R")
#source("Scripts\\06_PCA for site characteristics.R")
site.char.all <- read.csv("Site characteristics//Site characteristics_soil veg and climate_all sites.csv")
library(car)
library(MASS)
library(Hmisc)
library(plyr)
library(tidyverse)

# Prepare data ------------------------------------------------------------
data.full <- merge(capacity[,c("Site","mean","npp","c.cap")],
                   pca.all,
                   by.x="Site", by.y="site")
char.carbon <- merge(capacity[,c("Site","mean","npp","c.cap")],
                   site.char.all,
                   by.x="Site", by.y="site")

colnames(data.full)[2] <- "eco.rt"
colnames(char.carbon)[2] <- "eco.rt"

# for writing files #
write_path <- "Regression output\\"
varpart_path <- "Regression output\\varpart\\"

# Calculate grass:forb, c3:c4, c3g:c4g #
char.carbon$g.f.ratio <- with(char.carbon, (C3g+C4g)/F)
char.carbon$c3.c4.ratio <- with(char.carbon, ((F+C3g+W)/C4g))
char.carbon$c3g.c4g.ratio <- with(char.carbon, (C3g/C4g))

# Get column for full Konza NPP #
char.carbon$npp.fullKNZ <- char.carbon$npp
char.carbon[char.carbon$Site=="f.knz",]$npp.fullKNZ <- 
  char.carbon[char.carbon$Site=="f.knz",]$npp/0.38

### Simple NPP and EcoRT regressions with MAP and MAT
summary(lm(npp.fullKNZ ~ annual.ppt + annual.temp, data=char.carbon))
summary(lm(eco.rt ~ annual.ppt + annual.temp, data=char.carbon))

# Calculate soil + vegetative C ------------------------
C_content.4north <- read.csv("SoilC//EDGE_north_sites_soilC.csv")
C_content.sev <- read.csv("SoilC//Ladwig_soil C sev.csv")  

C.4north <- C_content.4north %>%
  group_by(site) %>%
  summarise(C_gm2 = mean(C_gm2))

C.sev <- C_content.sev %>%
  group_by(plant_under) %>%
  summarise(C_gm2 = mean(C_gm2)) %>%
  rename(site=plant_under)
  
C_content <- rbind(C.4north, C.sev)
C_content$site_old <- c("d.chy","e.hys","f.knz","c.sgs","b.sevblk","a.sevblu")
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



# Run regressions with PC axes ---------------------------------------------------------

## c.cap ##
sink(file=paste0(write_path, "c.cap_climate1_regression.csv"))
summary(lm(c.cap~climatePC1, data=data.full))
Anova(lm(c.cap~climatePC1, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "c.cap_climate2_regression.csv"))
summary(lm(c.cap~climatePC2, data=data.full))
Anova(lm(c.cap~climatePC2, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "c.cap_soil1_regression.csv"))
summary(lm(c.cap~soilPC1, data=data.full))
Anova(lm(c.cap~soilPC1, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "c.cap_soil2_regression.csv"))
summary(lm(c.cap~soilPC2, data=data.full))
Anova(lm(c.cap~soilPC2, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "c.cap_veg1_regression.csv"))
summary(lm(c.cap~vegPC1, data=data.full))
Anova(lm(c.cap~vegPC1, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "c.cap_veg2_regression.csv"))
summary(lm(c.cap~vegPC2, data=data.full))
Anova(lm(c.cap~vegPC2, data=data.full),type=3)
sink()

## c.cap_no KNZ ##
sink(file=paste0(write_path, "c.cap_noKNZ_climate1_regression_.csv"))
summary(lm(c.cap~climatePC1, data=subset(data.full, Site!="f.knz")))
Anova(lm(c.cap~climatePC1, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "c.cap_noKNZ_climate2_regression.csv"))
summary(lm(c.cap~climatePC2, data=subset(data.full, Site!="f.knz")))
Anova(lm(c.cap~climatePC2, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "c.cap_noKNZ_soil1_regression.csv"))
summary(lm(c.cap~soilPC1, data=subset(data.full, Site!="f.knz")))
Anova(lm(c.cap~soilPC1, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "c.cap_noKNZ_soil2_regression.csv"))
summary(lm(c.cap~soilPC2, data=subset(data.full, Site!="f.knz")))
Anova(lm(c.cap~soilPC2, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "c.cap_noKNZ_veg1_regression.csv"))
summary(lm(c.cap~vegPC1, data=subset(data.full, Site!="f.knz")))
Anova(lm(c.cap~vegPC1, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "c.cap_noKNZ_veg2_regression.csv"))
summary(lm(c.cap~vegPC2, data=subset(data.full, Site!="f.knz")))
Anova(lm(c.cap~vegPC2, data=subset(data.full, Site!="f.knz")),type=3)
sink()

## npp ##
sink(file=paste0(write_path, "npp_climate1_regression.csv"))
summary(lm(npp~climatePC1, data=data.full))
Anova(lm(npp~climatePC1, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "npp_climate2_regression.csv"))
summary(lm(npp~climatePC2, data=data.full))
Anova(lm(npp~climatePC2, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "npp_soil1_regression.csv"))
summary(lm(npp~soilPC1, data=data.full))
Anova(lm(npp~soilPC1, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "npp_soil2_regression.csv"))
summary(lm(npp~soilPC2, data=data.full))
Anova(lm(npp~soilPC2, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "npp_veg1_regression.csv"))
summary(lm(npp~vegPC1, data=data.full))
Anova(lm(npp~vegPC1, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "npp_veg2_regression.csv"))
summary(lm(npp~vegPC2, data=data.full))
Anova(lm(npp~vegPC2, data=data.full),type=3)
sink()

## npp_no KNZ ##
sink(file=paste0(write_path, "npp_noKNZ_climate1_regression_.csv"))
summary(lm(npp~climatePC1, data=subset(data.full, Site!="f.knz")))
Anova(lm(npp~climatePC1, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "npp_noKNZ_climate2_regression.csv"))
summary(lm(npp~climatePC2, data=subset(data.full, Site!="f.knz")))
Anova(lm(npp~climatePC2, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "npp_noKNZ_soil1_regression.csv"))
summary(lm(npp~soilPC1, data=subset(data.full, Site!="f.knz")))
Anova(lm(npp~soilPC1, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "npp_noKNZ_soil2_regression.csv"))
summary(lm(npp~soilPC2, data=subset(data.full, Site!="f.knz")))
Anova(lm(npp~soilPC2, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "npp_noKNZ_veg1_regression.csv"))
summary(lm(npp~vegPC1, data=subset(data.full, Site!="f.knz")))
Anova(lm(npp~vegPC1, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "npp_noKNZ_veg2_regression.csv"))
summary(lm(npp~vegPC2, data=subset(data.full, Site!="f.knz")))
Anova(lm(npp~vegPC2, data=subset(data.full, Site!="f.knz")),type=3)
sink()

## eco.rt ##
sink(file=paste0(write_path, "eco.rt_climate1_regression.csv"))
summary(lm(eco.rt~climatePC1, data=data.full))
Anova(lm(eco.rt~climatePC1, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "eco.rt_climate2_regression.csv"))
summary(lm(eco.rt~climatePC2, data=data.full))
Anova(lm(eco.rt~climatePC2, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "eco.rt_soil1_regression.csv"))
summary(lm(eco.rt~soilPC1, data=data.full))
Anova(lm(eco.rt~soilPC1, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "eco.rt_soil2_regression.csv"))
summary(lm(eco.rt~soilPC2, data=data.full))
Anova(lm(eco.rt~soilPC2, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "eco.rt_veg1_regression.csv"))
summary(lm(eco.rt~vegPC1, data=data.full))
Anova(lm(eco.rt~vegPC1, data=data.full),type=3)
sink()

sink(file=paste0(write_path, "eco.rt_veg2_regression.csv"))
summary(lm(eco.rt~vegPC2, data=data.full))
Anova(lm(eco.rt~vegPC2, data=data.full),type=3)
sink()

## eco.rt_no KNZ ##
sink(file=paste0(write_path, "eco.rt_noKNZ_climate1_regression_.csv"))
summary(lm(eco.rt~climatePC1, data=subset(data.full, Site!="f.knz")))
Anova(lm(eco.rt~climatePC1, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "eco.rt_noKNZ_climate2_regression.csv"))
summary(lm(eco.rt~climatePC2, data=subset(data.full, Site!="f.knz")))
Anova(lm(eco.rt~climatePC2, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "eco.rt_noKNZ_soil1_regression.csv"))
summary(lm(eco.rt~soilPC1, data=subset(data.full, Site!="f.knz")))
Anova(lm(eco.rt~soilPC1, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "eco.rt_noKNZ_soil2_regression.csv"))
summary(lm(eco.rt~soilPC2, data=subset(data.full, Site!="f.knz")))
Anova(lm(eco.rt~soilPC2, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "eco.rt_noKNZ_veg1_regression.csv"))
summary(lm(eco.rt~vegPC1, data=subset(data.full, Site!="f.knz")))
Anova(lm(eco.rt~vegPC1, data=subset(data.full, Site!="f.knz")),type=3)
sink()

sink(file=paste0(write_path, "eco.rt_noKNZ_veg2_regression.csv"))
summary(lm(eco.rt~vegPC2, data=subset(data.full, Site!="f.knz")))
Anova(lm(eco.rt~vegPC2, data=subset(data.full, Site!="f.knz")),type=3)
sink()

# Regressions with raw site characteristics -------------------------------

# npp.fullKNZ #
  # annual.ppt #
sink(file=paste0(write_path, "npp_annual.ppt_regression_fullKNZ.csv"))
summary(lm(npp.fullKNZ~annual.ppt, data=char.carbon))
Anova(lm(npp.fullKNZ~annual.ppt, data=char.carbon),type=3)
sink()
  # annual.temp #
sink(file=paste0(write_path, "npp_annual.temp_regression_fullKNZ.csv"))
summary(lm(npp.fullKNZ~annual.temp, data=char.carbon))
Anova(lm(npp.fullKNZ~annual.temp, data=char.carbon),type=3)
sink()
  # BD #
sink(file=paste0(write_path, "npp_BD_regression_fullKNZ.csv"))
summary(lm(npp.fullKNZ~BD, data=char.carbon))
Anova(lm(npp.fullKNZ~BD, data=char.carbon),type=3)
sink()
  # g.f.ratio #
sink(file=paste0(write_path, "npp_g.f.ratio_regression_fullKNZ.csv"))
summary(lm(npp.fullKNZ~g.f.ratio, data=char.carbon))
Anova(lm(npp.fullKNZ~g.f.ratio, data=char.carbon),type=3)
sink()
# c3.c4.ratio #
sink(file=paste0(write_path, "npp_c3.c4.ratio_regression_fullKNZ.csv"))
summary(lm(npp.fullKNZ~c3.c4.ratio, data=char.carbon))
Anova(lm(npp.fullKNZ~c3.c4.ratio, data=char.carbon),type=3)
sink()
# Annual #
sink(file=paste0(write_path, "npp_Annual_regression_fullKNZ.csv"))
summary(lm(npp.fullKNZ~Annual, data=char.carbon))
Anova(lm(npp.fullKNZ~Annual, data=char.carbon),type=3)
sink()

# npp noKNZ #
# annual.ppt #
sink(file=paste0(write_path, "npp_annual.ppt_regression_noKNZ.csv"))
summary(lm(npp~annual.ppt, data=subset(char.carbon, Site!="f.knz")))
Anova(lm(npp~annual.ppt, data=subset(char.carbon, Site!="f.knz")),type=3)
sink()
# annual.temp #
sink(file=paste0(write_path, "npp_annual.temp_regression_noKNZ.csv"))
summary(lm(npp~annual.temp, data=subset(char.carbon, Site!="f.knz")))
Anova(lm(npp~annual.temp, data=subset(char.carbon, Site!="f.knz")),type=3)
sink()
# BD #
sink(file=paste0(write_path, "npp_BD_regression_noKNZ.csv"))
summary(lm(npp~BD, data=subset(char.carbon, Site!="f.knz")))
Anova(lm(npp~BD, data=subset(char.carbon, Site!="f.knz")),type=3)
sink()
# g.f.ratio #
sink(file=paste0(write_path, "npp_g.f.ratio_regression_noKNZ.csv"))
summary(lm(npp~g.f.ratio, data=subset(char.carbon, Site!="f.knz")))
Anova(lm(npp~g.f.ratio, data=subset(char.carbon, Site!="f.knz")),type=3)
sink()
# c3.c4.ratio #
sink(file=paste0(write_path, "npp_c3.c4.ratio_regression_noKNZ.csv"))
summary(lm(npp~c3.c4.ratio, data=subset(char.carbon, Site!="f.knz")))
Anova(lm(npp~c3.c4.ratio, data=subset(char.carbon, Site!="f.knz")),type=3)
sink()
# Annual #
sink(file=paste0(write_path, "npp_Annual_regression_noKNZ.csv"))
summary(lm(npp~Annual, data=subset(char.carbon, Site!="f.knz")))
Anova(lm(npp~Annual, data=subset(char.carbon, Site!="f.knz")),type=3)
sink()

# eco.rt #
# annual.ppt #
sink(file=paste0(write_path, "ecort_annual.ppt_regression.csv"))
summary(lm(eco.rt~annual.ppt, data=char.carbon))
Anova(lm(eco.rt~annual.ppt, data=char.carbon),type=3)
sink()
# annual.temp #
sink(file=paste0(write_path, "ecort_annual.temp_regression.csv"))
summary(lm(eco.rt~annual.temp, data=char.carbon))
Anova(lm(eco.rt~annual.temp, data=char.carbon),type=3)
sink()
# BD #
sink(file=paste0(write_path, "ecort_BD_regression.csv"))
summary(lm(eco.rt~BD, data=char.carbon))
Anova(lm(eco.rt~BD, data=char.carbon),type=3)
sink()
# g.f.ratio #
sink(file=paste0(write_path, "ecort_g.f.ratio_regression.csv"))
summary(lm(eco.rt~g.f.ratio, data=char.carbon))
Anova(lm(eco.rt~g.f.ratio, data=char.carbon),type=3)
sink()
# c3.c4.ratio #
sink(file=paste0(write_path, "ecort_c3.c4.ratio_regression.csv"))
summary(lm(eco.rt~c3.c4.ratio, data=char.carbon))
Anova(lm(eco.rt~c3.c4.ratio, data=char.carbon),type=3)
sink()
# Annual #
sink(file=paste0(write_path, "ecort_Annual_regression.csv"))
summary(lm(eco.rt~Annual, data=char.carbon))
Anova(lm(eco.rt~Annual, data=char.carbon),type=3)
sink()

# Regression plots NO KNZ --------------------------------------------------------

# npp #
annual.ppt_npp.plot <- ggplot(subset(char.carbon, Site!="f.knz"), aes(x=annual.ppt, y=npp))+
  geom_point(size=3, pch=21)+
  stat_smooth(method="lm")+
  xlab("Mean annual precipitation (mm)")+
  ylab("Net Primary Productivity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

annual.temp_npp.plot <- ggplot(subset(char.carbon, Site!="f.knz"), aes(x=annual.temp, y=npp.fullKNZ))+
  geom_point(size=3, pch=21)+
  stat_smooth(method="lm",linetype=2)+
  xlab("Mean annual temperature (C)")+
  ylab("Net Primary Productivity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

BD_npp.plot <- ggplot(subset(char.carbon, Site!="f.knz"), aes(x=BD, y=npp.fullKNZ))+
  geom_point(size=3, pch=21)+
  stat_smooth(method="lm", linetype=1)+
  xlab("Bulk Density (g cm-3)")+
  ylab("Net Primary Productivity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

g.f.ratio_npp.plot <- ggplot(subset(char.carbon, Site!="f.knz"), aes(x=g.f.ratio, y=npp.fullKNZ))+
  geom_point(size=3, pch=21)+
  stat_smooth(method="lm", linetype=1)+
  xlab("Grass:Forb abundance")+
  ylab("Net Primary Productivity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

c3.c4.ratio_npp.plot <- ggplot(subset(char.carbon, Site!="f.knz"), aes(x=c3.c4.ratio, y=npp.fullKNZ))+
  geom_point(size=3, pch=21)+
  stat_smooth(method="lm", linetype=2)+
  xlab("C3:C4 abundance")+
  ylab("Net Primary Productivity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

Annual_npp.plot <- ggplot(subset(char.carbon, Site!="f.knz"), aes(x=Annual, y=npp.fullKNZ))+
  geom_point(size=3, pch=21)+
  stat_smooth(method="lm", linetype=1)+
  xlab("Annual species abundance (%)")+
  ylab("Net Primary Productivity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

pdf("Figures//Regressions//ppt_npp_regression_noKNZ.pdf", width=3.5, height=3.0,  useDingbats = F)
print(annual.ppt_npp.plot)
dev.off()

pdf("Figures//Regressions//temp_npp_regression_noKNZ.pdf", width=3.5, height=3.0,  useDingbats = F)
print(annual.temp_npp.plot)
dev.off()

pdf("Figures//Regressions//BD_npp_regression_noKNZ.pdf", width=3.5, height=3.0,  useDingbats = F)
print(BD_npp.plot)
dev.off()

pdf("Figures//Regressions//g_f_npp_regression_noKNZ.pdf", width=3.5, height=3.0,  useDingbats = F)
print(g.f.ratio_npp.plot)
dev.off()

pdf("Figures//Regressions//c3_c4_npp_regression_noKNZ.pdf", width=3.5, height=3.0,  useDingbats = F)
print(c3.c4.ratio_npp.plot)
dev.off()

pdf("Figures//Regressions//annual_npp_regression_noKNZ.pdf", width=3.5, height=3.0,  useDingbats = F)
print(Annual_npp.plot)
dev.off()

# eco.rt #
summary(lm(eco.rt~annual.ppt, data=subset(char.carbon, Site!="f.knz")))
summary(lm(eco.rt~annual.temp, data=subset(char.carbon, Site!="f.knz")))
summary(lm(eco.rt~BD, data=subset(char.carbon, Site!="f.knz")))
summary(lm(eco.rt~g.f.ratio, data=subset(char.carbon, Site!="f.knz")))
summary(lm(eco.rt~c3.c4.ratio, data=subset(char.carbon, Site!="f.knz")))
summary(lm(eco.rt~Annual, data=subset(char.carbon, Site!="f.knz")))

annual.ppt_eco.rt.plot <- ggplot(subset(char.carbon, Site!="f.knz"), aes(x=annual.ppt, y=eco.rt))+
  geom_point(size=3, pch=21)+
  stat_smooth(method="lm", linetype=1)+
  xlab("Mean annual precipitation (mm)")+
  ylab("Ecosystem residence time (years)")+
  theme(text=element_text(size=14))+
  theme_few()

annual.temp_eco.rt.plot <- ggplot(subset(char.carbon, Site!="f.knz"), aes(x=annual.temp, y=eco.rt))+
  geom_point(size=3, pch=21)+
  stat_smooth(method="lm", linetype=1)+
  xlab("Mean annual temperature (C)")+
  ylab("Ecosystem residence time (years)")+
  theme(text=element_text(size=14))+
  theme_few()

BD_eco.rt.plot <- ggplot(subset(char.carbon, Site!="f.knz"), aes(x=BD, y=eco.rt))+
  geom_point(size=3, pch=21)+
  stat_smooth(method="lm", linetype=1)+
  xlab("Bulk Density (g cm-3)")+
  ylab("Ecosystem residence time (years)")+
  theme(text=element_text(size=14))+
  theme_few()

g.f.ratio_eco.rt.plot <- ggplot(subset(char.carbon, Site!="f.knz"), aes(x=g.f.ratio, y=eco.rt))+
  geom_point(size=3, pch=21)+
  stat_smooth(method="lm", linetype=1)+
  xlab("Grass:Forb abundance")+
  ylab("Ecosystem residence time (years)")+
  theme(text=element_text(size=14))+
  theme_few()

c3.c4.ratio_eco.rt.plot <- ggplot(subset(char.carbon, Site!="f.knz"), aes(x=c3.c4.ratio, y=eco.rt))+
  geom_point(size=3, pch=21)+
  stat_smooth(method="lm", linetype=2)+
  xlab("C3:C4 abundance")+
  ylab("Ecosystem residence time (years)")+
  theme(text=element_text(size=14))+
  theme_few()

Annual_eco.rt.plot <- ggplot(subset(char.carbon, Site!="f.knz"), aes(x=Annual, y=eco.rt))+
  geom_point(size=3, pch=21)+
  stat_smooth(method="lm", linetype=2)+
  xlab("Annual species abundance (%)")+
  ylab("Ecosystem residence time (years)")+
  theme(text=element_text(size=14))+
  theme_few()


pdf("Figures//Regressions//ppt_eco.rt_regression_noKNZ.pdf", width=3.5, height=3.0,  useDingbats = F)
print(annual.ppt_eco.rt.plot)
dev.off()

pdf("Figures//Regressions//temp_eco.rt_regression_noKNZ.pdf", width=3.5, height=3.0,  useDingbats = F)
print(annual.temp_eco.rt.plot)
dev.off()

pdf("Figures//Regressions//BD_eco.rt_regression_noKNZ.pdf", width=3.5, height=3.0,  useDingbats = F)
print(BD_eco.rt.plot)
dev.off()

pdf("Figures//Regressions//g_f_eco.rt_regression_noKNZ.pdf", width=3.5, height=3.0,  useDingbats = F)
print(g.f.ratio_eco.rt.plot)
dev.off()

pdf("Figures//Regressions//c3_c4_eco.rt_regression_noKNZ.pdf", width=3.5, height=3.0,  useDingbats = F)
print(c3.c4.ratio_eco.rt.plot)
dev.off()

pdf("Figures//Regressions//annual_eco.rt_regression_noKNZ.pdf", width=3.5, height=3.0,  useDingbats = F)
print(Annual_eco.rt.plot)
dev.off()


# Regression plots with KNZ --------------------------------------------------------

# npp #
annual.ppt_npp.plot <- ggplot(char.carbon, aes(x=annual.ppt, y=npp.fullKNZ))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="grey60")+
  stat_smooth(data=subset(char.carbon, Site!="f.knz"), 
              aes(x=annual.ppt,y=npp),method="lm",se=F, col="black")+
  xlab("Mean annual precipitation (mm)")+
  ylab("Net Primary Productivity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

annual.temp_npp.plot <- ggplot(char.carbon, aes(x=annual.temp, y=npp.fullKNZ))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="grey60", linetype=2)+
  stat_smooth(data=subset(char.carbon, Site!="f.knz"), 
              aes(x=annual.temp,y=npp),method="lm",se=F, col="black", linetype=2)+
  xlab("Mean annual temperature (C)")+
  ylab("Net Primary Productivity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

BD_npp.plot <- ggplot(char.carbon, aes(x=BD, y=npp.fullKNZ))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="grey60", linetype=1)+
  stat_smooth(data=subset(char.carbon, Site!="f.knz"), 
              aes(x=BD,y=npp),method="lm",se=F, col="black", linetype=1)+
  xlab("Bulk Density (g cm-3)")+
  ylab("Net Primary Productivity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

g.f.ratio_npp.plot <- ggplot(char.carbon, aes(x=g.f.ratio, y=npp.fullKNZ))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="grey60", linetype=1)+
  stat_smooth(data=subset(char.carbon, Site!="f.knz"), 
              aes(x=g.f.ratio,y=npp),method="lm",se=F, col="black", linetype=1)+
  xlab("Grass:Forb abundance")+
  ylab("Net Primary Productivity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

c3.c4.ratio_npp.plot <- ggplot(char.carbon, aes(x=c3.c4.ratio, y=npp.fullKNZ))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="grey60", linetype=2)+
  stat_smooth(data=subset(char.carbon, Site!="f.knz"), 
              aes(x=c3.c4.ratio,y=npp),method="lm",se=F, col="black", linetype=2)+
  xlab("C3:C4 abundance")+
  ylab("Net Primary Productivity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

Annual_npp.plot <- ggplot(char.carbon, aes(x=Annual, y=npp.fullKNZ))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="grey60", linetype=1)+
  stat_smooth(data=subset(char.carbon, Site!="f.knz"), 
              aes(x=Annual,y=npp),method="lm",se=F, col="black", linetype=2)+
  xlab("Annual species abundance (%)")+
  ylab("Net Primary Productivity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

ggsave("Figures//Regressions//ppt_npp_regression.pdf", annual.ppt_npp.plot, width=3.5, height=3.0, unit="in")
ggsave("Figures//Regressions//temp_npp_regression.pdf", annual.temp_npp.plot, width=3.5, height=3.0, unit="in")
ggsave("Figures//Regressions//BD_npp_regression.pdf", BD_npp.plot, width=3.5, height=3.0, unit="in")
ggsave("Figures//Regressions//g_f_npp_regression.pdf", g.f.ratio_npp.plot, width=3.5, height=3.0, unit="in")
ggsave("Figures//Regressions//c3_c4_npp_regression.pdf", c3.c4.ratio_npp.plot, width=3.5, height=3.0, unit="in")
ggsave("Figures//Regressions//annual_npp_regression.pdf", Annual_npp.plot, width=3.5, height=3.0, unit="in")

# eco.rt #
annual.ppt_eco.rt.plot <- ggplot(char.carbon, aes(x=annual.ppt, y=eco.rt))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="black", linetype=2)+
  xlab("Mean annual precipitation (mm)")+
  ylab("Ecosystem residence time (years)")+
  theme(text=element_text(size=14))+
  theme_few()

annual.temp_eco.rt.plot <- ggplot(char.carbon, aes(x=annual.temp, y=eco.rt))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="black", linetype=1)+
  xlab("Mean annual temperature (C)")+
  ylab("Ecosystem residence time (years)")+
  theme(text=element_text(size=14))+
  theme_few()

BD_eco.rt.plot <- ggplot(char.carbon, aes(x=BD, y=eco.rt))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="black", linetype=2)+
  xlab("Bulk Density (g cm-3)")+
  ylab("Ecosystem residence time (years)")+
  theme(text=element_text(size=14))+
  theme_few()

g.f.ratio_eco.rt.plot <- ggplot(char.carbon, aes(x=g.f.ratio, y=eco.rt))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="black", linetype=2)+
  xlab("Grass:Forb abundance")+
  ylab("Ecosystem residence time (years)")+
  theme(text=element_text(size=14))+
  theme_few()

c3.c4.ratio_eco.rt.plot <- ggplot(char.carbon, aes(x=c3.c4.ratio, y=eco.rt))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="black", linetype=2)+
  xlab("C3:C4 abundance")+
  ylab("Ecosystem residence time (years)")+
  theme(text=element_text(size=14))+
  theme_few()

Annual_eco.rt.plot <- ggplot(char.carbon, aes(x=Annual, y=eco.rt))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="black", linetype=2)+
  xlab("Annual species abundance (%)")+
  ylab("Ecosystem residence time (years)")+
  theme(text=element_text(size=14))+
  theme_few()

ggsave("Figures//Regressions//ppt_eco.rt_regression.pdf", annual.ppt_eco.rt.plot, width=3.5, height=3.0, unit="in")
ggsave("Figures//Regressions//temp_eco.rt_regression.pdf", annual.temp_eco.rt.plot, width=3.5, height=3.0, unit="in")
ggsave("Figures//Regressions//BD_eco.rt_regression.pdf", BD_eco.rt.plot, width=3.5, height=3.0, unit="in")
ggsave("Figures//Regressions//g_f_eco.rt_regression.pdf", g.f.ratio_eco.rt.plot, width=3.5, height=3.0, unit="in")
ggsave("Figures//Regressions//c3_c4_eco.rt_regression.pdf", c3.c4.ratio_eco.rt.plot, width=3.5, height=3.0, unit="in")
ggsave("Figures//Regressions//annual_eco.rt_regression.pdf", Annual_eco.rt.plot, width=3.5, height=3.0, unit="in")

# Stepwise model selection ------------------------------------------------

# npp models based on most significant relationships #
npp.fullKNZ.model <- lm(npp.fullKNZ~annual.ppt+BD+g.f.ratio+Annual, data=char.carbon)
sink(paste0(write_path,"stepwise model selection_npp_fullKNZ.txt"))
stepAIC(npp.fullKNZ.model, direction="both")
summary(npp.fullKNZ.model)
sink()

npp.noKNZ.model <- lm(npp~annual.ppt+annual.temp, data=subset(char.carbon,Site!="f.knz"))
sink(paste0(write_path,"stepwise model selection_npp_noKNZ_2.txt"))
z <- stepAIC(npp.noKNZ.model, direction="both")
summary(z)
sink()

# npp models based on most significant relationships #
# npp.fullKNZ.model <- lm(npp.fullKNZ~annual.ppt+BD+g.f.ratio+Annual, data=char.carbon)
# sink(paste0(write_path,"stepwise model selection_npp_fullKNZ.txt"))
# stepAIC(npp.fullKNZ.model, direction="both")
# summary(npp.fullKNZ.model)
# sink()

eco.rt.noKNZ.model <- lm(eco.rt~annual.ppt*annual.temp, data=subset(char.carbon,Site!="f.knz"))
sink(paste0(write_path,"stepwise model selection_ecort_noKNZ.txt"))
z <- stepAIC(eco.rt.noKNZ.model, direction="both")
summary(z)
sink()


# Normalized regressions with MAT and MAP ---------------------------------

### Scaled models of NPP and EcoRT with MAP and MAT -- without Konza
test <- subset(char.carbon, Site != "f.knz")
zz <- varpart(test$npp, ~annual.ppt, ~annual.temp, data=test)
plot(zz)

## Combine total C with MAT and MAT and normalize
totC_climate <- site.char.all %>%
  dplyr::select(site, annual.ppt, annual.temp) %>%
  full_join(C_content_with_veg, by=c("site"="site_old")) %>%
  mutate(
    C_norm = scale(all_C_gm2),
    norm_map = scale(annual.ppt),
    norm_mat = scale(annual.temp)
  )

anova(rda(test$npp ~ annual.ppt+Condition(annual.temp), data=test))
anova(rda(test$npp ~ annual.temp+Condition(annual.ppt), data=test))
anova(rda(test$npp ~ annual.temp+annual.ppt, data=test))
anova(rda(test$eco.rt ~ annual.ppt+Condition(annual.temp), data=test))
anova(rda(test$eco.rt ~ annual.temp+Condition(annual.ppt), data=test))
anova(rda(test$eco.rt ~ annual.temp+annual.ppt, data=test))


test$norm_map <- scale(test$annual.ppt)
test$norm_mat <- scale(test$annual.temp)
test$norm_npp <- scale(test$npp)
test$norm_ecort <- scale(test$eco.rt)

npp_mod <- lm(norm_npp ~ norm_map+norm_mat, data=test)
npp_mod_summary <- summary(npp_mod)
npp_mod_anova <- Anova(npp_mod,type=3)
ecort_mod <- lm(norm_ecort ~ norm_map+norm_mat, data=test)
ecort_mod_summary <- summary(ecort_mod)
ecort_mod_anova <- Anova(ecort_mod,type=3)
c_mod <- lm(C_norm ~ norm_map+norm_mat, data=totC_climate) ### THIS HAS KONZA IN MODEL
c_mod_summary <- summary(c_mod)
c_mod_anova <- Anova(c_mod,type=3)

npp_model_out_scaled <- data.frame(dep_var = "npp",
                               Effect=rownames(npp_mod_anova),
                               Df=npp_mod_anova$Df,
                               Estimate=c(npp_mod_summary$coefficients[,1],NA),
                               F_value=npp_mod_anova$F,
                               P_value=npp_mod_anova$Pr,
                               Adj_R2=npp_mod_summary$adj.r.squared,
                               ci_90_lower=c(confint(npp_mod,level=0.9)[1:3,1],NA),
                               ci_90_upper=c(confint(npp_mod,level=0.9)[1:3,2],NA),
                               ci_95_lower=c(confint(npp_mod,level=0.95)[1:3,1],NA),
                               ci_95_upper=c(confint(npp_mod,level=0.95)[1:3,2],NA))
ecort_model_out_scaled <- data.frame(dep_var = "ecort",
                               Effect=rownames(ecort_mod_anova),
                               Df=ecort_mod_anova$Df,
                               Estimate=c(ecort_mod_summary$coefficients[,1],NA),
                               F_value=ecort_mod_anova$F,
                               P_value=ecort_mod_anova$Pr,
                               Adj_R2=ecort_mod_summary$adj.r.squared,
                               ci_90_lower=c(confint(ecort_mod,level=0.9)[1:3,1],NA),
                               ci_90_upper=c(confint(ecort_mod,level=0.9)[1:3,2],NA),
                               ci_95_lower=c(confint(ecort_mod,level=0.95)[1:3,1],NA),
                               ci_95_upper=c(confint(ecort_mod,level=0.95)[1:3,2],NA))

C_model_out_scaled <- data.frame(dep_var = "carbon",
                               Effect=rownames(c_mod_anova),
                               Df=c_mod_anova$Df,
                               Estimate=c(c_mod_summary$coefficients[,1],NA),
                               F_value=c_mod_anova$F,
                               P_value=c_mod_anova$Pr,
                               Adj_R2=c_mod_summary$adj.r.squared,
                               ci_90_lower=c(confint(c_mod,level=0.9)[1:3,1],NA),
                               ci_90_upper=c(confint(c_mod,level=0.9)[1:3,2],NA),
                               ci_95_lower=c(confint(c_mod,level=0.95)[1:3,1],NA),
                               ci_95_upper=c(confint(c_mod,level=0.95)[1:3,2],NA))

model_out_scaled <- rbind(npp_model_out_scaled, ecort_model_out_scaled, C_model_out_scaled)

write.csv(model_out_scaled, file="scaled regressions between npp or ecort and map and mat.csv")

zplot <- ggplot(subset(model_out_scaled, Effect %in% c("norm_map","norm_mat", "MAP_norm", "MAT_norm")),
       aes(x=Effect, y=Estimate, ymin=ci_90_lower, ymax=ci_90_upper)) +
  geom_hline(yintercept=0,col="grey",size=0.7) +
  geom_point(size=2) +
  geom_errorbar(width=0) +
  facet_grid(~dep_var) +
  theme_few()

cplot <- ggplot(subset(C_model_out_scaled,Effect %in% c("MAP_norm","MAT_norm")),
                       aes(x=Effect, y=Estimate, ymin=ci_90_lower, ymax=ci_90_upper)) +
  geom_hline(yintercept=0,col="grey",size=0.7) +
  geom_point(size=2) +
  geom_errorbar(width=0) +
  theme_few()

pdf(paste0("standardized slopes carbon vs map mat",Sys.Date(),".pdf"), width=1.9, height=1.75,useDingbats = F)
print(cplot)
dev.off()

ggsave(paste0("standardized slopes ecort npp map mat",Sys.Date(),".eps"),width=5,height=2,units="in")
?pdf
pdf(paste0("standardized slopes ecort npp map mat",Sys.Date(),".pdf"),useDingbats=F, height=2, width=3)
print(zplot)
dev.off()
?print
?confint

ecort_mod_summary <- summary(lm(norm_ecort ~ norm_map+norm_mat, data=test))
str(npp_mod_summary)


### Scaled models of NPP and EcoRT with MAP and MAT -- with Konza

char.carbon$norm_map <- scale(char.carbon$annual.ppt)
char.carbon$norm_mat <- scale(char.carbon$annual.temp)
char.carbon$norm_npp <- scale(char.carbon$npp.fullKNZ)
char.carbon$norm_ecort <- scale(char.carbon$eco.rt)

summary(lm(npp.fullKNZ ~ annual.ppt, data=char.carbon))
summary(lm(eco.rt ~ annual.temp, data=char.carbon))
summary(lm(all_C_gm2 ~ annual.ppt, data=totC_climate))

npp_mod <- lm(norm_npp ~ norm_map+norm_mat, data=char.carbon)
npp_mod_summary <- summary(npp_mod)
npp_mod_anova <- Anova(npp_mod,type=3)
ecort_mod <- lm(norm_ecort ~ norm_map+norm_mat, data=char.carbon)
ecort_mod_summary <- summary(ecort_mod)
ecort_mod_anova <- Anova(ecort_mod,type=3)
c_mod <- lm(C_norm ~ norm_map+norm_mat, data=totC_climate) 
c_mod_summary <- summary(c_mod)
c_mod_anova <- Anova(c_mod,type=3)


npp_model_out_scaled <- data.frame(dep_var = "npp",
                                   Effect=rownames(npp_mod_anova),
                                   Df=npp_mod_anova$Df,
                                   Estimate=c(npp_mod_summary$coefficients[,1],NA),
                                   F_value=npp_mod_anova$F,
                                   P_value=npp_mod_anova$Pr,
                                   Adj_R2=npp_mod_summary$adj.r.squared,
                                   ci_90_lower=c(confint(npp_mod,level=0.9)[1:3,1],NA),
                                   ci_90_upper=c(confint(npp_mod,level=0.9)[1:3,2],NA),
                                   ci_95_lower=c(confint(npp_mod,level=0.95)[1:3,1],NA),
                                   ci_95_upper=c(confint(npp_mod,level=0.95)[1:3,2],NA))
ecort_model_out_scaled <- data.frame(dep_var = "ecort",
                                     Effect=rownames(ecort_mod_anova),
                                     Df=ecort_mod_anova$Df,
                                     Estimate=c(ecort_mod_summary$coefficients[,1],NA),
                                     F_value=ecort_mod_anova$F,
                                     P_value=ecort_mod_anova$Pr,
                                     Adj_R2=ecort_mod_summary$adj.r.squared,
                                     ci_90_lower=c(confint(ecort_mod,level=0.9)[1:3,1],NA),
                                     ci_90_upper=c(confint(ecort_mod,level=0.9)[1:3,2],NA),
                                     ci_95_lower=c(confint(ecort_mod,level=0.95)[1:3,1],NA),
                                     ci_95_upper=c(confint(ecort_mod,level=0.95)[1:3,2],NA))
C_model_out_scaled <- data.frame(dep_var = "carbon",
                                 Effect=rownames(c_mod_anova),
                                 Df=c_mod_anova$Df,
                                 Estimate=c(c_mod_summary$coefficients[,1],NA),
                                 F_value=c_mod_anova$F,
                                 P_value=c_mod_anova$Pr,
                                 Adj_R2=c_mod_summary$adj.r.squared,
                                 ci_90_lower=c(confint(c_mod,level=0.9)[1:3,1],NA),
                                 ci_90_upper=c(confint(c_mod,level=0.9)[1:3,2],NA),
                                 ci_95_lower=c(confint(c_mod,level=0.95)[1:3,1],NA),
                                 ci_95_upper=c(confint(c_mod,level=0.95)[1:3,2],NA))

model_out_scaled <- rbind(npp_model_out_scaled, ecort_model_out_scaled, C_model_out_scaled)

write.csv(model_out_scaled, file="Regression output//scaled regressions between npp or ecort and map and mat_withKNZ.csv")

slope_plot <- ggplot(subset(model_out_scaled, Effect %in% c("norm_map","norm_mat")),
                aes(x=Effect, y=Estimate, ymin=ci_90_lower, ymax=ci_90_upper)) +
  geom_hline(yintercept=0,col="grey",size=0.7) +
  geom_point(size=2) +
  geom_errorbar(width=0) +
  facet_grid(~dep_var) +
  theme_few()

slope_plot_v2 <- ggplot(subset(model_out_scaled, Effect %in% c("norm_map","norm_mat")),
                        aes(x=Effect, y=Estimate, ymin=ci_90_lower, ymax=ci_90_upper, col=dep_var, pch=dep_var)) +
  geom_hline(yintercept=0,col="grey",size=0.7) +
  geom_point(size=2, position=position_dodge(width=0.4)) +
  geom_errorbar(width=0, position=position_dodge(width=0.4)) +
  theme_few()
  
pdf(paste0("Figures//standardized slopes ecort npp map mat_withKNZ_",Sys.Date(),".pdf"),useDingbats=F, height=2, width=4.5)
print(slope_plot)
dev.off()

pdf(paste0("Figures//standardized slopes ecort npp map mat_withKNZ_1panel",Sys.Date(),".pdf"),useDingbats=F, height=3, width=4)
print(slope_plot_v2)
dev.off()

ecort_mod_summary <- summary(lm(norm_ecort ~ norm_map+norm_mat, data=test))
str(npp_mod_summary)


# Colinearity tests -------------------------------------------------------

# colinearity tests #
sink(paste0(write_path,"colinearity results_npp_fullKNZ.txt"))
rcorr(as.matrix(char.carbon[,c("annual.ppt",
                            "annual.temp",
                            "BD",
                            "c3.c4.ratio",
                            "g.f.ratio",
                            "Annual")]))
sink()

sink(paste0(write_path,"colinearity results_npp_noKNZ.txt"))
rcorr(as.matrix(char.carbon[-6,c("annual.ppt",
                               "annual.temp",
                               "BD",
                               "c3.c4.ratio",
                               "g.f.ratio",
                               "Annual")]))
sink()

# Variance partitioning #
sink(paste0(write_path,"varpart_npp_fullKNZ.txt"))
varpart(char.carbon$npp.fullKNZ, ~annual.ppt, ~BD, ~g.f.ratio, ~Annual, data=char.carbon)
sink()

sink(paste0(write_path,"varpart_npp_noKNZ.txt"))
varpart(subset(char.carbon,Site!="f.knz")$npp, 
        ~annual.ppt, ~BD, ~g.f.ratio, 
        data=subset(char.carbon,Site!="f.knz"))
sink()

# Variance partitioning for NPP -------------------------------------------

char.noKNZ <- subset(char.carbon, Site!="f.knz")

# varpart with KNZ #
{
annual.temp.varpart <- varpart(char.carbon$npp.fullKNZ, ~annual.ppt, ~annual.temp, data=char.carbon)
BD.varpart <- varpart(char.carbon$npp.fullKNZ, ~annual.ppt, ~BD, data=char.carbon)
g.f.ratio.varpart <- varpart(char.carbon$npp.fullKNZ, ~annual.ppt, ~g.f.ratio, data=char.carbon)
c3.c4.ratio.varpart <- varpart(char.carbon$npp.fullKNZ, ~annual.ppt, ~c3.c4.ratio, data=char.carbon)
Annual.varpart <- varpart(char.carbon$npp.fullKNZ, ~annual.ppt, ~Annual, data=char.carbon)

# testing indivivual fractions #
annual.temp.X1CondX2.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ annual.ppt+Condition(annual.temp), data=char.carbon))
annual.temp.X2CondX1.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ annual.temp+Condition(annual.ppt), data=char.carbon))
annual.temp.X2plusX1.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ annual.temp+annual.ppt, data=char.carbon))

BD.X1CondX2.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ annual.ppt+Condition(BD), data=char.carbon))
BD.X2CondX1.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ BD+Condition(annual.ppt), data=char.carbon))
BD.X2plusX1.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ BD+annual.ppt, data=char.carbon))

g.f.ratio.X1CondX2.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ annual.ppt+Condition(g.f.ratio), data=char.carbon))
g.f.ratio.X2CondX1.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ g.f.ratio+Condition(annual.ppt), data=char.carbon))
g.f.ratio.X2plusX1.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ g.f.ratio+annual.ppt, data=char.carbon))

c3.c4.ratio.X1CondX2.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ annual.ppt+Condition(c3.c4.ratio), data=char.carbon))
c3.c4.ratio.X2CondX1.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ c3.c4.ratio+Condition(annual.ppt), data=char.carbon))
c3.c4.ratio.X2plusX1.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ c3.c4.ratio+annual.ppt, data=char.carbon))

Annual.X1CondX2.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ annual.ppt+Condition(Annual), data=char.carbon))
Annual.X2CondX1.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ Annual+Condition(annual.ppt), data=char.carbon))
Annual.X2plusX1.withKNZ <- anova(rda(char.carbon$npp.fullKNZ ~ Annual+annual.ppt, data=char.carbon))

# Create text files with varpart output #
sink(paste0(varpart_path, "AP AT vs NPP varpart_withKNZ.txt"))
annual.temp.varpart
anova(rda(char.carbon$npp.fullKNZ ~ annual.ppt+Condition(annual.temp), data=char.carbon))
anova(rda(char.carbon$npp.fullKNZ ~ annual.temp+Condition(annual.ppt), data=char.carbon))
anova(rda(char.carbon$npp.fullKNZ ~ annual.temp+annual.ppt, data=char.carbon))
sink()

sink(paste0(varpart_path, "AP BD vs NPP varpart_withKNZ.txt"))
BD.varpart
anova(rda(char.carbon$npp.fullKNZ ~ annual.ppt+Condition(BD), data=char.carbon))
anova(rda(char.carbon$npp.fullKNZ ~ BD+Condition(annual.ppt), data=char.carbon))
anova(rda(char.carbon$npp.fullKNZ ~ BD+annual.ppt, data=char.carbon))
sink()

sink(paste0(varpart_path, "AP G-F vs NPP varpart_withKNZ.txt"))
g.f.ratio.varpart
anova(rda(char.carbon$npp.fullKNZ ~ annual.ppt+Condition(g.f.ratio), data=char.carbon))
anova(rda(char.carbon$npp.fullKNZ ~ g.f.ratio+Condition(annual.ppt), data=char.carbon))
anova(rda(char.carbon$npp.fullKNZ ~ g.f.ratio+annual.ppt, data=char.carbon))
sink()

sink(paste0(varpart_path, "AP c3-c4 vs NPP varpart_withKNZ.txt"))
c3.c4.ratio.varpart
anova(rda(char.carbon$npp.fullKNZ ~ annual.ppt+Condition(c3.c4.ratio), data=char.carbon))
anova(rda(char.carbon$npp.fullKNZ ~ c3.c4.ratio+Condition(annual.ppt), data=char.carbon))
anova(rda(char.carbon$npp.fullKNZ ~ c3.c4.ratio+annual.ppt, data=char.carbon))
sink()

sink(paste0(varpart_path, "AP AnnCov vs NPP varpart_withKNZ.txt"))
Annual.varpart
anova(rda(char.carbon$npp.fullKNZ ~ annual.ppt+Condition(Annual), data=char.carbon))
anova(rda(char.carbon$npp.fullKNZ ~ Annual+Condition(annual.ppt), data=char.carbon))
anova(rda(char.carbon$npp.fullKNZ ~ Annual+annual.ppt, data=char.carbon))
sink()

# Plotting varpart output #

plot(annual.temp.varpart)
plot(BD.varpart)
plot(g.f.ratio.varpart)
plot(c3.c4.ratio.varpart)
plot(Annual.varpart)
}

# varpart noKNZ #
{
annual.temp.varpart.noKNZ <- varpart(char.noKNZ$npp, ~annual.ppt, ~annual.temp, data=char.noKNZ)
BD.varpart.noKNZ <- varpart(char.noKNZ$npp, ~annual.ppt, ~BD, data=char.noKNZ)
g.f.ratio.varpart.noKNZ <- varpart(char.noKNZ$npp, ~annual.ppt, ~g.f.ratio, data=char.noKNZ)
c3.c4.ratio.varpart.noKNZ <- varpart(char.noKNZ$npp, ~annual.ppt, ~c3.c4.ratio, data=char.noKNZ)
Annual.varpart.noKNZ <- varpart(char.noKNZ$npp, ~annual.ppt, ~Annual, data=char.noKNZ)

# testing indivivual fractions #
annual.temp.X1CondX2.noKNZ <- anova(rda(char.noKNZ$npp ~ annual.ppt+Condition(annual.temp), data=char.noKNZ))
annual.temp.X2CondX1.noKNZ <- anova(rda(char.noKNZ$npp ~ annual.temp+Condition(annual.ppt), data=char.noKNZ))
annual.temp.X2plusX1.noKNZ <- anova(rda(char.noKNZ$npp ~ annual.temp+annual.ppt, data=char.noKNZ))

BD.X1CondX2.noKNZ <- anova(rda(char.noKNZ$npp ~ annual.ppt+Condition(BD), data=char.noKNZ))
BD.X2CondX1.noKNZ <- anova(rda(char.noKNZ$npp ~ BD+Condition(annual.ppt), data=char.noKNZ))
BD.X2plusX1.noKNZ <- anova(rda(char.noKNZ$npp ~ BD+annual.ppt, data=char.noKNZ))

g.f.ratio.X1CondX2.noKNZ <- anova(rda(char.noKNZ$npp ~ annual.ppt+Condition(g.f.ratio), data=char.noKNZ))
g.f.ratio.X2CondX1.noKNZ <- anova(rda(char.noKNZ$npp ~ g.f.ratio+Condition(annual.ppt), data=char.noKNZ))
g.f.ratio.X2plusX1.noKNZ <- anova(rda(char.noKNZ$npp ~ g.f.ratio+annual.ppt, data=char.noKNZ))

c3.c4.ratio.X1CondX2.noKNZ <- anova(rda(char.noKNZ$npp ~ annual.ppt+Condition(c3.c4.ratio), data=char.noKNZ))
c3.c4.ratio.X2CondX1.noKNZ <- anova(rda(char.noKNZ$npp ~ c3.c4.ratio+Condition(annual.ppt), data=char.noKNZ))
c3.c4.ratio.X2plusX1.noKNZ <- anova(rda(char.noKNZ$npp ~ c3.c4.ratio+annual.ppt, data=char.noKNZ))

Annual.X1CondX2.noKNZ <- anova(rda(char.noKNZ$npp ~ annual.ppt+Condition(Annual), data=char.noKNZ))
Annual.X2CondX1.noKNZ <- anova(rda(char.noKNZ$npp ~ Annual+Condition(annual.ppt), data=char.noKNZ))
Annual.X2plusX1.noKNZ <- anova(rda(char.noKNZ$npp ~ Annual+annual.ppt, data=char.noKNZ))

# Create text files with varpart output #
sink(paste0(varpart_path, "AP AT vs NPP varpart_noKNZ.txt"))
annual.temp.varpart.noKNZ
anova(rda(char.noKNZ$npp ~ annual.ppt+Condition(annual.temp), data=char.noKNZ))
anova(rda(char.noKNZ$npp ~ annual.temp+Condition(annual.ppt), data=char.noKNZ))
anova(rda(char.noKNZ$npp ~ annual.temp+annual.ppt, data=char.noKNZ))
sink()

sink(paste0(varpart_path, "AP BD vs NPP varpart_noKNZ.txt"))
BD.varpart.noKNZ
anova(rda(char.noKNZ$npp ~ annual.ppt+Condition(BD), data=char.noKNZ))
anova(rda(char.noKNZ$npp ~ BD+Condition(annual.ppt), data=char.noKNZ))
anova(rda(char.noKNZ$npp ~ BD+annual.ppt, data=char.noKNZ))
sink()

sink(paste0(varpart_path, "AP G-F vs NPP varpart_noKNZ.txt"))
g.f.ratio.varpart.noKNZ
anova(rda(char.noKNZ$npp ~ annual.ppt+Condition(g.f.ratio), data=char.noKNZ))
anova(rda(char.noKNZ$npp ~ g.f.ratio+Condition(annual.ppt), data=char.noKNZ))
anova(rda(char.noKNZ$npp ~ g.f.ratio+annual.ppt, data=char.noKNZ))
sink()

sink(paste0(varpart_path, "AP c3-c4 vs NPP varpart_noKNZ.txt"))
c3.c4.ratio.varpart.noKNZ
anova(rda(char.noKNZ$npp ~ annual.ppt+Condition(c3.c4.ratio), data=char.noKNZ))
anova(rda(char.noKNZ$npp ~ c3.c4.ratio+Condition(annual.ppt), data=char.noKNZ))
anova(rda(char.noKNZ$npp ~ c3.c4.ratio+annual.ppt, data=char.noKNZ))
sink()

sink(paste0(varpart_path, "AP AnnCov vs NPP varpart_noKNZ.txt"))
Annual.varpart.noKNZ
anova(rda(char.noKNZ$npp ~ annual.ppt+Condition(Annual), data=char.noKNZ))
anova(rda(char.noKNZ$npp ~ Annual+Condition(annual.ppt), data=char.noKNZ))
anova(rda(char.noKNZ$npp ~ Annual+annual.ppt, data=char.noKNZ))
sink()

plot(annual.temp.varpart.noKNZ)
plot(BD.varpart.noKNZ)
plot(g.f.ratio.varpart.noKNZ)
plot(c3.c4.ratio.varpart.noKNZ)
plot(Annual.varpart.noKNZ)
}

# Create table with output #
# varpart table #
varpart.table <- data.frame(data=c(rep("withKNZ",20),rep("noKNZ",20)),
                            variable1=rep("annual.ppt",40),
                            variable2=c(rep("annual.temp",4),
                                        rep("BD",4),
                                        rep("g.f.ratio",4),
                                        rep("c3.c4.ratio",4),
                                        rep("Annual",4)),
                            test=c("X1|X2","jointX1X2","X2|X1","Residuals"),
                            AdjR2=0,
                            Fval=0,
                            Pval=0)
# withKNZ #                            
varpart.table$AdjR2[varpart.table$data=="withKNZ"&varpart.table$variable2=="annual.temp"] <- 
  annual.temp.varpart$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="withKNZ"&varpart.table$variable2=="annual.temp"] <-
  c(annual.temp.X1CondX2.withKNZ$F[1], NA, annual.temp.X2CondX1.withKNZ$F[1], annual.temp.X2plusX1.withKNZ$F[1])
varpart.table$Pval[varpart.table$data=="withKNZ"&varpart.table$variable2=="annual.temp"] <-
  c(annual.temp.X1CondX2.withKNZ$Pr[1], NA, annual.temp.X2CondX1.withKNZ$Pr[1], annual.temp.X2plusX1.withKNZ$Pr[1])

varpart.table$AdjR2[varpart.table$data=="withKNZ"&varpart.table$variable2=="BD"] <- 
  BD.varpart$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="withKNZ"&varpart.table$variable2=="BD"] <-
  c(BD.X1CondX2.withKNZ$F[1], NA, BD.X2CondX1.withKNZ$F[1], BD.X2plusX1.withKNZ$F[1])
varpart.table$Pval[varpart.table$data=="withKNZ"&varpart.table$variable2=="BD"] <-
  c(BD.X1CondX2.withKNZ$Pr[1], NA, BD.X2CondX1.withKNZ$Pr[1], BD.X2plusX1.withKNZ$Pr[1])

varpart.table$AdjR2[varpart.table$data=="withKNZ"&varpart.table$variable2=="g.f.ratio"] <- 
  g.f.ratio.varpart$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="withKNZ"&varpart.table$variable2=="g.f.ratio"] <-
  c(g.f.ratio.X1CondX2.withKNZ$F[1], NA, g.f.ratio.X2CondX1.withKNZ$F[1], g.f.ratio.X2plusX1.withKNZ$F[1])
varpart.table$Pval[varpart.table$data=="withKNZ"&varpart.table$variable2=="g.f.ratio"] <-
  c(g.f.ratio.X1CondX2.withKNZ$Pr[1], NA, g.f.ratio.X2CondX1.withKNZ$Pr[1], g.f.ratio.X2plusX1.withKNZ$Pr[1])

varpart.table$AdjR2[varpart.table$data=="withKNZ"&varpart.table$variable2=="c3.c4.ratio"] <- 
  c3.c4.ratio.varpart$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="withKNZ"&varpart.table$variable2=="c3.c4.ratio"] <-
  c(c3.c4.ratio.X1CondX2.withKNZ$F[1], NA, c3.c4.ratio.X2CondX1.withKNZ$F[1], c3.c4.ratio.X2plusX1.withKNZ$F[1])
varpart.table$Pval[varpart.table$data=="withKNZ"&varpart.table$variable2=="c3.c4.ratio"] <-
  c(c3.c4.ratio.X1CondX2.withKNZ$Pr[1], NA, c3.c4.ratio.X2CondX1.withKNZ$Pr[1], c3.c4.ratio.X2plusX1.withKNZ$Pr[1])

varpart.table$AdjR2[varpart.table$data=="withKNZ"&varpart.table$variable2=="Annual"] <- 
  Annual.varpart$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="withKNZ"&varpart.table$variable2=="Annual"] <-
  c(Annual.X1CondX2.withKNZ$F[1], NA, Annual.X2CondX1.withKNZ$F[1], Annual.X2plusX1.withKNZ$F[1])
varpart.table$Pval[varpart.table$data=="withKNZ"&varpart.table$variable2=="Annual"] <-
  c(Annual.X1CondX2.withKNZ$Pr[1], NA, Annual.X2CondX1.withKNZ$Pr[1], Annual.X2plusX1.withKNZ$Pr[1])

# noKNZ #
varpart.table$AdjR2[varpart.table$data=="noKNZ"&varpart.table$variable2=="annual.temp"] <- 
  annual.temp.varpart.noKNZ$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="noKNZ"&varpart.table$variable2=="annual.temp"] <-
  c(annual.temp.X1CondX2.noKNZ$F[1], NA, annual.temp.X2CondX1.noKNZ$F[1], annual.temp.X2plusX1.noKNZ$F[1])
varpart.table$Pval[varpart.table$data=="noKNZ"&varpart.table$variable2=="annual.temp"] <-
  c(annual.temp.X1CondX2.noKNZ$Pr[1], NA, annual.temp.X2CondX1.noKNZ$Pr[1], annual.temp.X2plusX1.noKNZ$Pr[1])

varpart.table$AdjR2[varpart.table$data=="noKNZ"&varpart.table$variable2=="BD"] <- 
  BD.varpart.noKNZ$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="noKNZ"&varpart.table$variable2=="BD"] <-
  c(BD.X1CondX2.noKNZ$F[1], NA, BD.X2CondX1.noKNZ$F[1], BD.X2plusX1.noKNZ$F[1])
varpart.table$Pval[varpart.table$data=="noKNZ"&varpart.table$variable2=="BD"] <-
  c(BD.X1CondX2.noKNZ$Pr[1], NA, BD.X2CondX1.noKNZ$Pr[1], BD.X2plusX1.noKNZ$Pr[1])

varpart.table$AdjR2[varpart.table$data=="noKNZ"&varpart.table$variable2=="g.f.ratio"] <- 
  g.f.ratio.varpart.noKNZ$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="noKNZ"&varpart.table$variable2=="g.f.ratio"] <-
  c(g.f.ratio.X1CondX2.noKNZ$F[1], NA, g.f.ratio.X2CondX1.noKNZ$F[1], g.f.ratio.X2plusX1.noKNZ$F[1])
varpart.table$Pval[varpart.table$data=="noKNZ"&varpart.table$variable2=="g.f.ratio"] <-
  c(g.f.ratio.X1CondX2.noKNZ$Pr[1], NA, g.f.ratio.X2CondX1.noKNZ$Pr[1], g.f.ratio.X2plusX1.noKNZ$Pr[1])

varpart.table$AdjR2[varpart.table$data=="noKNZ"&varpart.table$variable2=="c3.c4.ratio"] <- 
  c3.c4.ratio.varpart.noKNZ$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="noKNZ"&varpart.table$variable2=="c3.c4.ratio"] <-
  c(c3.c4.ratio.X1CondX2.noKNZ$F[1], NA, c3.c4.ratio.X2CondX1.noKNZ$F[1], c3.c4.ratio.X2plusX1.noKNZ$F[1])
varpart.table$Pval[varpart.table$data=="noKNZ"&varpart.table$variable2=="c3.c4.ratio"] <-
  c(c3.c4.ratio.X1CondX2.noKNZ$Pr[1], NA, c3.c4.ratio.X2CondX1.noKNZ$Pr[1], c3.c4.ratio.X2plusX1.noKNZ$Pr[1])

varpart.table$AdjR2[varpart.table$data=="noKNZ"&varpart.table$variable2=="Annual"] <- 
  Annual.varpart.noKNZ$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="noKNZ"&varpart.table$variable2=="Annual"] <-
  c(Annual.X1CondX2.noKNZ$F[1], NA, Annual.X2CondX1.noKNZ$F[1], Annual.X2plusX1.noKNZ$F[1])
varpart.table$Pval[varpart.table$data=="noKNZ"&varpart.table$variable2=="Annual"] <-
  c(Annual.X1CondX2.noKNZ$Pr[1], NA, Annual.X2CondX1.noKNZ$Pr[1], Annual.X2plusX1.noKNZ$Pr[1])


write.csv(varpart.table, file=paste0(varpart_path, "varpart npp_table_Nov2017.csv"), row.names=F)






# Variance partitioning for EcoRT -------------------------------------------

# varpart with KNZ #
{
  annual.ppt.varpart <- varpart(char.carbon$eco.rt, ~annual.temp, ~annual.ppt, data=char.carbon)
  BD.varpart <- varpart(char.carbon$eco.rt, ~annual.temp, ~BD, data=char.carbon)
  g.f.ratio.varpart <- varpart(char.carbon$eco.rt, ~annual.temp, ~g.f.ratio, data=char.carbon)
  c3.c4.ratio.varpart <- varpart(char.carbon$eco.rt, ~annual.temp, ~c3.c4.ratio, data=char.carbon)
  Annual.varpart <- varpart(char.carbon$eco.rt, ~annual.temp, ~Annual, data=char.carbon)
  
  # testing indivivual fractions #
  annual.ppt.X1CondX2.withKNZ <- anova(rda(char.carbon$eco.rt ~ annual.temp+Condition(annual.ppt), data=char.carbon))
  annual.ppt.X2CondX1.withKNZ <- anova(rda(char.carbon$eco.rt ~ annual.ppt+Condition(annual.temp), data=char.carbon))
  annual.ppt.X2plusX1.withKNZ <- anova(rda(char.carbon$eco.rt ~ annual.temp+annual.ppt, data=char.carbon))
  
  BD.X1CondX2.withKNZ <- anova(rda(char.carbon$eco.rt ~ annual.temp+Condition(BD), data=char.carbon))
  BD.X2CondX1.withKNZ <- anova(rda(char.carbon$eco.rt ~ BD+Condition(annual.temp), data=char.carbon))
  BD.X2plusX1.withKNZ <- anova(rda(char.carbon$eco.rt ~ BD+annual.temp, data=char.carbon))
  
  g.f.ratio.X1CondX2.withKNZ <- anova(rda(char.carbon$eco.rt ~ annual.temp+Condition(g.f.ratio), data=char.carbon))
  g.f.ratio.X2CondX1.withKNZ <- anova(rda(char.carbon$eco.rt ~ g.f.ratio+Condition(annual.temp), data=char.carbon))
  g.f.ratio.X2plusX1.withKNZ <- anova(rda(char.carbon$eco.rt ~ g.f.ratio+annual.temp, data=char.carbon))
  
  c3.c4.ratio.X1CondX2.withKNZ <- anova(rda(char.carbon$eco.rt ~ annual.temp+Condition(c3.c4.ratio), data=char.carbon))
  c3.c4.ratio.X2CondX1.withKNZ <- anova(rda(char.carbon$eco.rt ~ c3.c4.ratio+Condition(annual.temp), data=char.carbon))
  c3.c4.ratio.X2plusX1.withKNZ <- anova(rda(char.carbon$eco.rt ~ c3.c4.ratio+annual.temp, data=char.carbon))
  
  Annual.X1CondX2.withKNZ <- anova(rda(char.carbon$eco.rt ~ annual.temp+Condition(Annual), data=char.carbon))
  Annual.X2CondX1.withKNZ <- anova(rda(char.carbon$eco.rt ~ Annual+Condition(annual.temp), data=char.carbon))
  Annual.X2plusX1.withKNZ <- anova(rda(char.carbon$eco.rt ~ Annual+annual.temp, data=char.carbon))
  
  # Create text files with varpart output #
  sink(paste0(varpart_path, "AT AP vs EcoRT varpart_withKNZ.txt"))
  annual.temp.varpart
  anova(rda(char.carbon$eco.rt ~ annual.temp+Condition(annual.ppt), data=char.carbon))
  anova(rda(char.carbon$eco.rt ~ annual.ppt+Condition(annual.temp), data=char.carbon))
  anova(rda(char.carbon$eco.rt ~ annual.temp+annual.ppt, data=char.carbon))
  sink()
  
  sink(paste0(varpart_path, "AT BD vs EcoRT varpart_withKNZ.txt"))
  BD.varpart
  anova(rda(char.carbon$eco.rt ~ annual.temp+Condition(BD), data=char.carbon))
  anova(rda(char.carbon$eco.rt ~ BD+Condition(annual.temp), data=char.carbon))
  anova(rda(char.carbon$eco.rt ~ BD+annual.temp, data=char.carbon))
  sink()
  
  sink(paste0(varpart_path, "AT G-F vs EcoRT varpart_withKNZ.txt"))
  g.f.ratio.varpart
  anova(rda(char.carbon$eco.rt ~ annual.temp+Condition(g.f.ratio), data=char.carbon))
  anova(rda(char.carbon$eco.rt ~ g.f.ratio+Condition(annual.temp), data=char.carbon))
  anova(rda(char.carbon$eco.rt ~ g.f.ratio+annual.temp, data=char.carbon))
  sink()
  
  sink(paste0(varpart_path, "AT c3-c4 vs EcoRT varpart_withKNZ.txt"))
  c3.c4.ratio.varpart
  anova(rda(char.carbon$eco.rt ~ annual.temp+Condition(c3.c4.ratio), data=char.carbon))
  anova(rda(char.carbon$eco.rt ~ c3.c4.ratio+Condition(annual.temp), data=char.carbon))
  anova(rda(char.carbon$eco.rt ~ c3.c4.ratio+annual.temp, data=char.carbon))
  sink()
  
  sink(paste0(varpart_path, "AT AnnCov vs EcoRT varpart_withKNZ.txt"))
  Annual.varpart
  anova(rda(char.carbon$eco.rt ~ annual.temp+Condition(Annual), data=char.carbon))
  anova(rda(char.carbon$eco.rt ~ Annual+Condition(annual.temp), data=char.carbon))
  anova(rda(char.carbon$eco.rt ~ Annual+annual.temp, data=char.carbon))
  sink()
  
  # Plotting varpart output #
  
  plot(annual.ppt.varpart)
  plot(BD.varpart)
  plot(g.f.ratio.varpart)
  plot(c3.c4.ratio.varpart)
  plot(Annual.varpart)
}


# Create table with output #
# varpart table #
varpart.table <- data.frame(data=c(rep("withKNZ",20)),
                            variable1=rep("annual.temp",20),
                            variable2=c(rep("annual.ppt",4),
                                        rep("BD",4),
                                        rep("g.f.ratio",4),
                                        rep("c3.c4.ratio",4),
                                        rep("Annual",4)),
                            test=c("X1|X2","jointX1X2","X2|X1","Residuals"),
                            AdjR2=0,
                            Fval=0,
                            Pval=0)
# withKNZ #                            
varpart.table$AdjR2[varpart.table$data=="withKNZ"&varpart.table$variable2=="annual.ppt"] <- 
  annual.ppt.varpart$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="withKNZ"&varpart.table$variable2=="annual.ppt"] <-
  c(annual.ppt.X1CondX2.withKNZ$F[1], NA, annual.ppt.X2CondX1.withKNZ$F[1], annual.ppt.X2plusX1.withKNZ$F[1])
varpart.table$Pval[varpart.table$data=="withKNZ"&varpart.table$variable2=="annual.ppt"] <-
  c(annual.ppt.X1CondX2.withKNZ$Pr[1], NA, annual.ppt.X2CondX1.withKNZ$Pr[1], annual.ppt.X2plusX1.withKNZ$Pr[1])

varpart.table$AdjR2[varpart.table$data=="withKNZ"&varpart.table$variable2=="BD"] <- 
  BD.varpart$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="withKNZ"&varpart.table$variable2=="BD"] <-
  c(BD.X1CondX2.withKNZ$F[1], NA, BD.X2CondX1.withKNZ$F[1], BD.X2plusX1.withKNZ$F[1])
varpart.table$Pval[varpart.table$data=="withKNZ"&varpart.table$variable2=="BD"] <-
  c(BD.X1CondX2.withKNZ$Pr[1], NA, BD.X2CondX1.withKNZ$Pr[1], BD.X2plusX1.withKNZ$Pr[1])

varpart.table$AdjR2[varpart.table$data=="withKNZ"&varpart.table$variable2=="g.f.ratio"] <- 
  g.f.ratio.varpart$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="withKNZ"&varpart.table$variable2=="g.f.ratio"] <-
  c(g.f.ratio.X1CondX2.withKNZ$F[1], NA, g.f.ratio.X2CondX1.withKNZ$F[1], g.f.ratio.X2plusX1.withKNZ$F[1])
varpart.table$Pval[varpart.table$data=="withKNZ"&varpart.table$variable2=="g.f.ratio"] <-
  c(g.f.ratio.X1CondX2.withKNZ$Pr[1], NA, g.f.ratio.X2CondX1.withKNZ$Pr[1], g.f.ratio.X2plusX1.withKNZ$Pr[1])

varpart.table$AdjR2[varpart.table$data=="withKNZ"&varpart.table$variable2=="c3.c4.ratio"] <- 
  c3.c4.ratio.varpart$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="withKNZ"&varpart.table$variable2=="c3.c4.ratio"] <-
  c(c3.c4.ratio.X1CondX2.withKNZ$F[1], NA, c3.c4.ratio.X2CondX1.withKNZ$F[1], c3.c4.ratio.X2plusX1.withKNZ$F[1])
varpart.table$Pval[varpart.table$data=="withKNZ"&varpart.table$variable2=="c3.c4.ratio"] <-
  c(c3.c4.ratio.X1CondX2.withKNZ$Pr[1], NA, c3.c4.ratio.X2CondX1.withKNZ$Pr[1], c3.c4.ratio.X2plusX1.withKNZ$Pr[1])

varpart.table$AdjR2[varpart.table$data=="withKNZ"&varpart.table$variable2=="Annual"] <- 
  Annual.varpart$part$indfract$Adj.R.squared
varpart.table$Fval[varpart.table$data=="withKNZ"&varpart.table$variable2=="Annual"] <-
  c(Annual.X1CondX2.withKNZ$F[1], NA, Annual.X2CondX1.withKNZ$F[1], Annual.X2plusX1.withKNZ$F[1])
varpart.table$Pval[varpart.table$data=="withKNZ"&varpart.table$variable2=="Annual"] <-
  c(Annual.X1CondX2.withKNZ$Pr[1], NA, Annual.X2CondX1.withKNZ$Pr[1], Annual.X2plusX1.withKNZ$Pr[1])


write.csv(varpart.table, file=paste0(varpart_path, "varpart EcoRT_table_Nov2017.csv"), row.names=F)






# Regression between parameters and site characteristics ------------------

pMLE <- read.csv("DA Diagnostics and MLEs\\MLE values all parameters all plots_4 north sites_July2017.csv")
head(char.carbon)
head(pMLE)
pMLE$Site <- paste(letters[1:6],pMLE$Site,sep=".")
char.carbon$Site <- as.character(char.carbon$Site)
MLE.char <- merge(pMLE, char.carbon[,c("Site",
                                       "annual.ppt",
                                       "annual.temp",
                                       "BD",
                                       "g.f.ratio",
                                       "c3.c4.ratio",
                                       "Annual")],
                  by="Site", all=T)
# c1 #
summary(lm(c1~annual.ppt,data=MLE.char))
summary(lm(c1~annual.temp,data=MLE.char))
summary(lm(c1~BD,data=MLE.char))
summary(lm(c1~g.f.ratio,data=MLE.char))
summary(lm(c1~c3.c4.ratio,data=MLE.char))
summary(lm(c1~Annual,data=MLE.char))
# c2 #
summary(lm(c2~annual.ppt,data=MLE.char))
summary(lm(c2~annual.temp,data=MLE.char))
summary(lm(c2~BD,data=MLE.char))
summary(lm(c2~g.f.ratio,data=MLE.char))
summary(lm(c2~c3.c4.ratio,data=MLE.char))
summary(lm(c2~Annual,data=MLE.char))
# c3 #
summary(lm(c3~annual.ppt,data=MLE.char))
summary(lm(c3~annual.temp,data=MLE.char))
summary(lm(c3~BD,data=MLE.char))
summary(lm(c3~g.f.ratio,data=MLE.char))
summary(lm(c3~c3.c4.ratio,data=MLE.char))
summary(lm(c3~Annual,data=MLE.char))
# c4 #
summary(lm(c4~annual.ppt,data=MLE.char))
summary(lm(c4~annual.temp,data=MLE.char))
summary(lm(c4~BD,data=MLE.char))
summary(lm(c4~g.f.ratio,data=MLE.char))
summary(lm(c4~c3.c4.ratio,data=MLE.char))
summary(lm(c4~Annual,data=MLE.char))
# c5 #
summary(lm(c5~annual.ppt,data=MLE.char))
summary(lm(c5~annual.temp,data=MLE.char))
summary(lm(c5~BD,data=MLE.char))
summary(lm(c5~g.f.ratio,data=MLE.char))
summary(lm(c5~c3.c4.ratio,data=MLE.char))
summary(lm(c5~Annual,data=MLE.char))
# c6 #
summary(lm(c6~annual.ppt,data=MLE.char))
summary(lm(c6~annual.temp,data=MLE.char))
summary(lm(c6~BD,data=MLE.char))
summary(lm(c6~g.f.ratio,data=MLE.char))
summary(lm(c6~c3.c4.ratio,data=MLE.char))
summary(lm(c6~Annual,data=MLE.char))

# mscut #
summary(lm(mscut~annual.ppt,data=MLE.char))
summary(lm(mscut~annual.temp,data=MLE.char))
summary(lm(mscut~BD,data=MLE.char))
summary(lm(mscut~g.f.ratio,data=MLE.char))
summary(lm(mscut~c3.c4.ratio,data=MLE.char))
summary(lm(mscut~Annual,data=MLE.char))

# Q10 #
summary(lm(Q10~annual.ppt,data=MLE.char))
summary(lm(Q10~annual.temp,data=MLE.char))
summary(lm(Q10~BD,data=MLE.char))
summary(lm(Q10~g.f.ratio,data=MLE.char))
summary(lm(Q10~c3.c4.ratio,data=MLE.char))
summary(lm(Q10~Annual,data=MLE.char))


ggplot(MLE.char, aes(x=annual.ppt, y=Q10))+
  geom_point()+
  stat_smooth(method="lm",se=F)
ggplot(MLE.char, aes(x=g.f.ratio, y=Q10))+
  geom_point()+
  stat_smooth(method="lm",se=F)
ggplot(MLE.char, aes(x=Annual, y=Q10))+
  geom_point()+
  stat_smooth(method="lm",se=F)

library(vegan)
varpart(MLE.char$Q10, ~annual.ppt, ~g.f.ratio, data=MLE.char)
varpart(MLE.char$Q10, ~annual.ppt, ~Annual, data=MLE.char)
varpart(MLE.char$Q10, ~g.f.ratio, ~Annual, data=MLE.char)
plot(varpart(MLE.char$Q10, ~annual.ppt, ~g.f.ratio, ~Annual, data=MLE.char))

head(char.carbon)
ggplot(char.carbon, aes(x=annual.ppt, y=c.cap))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="grey60")+
  stat_smooth(data=subset(char.carbon, Site!="f.knz"), 
              aes(x=annual.ppt,y=c.cap),method="lm",se=F, col="black")+
  xlab("Mean annual precipitation (mm)")+
  ylab("Carbon capacity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

ggplot(char.carbon, aes(x=annual.temp, y=c.cap))+
  geom_point(size=3)+
  stat_smooth(method="lm", se=F, col="grey60")+
  stat_smooth(data=subset(char.carbon, Site!="f.knz"), 
              aes(x=annual.temp,y=c.cap),method="lm",se=F, col="black")+
  xlab("Mean annual precipitation (mm)")+
  ylab("Carbon capacity (g m-2)")+
  theme(text=element_text(size=14))+
  theme_few()

varpart(char.carbon$c.cap, ~annual.ppt, ~annual.temp, data=char.carbon)
anova(rda(char.carbon$c.cap ~ annual.temp+Condition(annual.ppt), data=char.carbon))
