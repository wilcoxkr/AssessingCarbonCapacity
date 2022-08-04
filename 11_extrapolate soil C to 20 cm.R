### Estimate soil C from 0-20 using C by depth curves and soil C from 0-10 cm measurements
### Author: Kevin Wilcox (wilcoxkr@gmail.com)
###
### Last updated: March 1, 2018
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(plyr)

setwd("C:\\Users\\Kevin.Wilcox\\Desktop\\fromOldComp\\FULL_ANALYSES_July2017\\")

c_prop <- read.csv("SoilC\\jobaggy and iscn c distribution.csv")

### Take a look at the distributions
display.brewer.all()
col_vec <- brewer.pal(6,"Dark2")

c_prop$source <- factor(c_prop$source, levels=c("Temp grasslands","SEV","CPER","HPG","HAR","KNZ"))

c_depth <- ggplot(c_prop, aes(x=depth, y=cum_prop, col=source)) +
  geom_point(size=3) +
  geom_path() +
  theme_few() +
  scale_color_manual(values=col_vec) +
  xlab("Soil Depth (cm)") +
  ylab("Cumulative proportion of soil C")

png(paste0("Figures//C distribution by depth_",Sys.Date(),".png"), width=5.5, height=4, res=600, units="in")
print(c_depth)
dev.off()

pdf(paste0("Figures//C distribution by depth_",Sys.Date(),".pdf"), width=5.5, height=4, useDingbats = F)
print(c_depth)
dev.off()

### Separate out each site
sev_carbon <- filter(c_prop, source=="SEV")
cper_carbon <- filter(c_prop, source=="CPER")
hpg_carbon <- filter(c_prop, source=="HPG")
hys_carbon <- filter(c_prop, source=="HAR")
knz_carbon <- filter(c_prop, source=="KNZ")

### run non-linear models (cum_prop_C ~ 1-B^d) for each site

sev_model <- nls(cum_prop ~ 1-b^depth, data=sev_carbon, start=list(b=0.95))
cper_model <- nls(cum_prop ~ 1-b^depth, data=cper_carbon, start=list(b=0.95))
hpg_model <- nls(cum_prop ~ 1-b^depth, data=hpg_carbon, start=list(b=0.95))
hys_model <- nls(cum_prop ~ 1-b^depth, data=hys_carbon, start=list(b=0.95))
knz_model <- nls(cum_prop ~ 1-b^depth, data=knz_carbon, start=list(b=0.95))

### Set coefficients for each site
sev_beta <- coef(sev_model)
cper_beta <- coef(cper_model)
hpg_beta <- coef(hpg_model)
hys_beta <- coef(hys_model)
knz_beta <- coef(knz_model)

### create trend-lines
sev_xhat <- 0:max(sev_carbon$depth)
sev_yhat <- 1 - sev_beta^sev_xhat

cper_xhat <- 0:max(cper_carbon$depth)
cper_yhat <- 1 - cper_beta^cper_xhat

hpg_xhat <- 0:max(hpg_carbon$depth)
hpg_yhat <- 1 - hpg_beta^hpg_xhat

hys_xhat <- 0:max(hys_carbon$depth)
hys_yhat <- 1 - hys_beta^hys_xhat

knz_xhat <- 0:max(knz_carbon$depth)
knz_yhat <- 1 - knz_beta^knz_xhat

### Plot to check
with(sev_carbon, plot(depth, cum_prop, xlim=c(0,200)))
lines(sev_xhat, sev_yhat, col="red3")

with(cper_carbon, plot(depth, cum_prop, xlim=c(0,200)))
lines(cper_xhat, cper_yhat, col="red3")

with(hpg_carbon, plot(depth, cum_prop, xlim=c(0,200)))
lines(hpg_xhat, hpg_yhat, col="red3")

with(hys_carbon, plot(depth, cum_prop, xlim=c(0,200)))
lines(hys_xhat, hys_yhat, col="red3")

with(knz_carbon, plot(depth, cum_prop, xlim=c(0,200)))
lines(knz_xhat, knz_yhat, col="red3")


all_hats <- data.frame(
  site = c(rep("SEV",length(sev_xhat)),
           rep("CPER",length(cper_xhat)),
           rep("HPG",length(hpg_xhat)),
           rep("HAR",length(hys_xhat)),
           rep("KNZ",length(knz_xhat))
           ),
  depth = c(sev_xhat, cper_xhat, hpg_xhat, hys_xhat, knz_xhat),
  cum_prop = c(sev_yhat, cper_yhat, hpg_yhat, hys_yhat, knz_yhat))

all_hats$site <- factor(all_hats$site, levels=c("SEV","CPER","HPG","HAR","KNZ"))

trends_depth <- ggplot(all_hats, aes(x=depth, y=cum_prop, col=site)) +
  geom_line() +
  geom_path() +
  theme_few() +
  scale_color_manual(values=col_vec[-1]) +
  xlab("Soil Depth (cm)") +
  ylab("Cumulative proportion of soil C")

png(paste0("Figures//C distribution by depth_trendlines",Sys.Date(),".png"), width=3.5, height=2.5, res=600, units="in")
print(trends_depth)
dev.off()

pdf(paste0("Figures//C distribution by depth_trendlines",Sys.Date(),".pdf"), width=3.5, height=2.5, useDingbats = F)
print(trends_depth)
dev.off()


### Read in soil C from EDGE sites
C_content.4north <- read.csv("SoilC//EDGE_north_sites_soilC.csv")
C_content.sev <- read.csv("SoilC//Ladwig_soil C sev.csv")  

C.u.4north <- ddply(C_content.4north, .(site), summarize,
                    C_gm2 = mean(C_gm2))
C.sd.4north <- ddply(C_content.4north, .(site), summarize,
                     C_gm2.sd = sd(C_gm2))
C.se.4north <- ddply(C_content.4north, .(site), summarize,
                     C_gm2.se = sd(C_gm2)/sqrt(length(C_gm2)))
C.4north <- C.u.4north %>%
  full_join(C.sd.4north, by="site") %>%
  full_join(C.se.4north, by="site")

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


### Calculate correction factor to convert soil C from 10 cm depth to 20 cm depth
### calculate parameters for each site
sev_Cp10 <- as.numeric(1-sev_beta^10)
sev_Cp20 <- as.numeric(1-sev_beta^20)

sbl_C10 <- filter(C_content, site=="SBL")$C_gm2
sbl_C10_sd <- filter(C_content, site=="SBL")$C_gm2.sd
sbl_C10_se <- filter(C_content, site=="SBL")$C_gm2.se

sbk_C10 <- filter(C_content, site=="SBK")$C_gm2
sbk_C10_sd <- filter(C_content, site=="SBK")$C_gm2.sd
sbk_C10_se <- filter(C_content, site=="SBK")$C_gm2.se

cper_Cp10 <- as.numeric(1-cper_beta^10)
cper_Cp20 <- as.numeric(1-cper_beta^20)
cper_C10 <- filter(C_content, site=="CPER")$C_gm2
cper_C10_sd <- filter(C_content, site=="CPER")$C_gm2.sd
cper_C10_se <- filter(C_content, site=="CPER")$C_gm2.se

hpg_Cp10 <- as.numeric(1-hpg_beta^10)
hpg_Cp20 <- as.numeric(1-hpg_beta^20)
hpg_C10 <- filter(C_content, site=="HPG")$C_gm2
hpg_C10_sd <- filter(C_content, site=="HPG")$C_gm2.sd
hpg_C10_se <- filter(C_content, site=="HPG")$C_gm2.se

hys_Cp10 <- as.numeric(1-hys_beta^10)
hys_Cp20 <- as.numeric(1-hys_beta^20)
hys_C10 <- filter(C_content, site=="HAR")$C_gm2
hys_C10_sd <- filter(C_content, site=="HAR")$C_gm2.sd
hys_C10_se <- filter(C_content, site=="HAR")$C_gm2.se

knz_Cp10 <- as.numeric(1-knz_beta^10)
knz_Cp20 <- as.numeric(1-knz_beta^20)
knz_C10 <- filter(C_content, site=="KNZ")$C_gm2
knz_C10_sd <- filter(C_content, site=="KNZ")$C_gm2.sd
knz_C10_se <- filter(C_content, site=="KNZ")$C_gm2.se

sbl_C20 <- sbl_C10/sev_Cp10 * sev_Cp20
sbk_C20 <- sbk_C10/sev_Cp10 * sev_Cp20
cper_C20 <- cper_C10/cper_Cp10 * cper_Cp20
hpg_C20 <- hpg_C10/hpg_Cp10 * hpg_Cp20
hys_C20 <- hys_C10/hys_Cp10 * hys_Cp20
knz_C20 <- knz_C10/knz_Cp10 * knz_Cp20
  
sbl_C20_se <- sbl_C10_se/sev_Cp10 * sev_Cp20
sbk_C20_se <- sbk_C10_se/sev_Cp10 * sev_Cp20
cper_C20_se <- cper_C10_se/cper_Cp10 * cper_Cp20
hpg_C20_se <- hpg_C10_se/hpg_Cp10 * hpg_Cp20
hys_C20_se <- hys_C10_se/hys_Cp10 * hys_Cp20
knz_C20_se <- knz_C10_se/knz_Cp10 * knz_Cp20

sbl_C20_sd <- sbl_C10_sd/sev_Cp10 * sev_Cp20
sbk_C20_sd <- sbk_C10_sd/sev_Cp10 * sev_Cp20
cper_C20_sd <- cper_C10_sd/cper_Cp10 * cper_Cp20
hpg_C20_sd <- hpg_C10_sd/hpg_Cp10 * hpg_Cp20
hys_C20_sd <- hys_C10_sd/hys_Cp10 * hys_Cp20
knz_C20_sd <- knz_C10_sd/knz_Cp10 * knz_Cp20

C_adjusted <- data.frame(
  site = c("SBL","SBK","CPER","HPG","HAR","KNZ"),
  C_to_20cm = c(sbl_C20,sbk_C20,cper_C20,hpg_C20,hys_C20,knz_C20),
  C_to_20cm_sd = c(sbl_C20_sd,sbk_C20_sd,cper_C20_sd,hpg_C20_sd,hys_C20_sd,knz_C20_sd),
  C_to_20cm_se = c(sbl_C20_se,sbk_C20_se,cper_C20_se,hpg_C20_se,hys_C20_se,knz_C20_se)
)

veg_C <- read.csv("soilC//vegC_all sites_measured.csv") %>%
  dplyr::group_by(site, C_type) %>%
  dplyr::summarise(C_mass=mean(C_mass)) %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(tot_veg_C = sum(C_mass))

C_adjusted_with_veg <- C_adjusted %>%
  full_join(veg_C, by="site") %>%
  mutate(C_to_20cm_plus_veg = C_to_20cm+tot_veg_C) %>%
  dplyr::select(-tot_veg_C)

C_10_and_20adj <- C_content %>%
  full_join(C_adjusted_with_veg, by="site")

C_for_plotting <- C_10_and_20adj %>%
  dplyr::select(site, C_gm2, C_to_20cm, C_to_20cm_plus_veg) %>%
  dplyr::rename(C_to_10cm=C_gm2) %>%
  gather(key=soil_layer, value=soil_carbon, -site)

C_for_plotting$site <- factor(C_for_plotting$site, levels=c("SBL","SBK","CPER","HPG","HAR","KNZ"))

c_plot <- ggplot(C_for_plotting, aes(x=site, y=soil_carbon, fill=soil_layer)) +
  geom_bar(stat="identity",position="dodge")+
  scale_fill_manual(values=c("grey","black","chartreuse4"))+
  theme_classic()

pdf(paste0("Figures//C at 10 and 20 cm_",Sys.Date(),".pdf"), width=5.5, height=4, useDingbats = F)
print(c_plot)
dev.off()

write.csv(C_adjusted_with_veg, file="soilC//veg plus soil C_extrapolated to 20 cm.csv", row.names=F)         
C_adjusted_with_veg <- read.csv("soilC//veg plus soil C_extrapolated to 20 cm.csv")     
?varpart
library(vegan)






