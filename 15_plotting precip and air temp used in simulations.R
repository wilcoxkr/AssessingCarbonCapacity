### Plotting precipitation and air temperature for model validation document
### 
### Author: Kevin Wilcox (kevin.wilcox@uwyo.edu)
### Created April 8, 2021; last updated April 8, 2021

### Set up workspace
rm(list=ls())
library(tidyverse)
library(ggthemes)

setwd("C:\\Users\\wilco\\OneDrive - University of Wyoming\\Cross_workstation_workspace\\Current projects\\EDGE DA_no chains\\TECO\\")
setwd("C:\\Users\\kwilcox4\\OneDrive - University of Wyoming\\Cross_workstation_workspace\\Current projects\\EDGE DA_no chains\\TECO\\")

sbl_clmt_raw <- read.csv("sbl\\sbl forcing_13-17_COMPLETE.csv")
sbk_clmt_raw <- read.csv("sbk\\sbk forcing_13-17_COMPLETE.csv")
cper_clmt_raw <- read.csv("cper\\cper forcing_13-17_GOLD.csv")
hpg_clmt_raw <- read.csv("hpg\\hpg forcing_13-17_GOLD.csv")
hys_clmt_raw <- read.csv("hys\\hys forcing_13-17_GOLD.csv")
knz_clmt_raw <- read.csv("knz\\knz forcing_13-17_GOLD.csv")

### Air temperature ###
#######################

tair_daily_means_all <- sbk_clmt_raw %>%
  group_by(year, doy) %>%
  summarize(tair_mean = mean(tair,na.rm=T),
            tair_max = max(tair, na.rm=T),
            tair_min = min(tair, na.rm=T)) %>%
  mutate(site="SBK") %>%
bind_rows(sbl_clmt_raw %>%
            group_by(year, doy) %>%
            summarize(tair_mean = mean(tair,na.rm=T),
                      tair_max = max(tair, na.rm=T),
                      tair_min = min(tair, na.rm=T)) %>%
  mutate(site="SBL")) %>%
bind_rows(cper_clmt_raw %>%
            group_by(year, doy) %>%
            summarize(tair_mean = mean(tair,na.rm=T),
                      tair_max = max(tair, na.rm=T),
                      tair_min = min(tair, na.rm=T)) %>%
            mutate(site="CPER")) %>%
  bind_rows(hpg_clmt_raw %>%
              group_by(year, doy) %>%
              summarize(tair_mean = mean(tair,na.rm=T),
                        tair_max = max(tair, na.rm=T),
                        tair_min = min(tair, na.rm=T)) %>%
              mutate(site="HPG")) %>%
  bind_rows(hys_clmt_raw %>%
              group_by(year, doy) %>%
              summarize(tair_mean = mean(tair,na.rm=T),
                        tair_max = max(tair, na.rm=T),
                        tair_min = min(tair, na.rm=T)) %>%
              mutate(site="HAR")) %>%
  bind_rows(knz_clmt_raw %>%
              group_by(year, doy) %>%
              summarize(tair_mean = mean(tair,na.rm=T),
                        tair_max = max(tair, na.rm=T),
                        tair_min = min(tair, na.rm=T)) %>%
              mutate(site="KNZ"))
  

tair_daily_means_all$site <- factor(tair_daily_means_all$site, levels=c("SBL","SBK","CPER","HPG","HAR","KNZ"))

  
tair_plot <- ggplot(tair_daily_means_all, aes(x=doy, y=tair_mean, col=factor(year))) +
  geom_path() +
  theme_few() +
  theme(legend.position="none") +
  facet_grid(site~.) +
  ylab("Air temperature (C)") +
  xlab("Day of year")

pdf(paste0("..//Model validation//figures//tair plot_all sites_", Sys.Date(), ".pdf"), width=2.5, height=10, useDingbats = F)
print(tair_plot)
dev.off()

### Cumulative precipitation

ppt_cumul_all <- sbk_clmt_raw %>%
  group_by(year) %>%
  mutate(ppt_cumul = cumsum(precp)) %>%
  mutate(site="SBK") %>%
  bind_rows(sbl_clmt_raw %>%
              group_by(year) %>%
              mutate(ppt_cumul = cumsum(precp)) %>%
              mutate(site="SBL")) %>%
  bind_rows(cper_clmt_raw %>%
              group_by(year) %>%
              mutate(ppt_cumul = cumsum(precp)) %>%
              mutate(site="CPER")) %>%
  bind_rows(hpg_clmt_raw %>%
              group_by(year) %>%
              mutate(ppt_cumul = cumsum(precp)) %>%
              mutate(site="HPG")) %>%
  bind_rows(hys_clmt_raw %>%
              group_by(year) %>%
              mutate(ppt_cumul = cumsum(precp)) %>%
              mutate(site="HAR")) %>%
  bind_rows(knz_clmt_raw %>%
              group_by(year) %>%
              mutate(ppt_cumul = cumsum(precp)) %>%
              mutate(site="KNZ"))

ppt_cumul_all$site <- factor(ppt_cumul_all$site, levels=c("SBL","SBK","CPER","HPG","HAR","KNZ"))
### Plot cumulative ppt for all sites
ppt_plot <- ggplot(ppt_cumul_all, aes(x=doy, y=ppt_cumul, col=factor(year))) +
  geom_path() +
  theme_few() +
#  theme(legend.position="none") +
  facet_grid(site~.) +
  ylab("Cumulative precipitation (mm)") +
  xlab("Day of year")

pdf(paste0("..//Model validation//figures//ppt plot_all sites_",Sys.Date(),".pdf"), width=4, height=10, useDingbats = F)
print(ppt_plot)
dev.off()




###### code to check climate forcing data #####

ggplot(subset(ppt_cumul_all, site=="KNZ" & year))

test <- read.csv("hpg//hpg forcing_13-17_COMPLETE.csv") %>%
  filter(year %in% c(2016,2017)) %>%
  filter(doy  != 366) %>%
  dplyr::select(-tair:-RH, -rad_h) %>%
  spread(key=year, value=precp) %>%
  rename(yr2016="2016", yr2017="2017")

  ggplot(test, aes(x=yr2016, y=yr2017) ) +
  geom_point()

  
  
  
  
  
  
  cper_h20 <- read.csv("c_daily_out_2021_04_06\\TECO_H2O_daily_cper.csv") %>%
    mutate(year=c(rep(2013,365),
                  rep(2014,365),
                  rep(2015,365),
                  rep(2016,366),
                  rep(2017,364))) %>%
    dplyr::select(year, d, P) %>%
    filter(year %in% c(2016, 2017)) %>%
    spread(key=year, value=P) %>%
    rename(yr2016="2016", yr2017="2017")
  
  
knz_h20 <- read.csv("c_daily_out_2021_04_06\\TECO_H2O_daily_knz.csv") %>%
  mutate(year=c(rep(2013,365),
                rep(2014,365),
                rep(2015,365),
                rep(2016,366),
                rep(2017,364))) %>%
  dplyr::select(year, d, P) %>%
  filter(year %in% c(2016, 2017)) %>%
  spread(key=year, value=P) %>%
  rename(yr2016="2016", yr2017="2017")

ggplot(cper_h20, aes(x=yr2016, y=yr2017)) +
  geom_point()
file.choose()

###########

sbl_raw <- read.csv("C:\\Users\\wilco\\Desktop\\sbl_fromEnqing.csv") %>%
  dplyr::select(year, doy, hour, Tair.degree.) %>%
  filter(year %in% c(2016,2017)) %>%
  spread(key=year, value=Tair.degree.) %>%
  rename(yr2016="2016", yr2017="2017")
ggplot(sbl_raw, aes(x=yr2016, y=yr2017)) +
  geom_point()

sbl_raw <- read.csv("C:\\Users\\wilco\\Desktop\\sbl_fromEnqing.csv") %>%
  dplyr::select(year, doy, hour, Tsoil.degree.) %>%
  filter(year %in% c(2016,2017)) %>%
  spread(key=year, value=Tsoil.degree.) %>%
  rename(yr2016="2016", yr2017="2017")
ggplot(sbl_raw, aes(x=yr2016, y=yr2017)) +
  geom_point()

sbl_raw <- read.csv("C:\\Users\\wilco\\Desktop\\sbl_fromEnqing.csv") %>%
  dplyr::select(year, doy, hour, Relhumidity...) %>%
  filter(year %in% c(2016,2017)) %>%
  spread(key=year, value=Relhumidity...) %>%
  rename(yr2016="2016", yr2017="2017")
ggplot(sbl_raw, aes(x=yr2016, y=yr2017)) +
  geom_point()

sbl_raw <- read.csv("C:\\Users\\wilco\\Desktop\\sbl_fromEnqing.csv") %>%
  dplyr::select(year, doy, hour, VPD) %>%
  filter(year %in% c(2016,2017)) %>%
  spread(key=year, value=VPD) %>%
  rename(yr2016="2016", yr2017="2017")
ggplot(sbl_raw, aes(x=yr2016, y=yr2017)) +
  geom_point()

sbl_raw <- read.csv("C:\\Users\\wilco\\Desktop\\sbl_fromEnqing.csv") %>%
  dplyr::select(year, doy, hour, Prec) %>%
  filter(year %in% c(2016,2017)) %>%
  spread(key=year, value=Prec) %>%
  rename(yr2016="2016", yr2017="2017")
ggplot(sbl_raw, aes(x=yr2016, y=yr2017)) +
  geom_point()

sbl_raw <- read.csv("C:\\Users\\wilco\\Desktop\\sbl_fromEnqing.csv") %>%
  dplyr::select(year, doy, hour, Solradiation.W.m2.) %>%
  filter(year %in% c(2016,2017)) %>%
  spread(key=year, value=Solradiation.W.m2.) %>%
  rename(yr2016="2016", yr2017="2017")
ggplot(sbl_raw, aes(x=yr2016, y=yr2017)) +
  geom_point()




ggplot(sbl_raw, aes(x=doy, y=yr2017)) +geom_point()
ggplot(sbl_raw, aes(x=hour, y=yr2017, col=doy)) +geom_path()


subset(hpg_raw, doy==1)$yr2016
