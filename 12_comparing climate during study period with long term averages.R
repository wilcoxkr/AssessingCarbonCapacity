### Comparing climate in select years with long term averages
### Author: Kevin Wilcox (wilcoxkr@gmail.com)
###
### Last updated: March 8, 2018

rm(list=ls())
setwd("C:\\Users\\Kevin.Wilcox\\Desktop\\fromOldComp\\FULL_ANALYSES_July2017\\Climate forcing\\")

library(tidyverse)

sev_forcing_means <- read.csv("sev forcing_13-14_COMPLETE.csv") %>%
  group_by(year) %>%
  summarise(ann_ppt = sum(precp,na.rm=T),
            mean_temp = mean(tair,na.rm=T))

sgs_forcing_means <- read.csv("sgs forcing_13-15_COMPLETE.csv") %>%
  group_by(year) %>%
  summarise(ann_ppt = sum(precp,na.rm=T),
            mean_temp = mean(tair,na.rm=T))

chy_forcing_means <- read.csv("chy forcing_13-15_COMPLETE.csv") %>%
  group_by(year) %>%
  summarise(ann_ppt = sum(precp,na.rm=T),
            mean_temp = mean(tair,na.rm=T))

hys_forcing_means <- read.csv("hys forcing_13-15_COMPLETE.csv") %>%
  group_by(year) %>%
  summarise(ann_ppt = sum(precp,na.rm=T),
            mean_temp = mean(tair,na.rm=T))

knz_forcing_means <- read.csv("knz forcing_13-15_COMPLETE.csv") %>%
  group_by(year) %>%
  summarise(ann_ppt = sum(precp,na.rm=T),
            mean_temp = mean(tair,na.rm=T))

### growing season Apr1-Sept30 in northern sites and Apr1-Oct31 at SEV
sev_gs_means <- read.csv("sev forcing_13-14_COMPLETE.csv") %>%
  filter(doy %in% 91:304) %>%
  group_by(year) %>%
  summarise(gs_ppt = sum(precp,na.rm=T),
            gs_temp = mean(tair,na.rm=T)) %>%
  mutate(ppt_sd=NA) %>%
  ungroup(year) %>%
  mutate(year=as.character(year)) %>%
  bind_rows(data.frame(year="2012",gs_ppt=122.2,gs_temp=21.6,ppt_sd=NA)) %>%
  bind_rows(data.frame(year="long-term",gs_ppt=163,gs_temp=19.3,ppt_sd=.485*163)) %>%
  mutate(site="SBK_SBL")


sgs_gs_means <- read.csv("sgs forcing_13-15_COMPLETE.csv") %>%
  filter(doy %in% 91:273) %>%
  group_by(year) %>%
  summarise(gs_ppt = sum(precp,na.rm=T),
            gs_temp = mean(tair,na.rm=T)) %>%
  mutate(ppt_sd=NA) %>%
  ungroup(year) %>%
  mutate(year=as.character(year)) %>%
  bind_rows(data.frame(year="long-term",gs_ppt=293,gs_temp=16.4,ppt_sd=.335*293)) %>%
  mutate(site="SGS")


chy_gs_means <- read.csv("chy forcing_13-15_COMPLETE.csv") %>%
  filter(doy %in% 91:273) %>%
  group_by(year) %>%
  summarise(gs_ppt = sum(precp,na.rm=T),
            gs_temp = mean(tair,na.rm=T)) %>%
  mutate(ppt_sd=NA) %>%
  ungroup(year) %>%
  mutate(year=as.character(year)) %>%
  bind_rows(data.frame(year="long-term",gs_ppt=303,gs_temp=14.6,ppt_sd=.328*303)) %>%
  mutate(site="HPG")

hys_gs_means <- read.csv("hys forcing_13-15_COMPLETE.csv") %>%
  filter(doy %in% 91:273) %>%
  group_by(year) %>%
  summarise(gs_ppt = sum(precp,na.rm=T),
            gs_temp = mean(tair,na.rm=T)) %>%
  mutate(ppt_sd=NA) %>%
  ungroup(year) %>%
  mutate(year=as.character(year)) %>%
  bind_rows(data.frame(year="long-term",gs_ppt=426,gs_temp=20.8,ppt_sd=.347*426)) %>%
  mutate(site="HAR")

knz_gs_means <- read.csv("knz forcing_13-15_COMPLETE.csv") %>%
  filter(doy %in% 91:273) %>%
  group_by(year) %>%
  summarise(gs_ppt = sum(precp,na.rm=T),
            gs_temp = mean(tair,na.rm=T)) %>%
  mutate(ppt_sd=NA) %>%
  ungroup(year) %>%
  mutate(year=as.character(year)) %>%
  bind_rows(data.frame(year="long-term",gs_ppt=652,gs_temp=21.4,ppt_sd=.298*652)) %>%
  mutate(site="KNZ")

all_gs_means <- bind_rows(sev_gs_means, sgs_gs_means, chy_gs_means, hys_gs_means, knz_gs_means)
all_gs_means$year <- factor(all_gs_means$year, levels=c("long-term","2012", "2013","2014","2015"))
all_gs_means$site <- factor(all_gs_means$site, levels=c("SBK_SBL","SGS","HPG","HAR","KNZ"))

ppt_plot <- ggplot(all_gs_means, aes(year, y=gs_ppt, ymin=gs_ppt-ppt_sd, ymax=gs_ppt+ppt_sd)) +
  geom_bar(stat="identity", fill="dodgerblue3") +
  geom_errorbar(width=0) +
  theme_few()+
  facet_grid(~site,scales="free") +
  xlab("")+
  ylab("Growing season precipitation (mm)") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

temp_plot <- ggplot(all_gs_means, aes(year, y=gs_temp)) +
  geom_bar(stat="identity", fill="red3") +
  theme_few()+
  facet_grid(~site,scales="free") +
  xlab("")+
  ylab("Growing season temperature (C)") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

pdf(paste0("..\\Figures\\long term versus gs ppt_",Sys.Date(),".pdf"), height=2.5, width=6,useDingbats = F)
print(ppt_plot)
dev.off()

pdf(paste0("..\\Figures\\long term versus gs temp_",Sys.Date(),".pdf"), height=2.5, width=6,useDingbats = F)
print(temp_plot)
dev.off()


colMeans(sev_gs_means)
colMeans(sgs_gs_means)
colMeans(chy_gs_means)
colMeans(hys_gs_means)
colMeans(knz_gs_means)

.485*163




