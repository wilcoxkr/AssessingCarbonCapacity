###
### Compiling NPP data for model validation
###
### Author: Kevin Wilcox (kevin.wilcox@uwyo.edu)
### created June 19, 2020, last updated June 19, 2020

###
### Set up workspace
###

library(tidyverse)
library(ggthemes)

setwd("C:\\Users\\wilco\\OneDrive - University of Wyoming\\Cross_workstation_workspace\\Current projects\\EDGE DA_no chains\\NPP\\")

###
### Read in data
###

### Has all plot treatment identifiers
trt_key <- read.csv("NorthernEDGE_PlotTreatmentList.csv") %>%
  mutate(Plot=factor(Plot))

### ANPP and BNPP data from Enqing
npp_14_17 <- read.csv("anpp and bnpp mean and sd_all sites_ 2014-17_fromEnqing.csv")

### ANPP CPER 2013
anpp_cper_13 <- read.csv("EDGE_2013_SGS_ANPP_FINAL.csv")

anpp_cper_13_means_sd <- anpp_cper_13 %>%
  dplyr::select(Year:ANPP) %>%
  drop_na() %>%
  group_by(Year, Site, Treatment) %>%
  summarize(ANPP_mean = mean(ANPP),
            ANPP_sd = sd(ANPP)) %>%
  mutate(doy=243)


