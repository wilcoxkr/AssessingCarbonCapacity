### Checking and formatting new climate forcing data for model validation
###
### Author: Kevin Wilcox (kevin.wilcox@uwyo.edu)
### Created Jan 26, 2022

### Set up workspace
rm(list=ls())
library(tidyverse)
library(ggthemes)
library(lubridate)

setwd("C:\\Users\\kwilcox4\\OneDrive - University of Wyoming\\Cross_workstation_workspace\\Current projects\\EDGE DA_no chains\\TECO\\")


###
### HPG
###
{
### Read in data
# Data from Dave and Nicole
hpg_clmt_raw_fromDave <- read.csv("..\\Climate forcing\\Model validation forcings\\HPGRS_hourly_wilcox__20210510.csv") %>%
  mutate(TIMESTAMP = mdy_hm(TIMESTAMP)) %>%
  mutate(year = format(as.Date(TIMESTAMP), "%Y"),
         doy = yday(TIMESTAMP)) %>%
  separate(TIMESTAMP, into = c("date", "time"), sep = " ") %>%
  separate(time, into = c("hour", "minute", "second"), sep=":") %>%
  dplyr::select(-minute, -second, -date) %>%
  mutate(year=as.numeric(year),
         hour=as.numeric(hour))

# Data from Enqing
hpg_clmt_raw <- read.csv("hpg\\hpg forcing_13-17_COMPLETE_2.csv")

### Isolate tair data without hour 23 from Enqing's data, and only tair data from hour 23 from Dave's data (see analysis notes for rationale)
### These will be combined to get a full-ish tair data set -- I'll have to spline to fill the hour 23 time slot when dave's data are missing
tair_fromEnqing <- hpg_clmt_raw %>%
  filter(!(year %in% 2016:2017 & hour==23))

tair_fromDave <- hpg_clmt_raw_fromDave %>%
  filter(year %in% 2016:2017 & hour==23)

tair_clean <- tair_fromEnqing %>%
  dplyr::select(year, doy, hour, tair) %>%
  bind_rows(
    tair_fromDave %>%
      dplyr::select(year, doy, hour, AirTC_Avg) %>%
      rename(tair = AirTC_Avg)
  )


#levels(as.factor(hpg_clmt_raw_fromDave$hour))

### Merge data from Enqing and Dave
hpg_clmt_merged <- hpg_clmt_raw %>%
  full_join(hpg_clmt_raw_fromDave, by=c("year", "doy", "hour"))

ggplot(hpg_clmt_merged, aes(x=tair, y=AirTC_Avg)) +
  geom_point() + theme_bw() + geom_abline(slope=1, col="red")

### Every entry in enqing's data at hour 23 in 2016 and 2017 is 8.895358
test <- filter(hpg_clmt_merged, hour==23)
ggplot(test, aes(x=tair, y=AirTC_Avg)) +
  geom_point() + theme_bw() + geom_abline(slope=1, col="red")
head(filter(test, year==2016))
ggplot(test, aes(x=factor(year), y=tair)) +
  geom_boxplot() + theme_bw()

# ### I will replace all tair values in original (from Enqing) data 2016-2017 with those from the CPER data -- the rest of the data I will leave as is
# tair_clean <- hpg_clmt_raw %>%
#   filter(year %in% 2013:2015) %>%
#   dplyr::select(year, doy, hour, tair) %>%
#   bind_rows(
#     hpg_clmt_raw_fromDave %>%
#     filter(year %in% 2016:2017) %>%
#     dplyr::select(year, doy, hour, AirTC_Avg) %>%
#     rename(tair = AirTC_Avg)
#   )
# 
# with(tair_clean, plot(tair))
# with(hpg_clmt_clean, plot(tair))
# ggplot(tair_clean, aes(x))

hpg_clmt_clean <- hpg_clmt_merged %>%
  dplyr::select(-tair) %>%
  full_join(tair_clean, by=c("year","doy","hour")) %>%
  dplyr::select(year, doy, hour, tair, Tsoil, VDEF, RH.x, precp, rad_h) %>%
  rename(RH = RH.x) %>%
  filter(year %in% 2013:2017)

filter(hpg_clmt_clean, is.na(tair))

write.csv(hpg_clmt_clean, file="..\\Climate forcing\\Model validation forcings\\hpg forcing_13-17_WORKING.csv", row.names=F)
hpg_clmt_clean <- read.csv("..\\Climate forcing\\Model validation forcings\\hpg forcing_13-17_WORKING.csv")



filter(hpg_clmt_clean, is.na(rad_h))
### testing stuff ###
daves_data <- hpg_clmt_raw_fromDave %>%
#  filter(year %in% 2016:2017) %>%
  dplyr::select(year, doy, hour, AirTC_Avg) #%>%
#  rename(tair = AirTC_Avg)

test2 <- hpg_clmt_clean %>%
  group_by(year, doy) %>%
  summarize(tair_mean = mean(tair,na.rm=T),
            tair_max = max(tair, na.rm=T),
            tair_min = min(tair, na.rm=T))

ggplot(filter(test2,doy %in% 140:175 & year==2017), aes(x=doy, y=tair_min)) + geom_point()
ggplot(test2, aes(x=doy, y=tair_mean)) + geom_point() + facet_wrap(~year)

########################

write.csv(hpg_clmt_clean, file="..\\Climate forcing\\Model validation forcings\\hpg forcing_13-17_GOLD.csv", row.names = F)

}

###
### CPER
###
{
cper_clmt_raw <- read.csv("cper\\cper forcing_13-17_fixing radiation_v2.csv") %>%
  mutate(rad_h = replace(rad_h, rad_h==169.6994767, NA),
         rad_h = replace(rad_h, rad_h==171.9219013, NA))


write.csv(cper_clmt_raw, "cper\\cper forcing_13-17_fixing radiation_v2.csv", row.names=F )

ggplot(filter(cper_clmt_raw, year %in% 2016:2017 & doy %in% 100:120), aes(doy, rad_h, col=factor(year))) +
  geom_point(alpha=0.5) + theme_bw()

test <-  filter(cper_clmt_raw, year %in% 2016 & doy %in% 110 & hour==19)

test2 <-  cper_clmt_raw %>%
  #mutate(rad_h = as.character(rad_h)) %>%
#  filter(year %in% 2016 & doy %in% 110 & hour==19)
  filter(is.na(rad_h))

ggplot(filter(cper_clmt_raw, doy %in% 1:50), aes(hour, rad_h, col=factor(doy))) +
  geom_path(alpha=0.5) +
  facet_wrap(~year) +
  theme_bw()

### Note, I just renamed the cper forcing_13-17_fixing radiation_v2.csv file cper forcing_13-17_GOLD.csv after making all the fixes

}