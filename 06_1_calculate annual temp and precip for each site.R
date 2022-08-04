### Calculating precipitation and temperature for time period of measurements ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com)
### last updated: Feb. 13th, 2017

#setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_Feb2017\\Climate forcing\\")
setwd("C:\\Users\\Kevin.Wilcox\\Desktop\\fromOldComp\\FULL_ANALYSES_July2017\\")

sevblu.raw <- read.csv("Climate forcing\\sev forcing_13-14_COMPLETE.csv")
sevblk.raw <- read.csv("Climate forcing\\sev forcing_13-14_COMPLETE.csv")
sgs.raw <- read.csv("Climate forcing\\sgs forcing_13-15_COMPLETE.csv")
chy.raw <- read.csv("Climate forcing\\chy forcing_13-15_COMPLETE.csv")
hys.raw <- read.csv("Climate forcing\\hys forcing_13-15_COMPLETE.csv")
knz.raw <- read.csv("Climate forcing\\knz forcing_13-15_COMPLETE.csv")

sevblu.raw$site <- "a.sevblu"
sevblk.raw$site <- "b.sevblk"
sgs.raw$site <- "c.sgs"
chy.raw$site <- "d.chy"
hys.raw$site <- "e.hys"
knz.raw$site <- "f.knz"

clmt.all <- rbind(sevblu.raw, rbind(sevblk.raw, rbind(sgs.raw, rbind(chy.raw, rbind(hys.raw, knz.raw)))))

annual.ppt.temp <- ddply(clmt.all, .(site, year), summarize, 
      annual.ppt = sum(precp, na.rm=T),
      annual.temp = mean(tair, na.rm=T))
gs.ppt.temp <- ddply(subset(clmt.all, doy>=91&doy<=273), .(site, year), summarize, 
                         gs.ppt = sum(precp, na.rm=T),
                         gs.temp = mean(tair, na.rm=T))

ppt.temp <- merge(annual.ppt.temp, gs.ppt.temp, by=c("site","year"))
mean.ppt.temp <- ddply(ppt.temp, .(site), summarize,
                       annual.ppt = mean(annual.ppt),
                       annual.temp = mean(annual.temp),
                       gs.ppt = mean(gs.ppt),
                       gs.temp = mean(gs.temp))                     

