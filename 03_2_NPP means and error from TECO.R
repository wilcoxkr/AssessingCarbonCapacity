### Calculating NPP and standard error from TECO output; Dec2017 - calculate plant C ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com) ###
### Last updated: Feb. 18th, 2017 ###

library(tidyverse)
library(plyr)
library(ggplot2)

#setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_Feb2017\\")
setwd("C:\\Users\\Kevin.Wilcox\\Desktop\\fromOldComp\\FULL_ANALYSES_July2017\\")

teco.out <- read.csv("NPP\\C daily from TECO_all sites_all years_v2.csv")

site.yr.npp <- ddply(teco.out, .(site, Year), summarize,
      NPP.mod = sum(NPP.mod, na.rm=T))

site.means <- ddply(site.yr.npp, .(site), summarize,
                    NPP.u = mean(NPP.mod),
                    NPP.sd = sd(NPP.mod),
                    NPP.se = sd(NPP.mod)/sqrt(length(NPP.mod)))

ggplot(site.means, aes(x=site, y=NPP.u))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=NPP.u-NPP.se,ymax=NPP.u+NPP.se ))

write.csv(site.means, file="NPP\\NPP means and error_from TECO.csv",row.names=F)


### ANPP and BNPP regressions with MAP

anpp_bnpp_teco <- teco.out %>%
  mutate(N_A=N_L+N_S) %>%
  group_by(site, Year) %>%
  dplyr::summarise(ANPP=sum(N_A),
            BNPP=sum(N_R)) %>%
  group_by(site) %>%
  dplyr::summarise(ANPP=mean(ANPP),
                   BNPP=mean(BNPP))
  






