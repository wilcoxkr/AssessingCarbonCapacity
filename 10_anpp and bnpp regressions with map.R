### ANPP and BNPP regressions with MAP; Dec2017 - calculate plant C ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com) ###
### Last updated: Dec. 22, 2017 ###

library(tidyverse)
library(ggplot2)
setwd("C:\\Users\\Kevin.Wilcox\\Desktop\\fromOldComp\\FULL_ANALYSES_July2017\\")

teco.out <- read.csv("NPP\\C daily from TECO_all sites_all years_v2.csv")
site.char.all <- read.csv("Site characteristics//Site characteristics_soil veg and climate_all sites.csv")

### ANPP and BNPP regressions with MAP

anpp_bnpp_teco <- teco.out %>%
  mutate(N_A=N_L+N_S) %>%
  group_by(site, Year) %>%
  dplyr::summarise(ANPP=sum(N_A),
                   BNPP=sum(N_R)) %>%
  group_by(site) %>%
  dplyr::summarise(ANPP=mean(ANPP),
                   BNPP=mean(BNPP))

anpp_bnpp_map_long <- site.char.all %>% 
  dplyr::select(site, annual.ppt) %>%
  full_join(anpp_bnpp_teco, by="site") %>%
  gather(key=prod_type, value=prod_value, -(site:annual.ppt))

ggplot(anpp_bnpp_map_long, aes(annual.ppt, prod_value, col=prod_type)) +
  geom_point(size=3) +
  geom_smooth(method="lm",se=F) +
  theme_classic() +
  ylab("ANPP or BNPP (g C m-2)") +
  xlab("MAP (mm)")

anova(lm(prod_value ~ annual.ppt*prod_type, data=anpp_bnpp_map_long))
summary(lm(prod_value ~ annual.ppt*prod_type, data=anpp_bnpp_map_long))

summary(lm(prod_value ~ annual.ppt, data=subset(anpp_bnpp_map_long, prod_type=="ANPP")))
summary(lm(prod_value ~ annual.ppt, data=subset(anpp_bnpp_map_long, prod_type=="BNPP")))


