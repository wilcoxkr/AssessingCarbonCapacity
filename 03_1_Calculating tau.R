### Calculating tau through time for each site using tau parameters ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com) ###
### Last updated: Nov 17 2017 ###

library(tidyverse)
library(ggplot2)
library(ggthemes)

rm(list=ls())
setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_July2017\\")
setwd("C:\\Users\\Kevin.Wilcox\\Desktop\\fromOldComp\\FULL_ANALYSES_July2017\\")
# Data import for calculating daily tau's -------------------------------------------------------
## Read in soil moisture and temp data ##
sevblu.moist.raw <- read.csv("DA input\\sevblu\\moist5cm_blue_sev13_14_fromSite_tao.csv")
sevblu.temp.raw <- read.csv("DA input\\sevblu\\temp5cm_blue_sev13_14_fromSite_tao.csv") 
sevblk.moist.raw <- read.csv("DA input\\sevblk\\moist5cm_black_sev13_14_fromSite_tao.csv")
sevblk.temp.raw <- read.csv("DA input\\sevblk\\temp5cm_black_sev13_14_fromSite_tao.csv") 
sgs.moist.raw <- read.csv("DA input\\sgs\\sgs_smoist_tau_13-15.csv")
sgs.temp.raw <- read.csv("DA input\\sgs\\sgs_stemp_tau_13-15.csv") 
chy.moist.raw <- read.csv("DA input\\chy\\chy_smoist_tau_13-15.csv")
chy.temp.raw <- read.csv("DA input\\chy\\chy_stemp_tau_13-15.csv") 
hys.moist.raw <- read.csv("DA input\\hys\\hys_smoist_tau_13-15.csv")
hys.temp.raw <- read.csv("DA input\\hys\\hys_stemp_tau_13-15.csv") 
knz.moist.raw <- read.csv("DA input\\knz\\knz_smoist_tau_13-15.csv")
knz.temp.raw <- read.csv("DA input\\knz\\knz_stemp_tau_13-15.csv") 

sevblk.moist <- sevblk.moist.raw$Smoist
sevblk.temp <- sevblk.temp.raw$Stemp
sevblu.moist <- sevblu.moist.raw$Smoist
sevblu.temp <- sevblu.temp.raw$Stemp
sgs.moist <- sgs.moist.raw$Smoist
sgs.temp <- sgs.temp.raw$temp
chy.moist <- chy.moist.raw$Smoist
chy.temp <- chy.temp.raw$temp
hys.moist <- hys.moist.raw$Smoist
hys.temp <- hys.temp.raw$temp
knz.moist <- knz.moist.raw$Smoist
knz.temp <- knz.temp.raw$temp

## Sample size ##
sevblu.Nt <- length(sevblu.temp)-1
sevblk.Nt <- length(sevblk.temp)-1
sgs.Nt <- length(sgs.temp)-1
chy.Nt <- length(chy.temp)-1
hys.Nt <- length(hys.temp)-1
knz.Nt <- length(knz.temp)-1

## GPP partitioning parameters ##
sevblu.b <- c(0.18/.55,0.37/.55,0,0,0,0)
sevblk.b <- c(0.18/.55,0.37/.55,0,0,0,0)
sgs.b <- c(0.25/.55,0.30/.55,0,0,0,0)
chy.b <- c(0.20/.55,0.35/.55,0,0,0,0)
hys.b <- c(0.39/.55,0.16/.55,0,0,0,0)
knz.b <- c(0.34/.55,0.21/.55,0,0,0,0)

## mscut and Q10 parameters ##
mle.raw <- read.csv("DA Diagnostics and MLEs//MLE values all parameters all plots_4 north sites_July2017.csv")
# mscut
sevblu.mscut <- subset(mle.raw, Site=="sevblu")$mscut
sevblk.mscut <- subset(mle.raw, Site=="sevblk")$mscut
sgs.mscut <- subset(mle.raw, Site=="sgs")$mscut
chy.mscut <- subset(mle.raw, Site=="chy")$mscut
hys.mscut <- subset(mle.raw, Site=="hys")$mscut
knz.mscut <- subset(mle.raw, Site=="knz")$mscut
# Q10
sevblu.Q10 <- subset(mle.raw, Site=="sevblu")$Q10
sevblk.Q10 <- subset(mle.raw, Site=="sevblk")$Q10
sgs.Q10 <- subset(mle.raw, Site=="sgs")$Q10
chy.Q10 <- subset(mle.raw, Site=="chy")$Q10
hys.Q10 <- subset(mle.raw, Site=="hys")$Q10
knz.Q10 <- subset(mle.raw, Site=="knz")$Q10


# Tau function ------------------------------------------------------------

tau.fxn <- function(Nt, temp, moist, mscut, Q10){ # Function to create tau for each day in simulation
  product <- numeric(Nt+1)
  moisture_vec <- numeric(Nt+1)
  tmp_vec <- numeric(Nt+1)
  for(i in 1:Nt+1){ # Creates "product" which is the combined effect of temperature and moisture on C turnover
    # Temperature effect on tau #
    sumtemp <- 0
    tmp <- temp[i]
    if(i > 10){ # Loop that calculates moving 10 day average # 
      for(j in (i-9):i){
        sumtemp <- sumtemp+temp[j]
      }
      tmp = sumtemp/10
    }
    # old variables in model were R10=0.65 and Q10=2.2; I've changed these to reflect those estimated by Zheng in his Ecosphere paper
    tmp <- 0.58*Q10^((tmp-10)/10) ## Could designate some estimable parameters for R10 and Q10 here... the .65 number is R10 and the 2.2 is Q10 I think #
    # Moisture effect on tau #
    moisture = 1
    if(moist[i] < mscut){
      moisture <- 1.0 - 5.0*(mscut-moist[i])
    }
    product[i] <- tmp*moisture
    moisture_vec[i] <- moisture
    tmp_vec[i] <- tmp
  }
  tau_all <- data.frame(tau_tmp=tmp_vec, tau_moist=moisture_vec, tau_product=product)
  return(tau_all)
}	


# Calculating residence time (no bootstrapping) ---------------------------

## Calculate environmental scalar vectors (using mscut and Q10 means) ##
sevblu.tau <- data.frame(Site="sevblu", 
                         doy=1:365,
                         day_cont=1:sevblu.Nt,
                         tau.fxn(Nt=sevblu.Nt, moist=sevblu.moist, temp=sevblu.temp, mscut=sevblu.mscut, Q10=sevblu.Q10)[-1,])
sevblk.tau <- data.frame(Site="sevblk", 
                         doy=1:365,
                         day_cont=1:sevblk.Nt,
                         tau.fxn(Nt=sevblk.Nt, moist=sevblk.moist, temp=sevblk.temp, mscut=sevblk.mscut, Q10=sevblk.Q10)[-1,])
sgs.tau <- data.frame(Site="sgs", 
                         doy=1:365,
                         day_cont=1:sgs.Nt,
                         tau.fxn(Nt=sgs.Nt, moist=sgs.moist, temp=sgs.temp, mscut=sgs.mscut, Q10=sgs.Q10)[-1,])
chy.tau <- data.frame(Site="chy", 
                         doy=1:365,
                         day_cont=1:chy.Nt,
                         tau.fxn(Nt=chy.Nt, moist=chy.moist, temp=chy.temp, mscut=chy.mscut, Q10=chy.Q10)[-1,])
hys.tau <- data.frame(Site="hys", 
                         doy=1:365,
                         day_cont=1:hys.Nt,
                         tau.fxn(Nt=hys.Nt, moist=hys.moist, temp=hys.temp, mscut=hys.mscut, Q10=hys.Q10)[-1,])
knz.tau <- data.frame(Site="knz", 
                         doy=1:365,
                         day_cont=1:knz.Nt,
                         tau.fxn(Nt=knz.Nt, moist=knz.moist, temp=knz.temp, mscut=knz.mscut, Q10=knz.Q10)[-1,])

# Combine all sites #
tau_all <- rbind(sevblk.tau, sevblu.tau, sgs.tau, chy.tau, hys.tau, knz.tau)

write.csv(tau_all, file="Tau_continuous//tau estimates through time_4 north sites_Nov2017.csv", row.names=F)
tau.all <- read.csv("Tau_continuous//tau estimates through time_all sites_Nov2017.csv")

# put in long format for plotting
tau_all_long <- tau_all %>%
  gather(key=tau_type, value=tau_value, -(Site:day_cont))

tau_all$Site <- factor(tau_all$Site, levels=c("sevblu","sevblk","sgs","chy","hys","knz"))

tau_plot_all <- ggplot(tau_all_long, aes(x=day_cont, y=tau_value, col=tau_type, lty=tau_type)) +
  geom_hline(yintercept=1, linetype=1)+
  geom_line()+
  xlab("Day")+
  theme_few()+
  theme(text=element_text(size=16))+
  facet_grid(Site ~ ., scales="fixed") +
  theme(legend.position="none")

ggsave("Figures//tau plot through time_withKNZ_with temp and moist tau.pdf", tau_plot_all, height=9, width=4, units="in")






temp_hats <- seq(-10,30,1) 
temp_tau_hats <- 0.58*3.5^((temp_hats-10)/10)
plot(temp_hats, temp_tau_hats, ylim=c(0,5))
lines(temp_hats, temp_tau_hats,col="blue")
chy.Q10





