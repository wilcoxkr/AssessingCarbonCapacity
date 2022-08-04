### Ecosystem residence time Sensitivity analysis ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com) ###
### Last updated: July 24, 2017 ###


# Data preparation --------------------------------------------------------
setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_July2017\\")

# Read in elements necessary for tau calculation (and tau function) #
source("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_Feb2017\\Scripts\\03_1_Calculating tau.R")

require(scales)
require(ggplot2)
require(ggthemes)

# Read in MLE parameters #
pMLE <- read.csv("DA Diagnostics and MLEs\\MLE values all parameters all plots_4 north sites_July2017.csv")


# Function for calculating residence time using daily tau -----------------

Res_time <- function(tau.,c_new,f_new,b){
  AC <- matrix(0,nrow=length(c_new), ncol=length(c_new))
  AC[1,] <- c(    -c_new[1],    0,             0,               0,                   0,                0)
  AC[2,] <- c(	0,           -c_new[2],      0,               0,                   0,                0) ## 
  AC[3,] <- c(	c_new[1],  	 c_new[2],    -c_new[3],      	  0,                   0,                0) # I'm not sure that all of c2 should go into litter... if plant respiration is occurring, this should detract from the amount of C in this pool... and since we put GPP into the model, this should be accounted for... ask Yiqi Zheng about this.
  AC[4,] <- c(	0,  		  0,   	   f_new[1]*c_new[3],   -c_new[4],       f_new[5]*c_new[5], f_new[7]*c_new[6])
  AC[5,] <- c(	0,            0,       f_new[2]*c_new[3], f_new[3]*c_new[4],   -c_new[5],            0)
  AC[6,] <- c(    0,      	  0,             0,           f_new[4]*c_new[4], f_new[6]*c_new[5],     -c_new[6]) #the constant matrix A
  t_pools.daily <-  data.frame(Day=0,lf_Rt=0,rt_Rt=0, 
                               lt_Rt=0,mC_Rt=0, 
                               sC_Rt=0,pC_Rt=0,
                               Eco_Rt=0,stringsAsFactors=FALSE)
    
  for(i in 1:length(tau.)){
    t_pools.temp <- (solve(AC*tau.[i])%*%b)/-365
    tE.temp <-  colSums(t_pools.temp)
    t_pools.daily=rbind(t_pools.daily, c(i,t(t_pools.temp),tE.temp))
  }
  #return(t_pools.daily[-1,])
  return(colMeans(t_pools.daily[-1,2:8]))
 }


# Set max and min c turnover rates for plotting limits --------------------
cmin <- numeric(6)
cmin[1] <- 1.0e-4 
cmin[2] <- 1.0e-4 
cmin[3] <- 5.0e-4 
cmin[4] <- 5.0e-3 
cmin[5] <- 1.0e-5 
cmin[6] <- 1.0e-8

# Set maximum limits of c flux ranges #
cmax <- numeric(6)
cmax[1] <- 1.0e-2
cmax[2] <- 1.0e-2 
cmax[3] <- 2.0e-2 
cmax[4] <- 5.0e-2 
cmax[5] <- 2.0e-3 
cmax[6] <- 3.0e-5

fmin <- numeric(7)
fmin[1] <- 3.0e-1 
fmin[2] <- 5.0e-2 
fmin[3] <- 2.5e-1 
fmin[4] <- 1.0e-3 
fmin[5] <- 1.0e-1
fmin[6] <- 0.2e-2 
fmin[7] <- 3.0e-1
# Max C transfer #
fmax <- numeric(7)
fmax[1] <- 7.0e-1 
fmax[2] <- 1.5e-1 
fmax[3] <- 6.5e-1  
fmax[4] <- 8.0e-3 
fmax[5] <- 6.0e-1 
fmax[6] <- 7.0e-2 
fmax[7] <- 7.0e-1
## soil moisture cutoff max and min ##
mscut_min <- .01 
mscut_max <- .40
## soil temperature tau modifier Q10 -- max and min ##
Q10_min <- 1.0
Q10_max <- 4.0


# Function for sensitivity analyses ---------------------------------------

sens.fxn <- function(par.set, Nt, moist, temp, b, iter=5, focus.par, parmin, parmax){
  Rt.eco <- data.frame(parameter=NULL, par.value=NULL,Eco_Rt=NULL)

  for(j in 1:length(focus.par)){  
    param.vec <- seq(parmin[j],parmax[j],length=iter)
    param.stack <- data.frame(param.vec, par.set[-grep(focus.par[j], colnames(par.set))],row.names=NULL)
    colnames(param.stack) <- c(focus.par[j],colnames(par.set[-grep(focus.par[j], colnames(par.set))]))
  
    for(i in 1:iter){
      param.temp <- param.stack[i,]
      tau.temp <- tau.fxn(Nt=Nt, moist=moist, temp=temp, mscut=param.temp$mscut, Q10=param.temp$Q10)
      c.temp <- c(param.temp$c1,param.temp$c2,param.temp$c3,param.temp$c4,param.temp$c5,param.temp$c6)
      f.temp <- c(param.temp$f1,param.temp$f2,param.temp$f3,param.temp$f4,param.temp$f5,param.temp$f6,param.temp$f7)
      Rt.all <- Res_time(tau.=tau.temp[-1], c_new=c.temp, f_new=f.temp, b=b)
      Rt.eco.temp <- data.frame(parameter=focus.par[j], par.value=param.vec[i],Eco_Rt=as.data.frame(t(Rt.all))$Eco_Rt)
      Rt.eco <- rbind(Rt.eco, Rt.eco.temp)
    }
  }  
  return(Rt.eco)
}


# Calculate sensitivity for each site -------------------------------------

sevblu.c1 <- sens.fxn(par.set=subset(pMLE, Site=="sevblu")[-1],
         Nt=sevblu.Nt,
         moist=sevblu.moist,
         temp=sevblu.temp,
         b=sevblu.b,
         iter=20,
         focus.par=c("c1","c2","c3","c4","c5","c6",
                     "f1","f2","f3","f4","f5","f6","f7",
                     "mscut","Q10"),
         parmin=c(cmin,fmin,mscut_min, Q10_min),
         parmax=c(cmax,fmax,mscut_max, Q10_max)
)

sevblk.c1 <- sens.fxn(par.set=subset(pMLE, Site=="sevblk")[-1],
                      Nt=sevblk.Nt,
                      moist=sevblk.moist,
                      temp=sevblk.temp,
                      b=sevblk.b,
                      iter=20,
                      focus.par=c("c1","c2","c3","c4","c5","c6",
                                  "f1","f2","f3","f4","f5","f6","f7",
                                  "mscut","Q10"),
                      parmin=c(cmin,fmin,mscut_min, Q10_min),
                      parmax=c(cmax,fmax,mscut_max, Q10_max)
)

sgs.c1 <- sens.fxn(par.set=subset(pMLE, Site=="sgs")[-1],
                      Nt=sgs.Nt,
                      moist=sgs.moist,
                      temp=sgs.temp,
                      b=sgs.b,
                      iter=20,
                      focus.par=c("c1","c2","c3","c4","c5","c6",
                                  "f1","f2","f3","f4","f5","f6","f7",
                                  "mscut","Q10"),
                      parmin=c(cmin,fmin,mscut_min, Q10_min),
                      parmax=c(cmax,fmax,mscut_max, Q10_max)
)

chy.c1 <- sens.fxn(par.set=subset(pMLE, Site=="chy")[-1],
                      Nt=chy.Nt,
                      moist=chy.moist,
                      temp=chy.temp,
                      b=chy.b,
                      iter=20,
                      focus.par=c("c1","c2","c3","c4","c5","c6",
                                  "f1","f2","f3","f4","f5","f6","f7",
                                  "mscut","Q10"),
                      parmin=c(cmin,fmin,mscut_min, Q10_min),
                      parmax=c(cmax,fmax,mscut_max, Q10_max)
)

hys.c1 <- sens.fxn(par.set=subset(pMLE, Site=="hys")[-1],
                      Nt=hys.Nt,
                      moist=hys.moist,
                      temp=hys.temp,
                      b=hys.b,
                      iter=20,
                      focus.par=c("c1","c2","c3","c4","c5","c6",
                                  "f1","f2","f3","f4","f5","f6","f7",
                                  "mscut","Q10"),
                      parmin=c(cmin,fmin,mscut_min, Q10_min),
                      parmax=c(cmax,fmax,0.38, Q10_max)
)

knz.c1 <- sens.fxn(par.set=subset(pMLE, Site=="knz")[-1],
                      Nt=knz.Nt,
                      moist=knz.moist,
                      temp=knz.temp,
                      b=knz.b,
                      iter=20,
                      focus.par=c("c1","c2","c3","c4","c5","c6",
                                  "f1","f2","f3","f4","f5","f6","f7",
                                  "mscut","Q10"),
                      parmin=c(cmin,fmin,mscut_min, Q10_min),
                      parmax=c(cmax,fmax,mscut_max, Q10_max)
)

sevblu.sensitivity <- data.frame(Site="sevblu",sevblu.c1)
sevblk.sensitivity <- data.frame(Site="sevblk",sevblk.c1)
sgs.sensitivity <- data.frame(Site="sgs",sgs.c1)
chy.sensitivity <- data.frame(Site="chy",chy.c1)
hys.sensitivity <- data.frame(Site="hys",hys.c1)
knz.sensitivity <- data.frame(Site="knz",knz.c1)

# Combine all sensitivity runs for all sites #
sensitivity.all <- rbind(sgs.sensitivity,
                                     rbind(chy.sensitivity,
                                           rbind(hys.sensitivity,
                                                 knz.sensitivity)))

#write.csv(sensitivity.all, 
          file="Sensitivity analysis\\Sensitivity output_all param_4 north sites_July2017.csv",
          row.names=F)

sensitivity.all <- read.csv("Sensitivity analysis\\Sensitivity output_all param_4 north sites_July2017.csv")

# Plotting sensitivity analysis output ------------------------------------

head(sgs.sensitivity )
sevblu.plot <- ggplot(subset(sensitivity.all, Site=="sevblu"), aes(x=par.value, y=Eco_Rt))+
  geom_point()+
  xlab("Parameter value")+
  ylab("Ecosystem residence time (years)")+
  ylim(0,100)+
  facet_grid(. ~ parameter, scales="free")+
  theme_few()+
  theme(axis.text.x = element_text(angle=90, hjust=1, size=8))+
  scale_x_continuous(breaks=pretty_breaks(n=3))

sevblk.plot <- ggplot(subset(sensitivity.all, Site=="sevblk"), aes(x=par.value, y=Eco_Rt))+
  geom_point()+
  xlab("Parameter value")+
  ylab("Ecosystem residence time (years)")+
  ylim(0,100)+
  facet_grid(. ~ parameter, scales="free")+
  theme_few()+
  theme(axis.text.x = element_text(angle=90, hjust=1, size=8))+
  scale_x_continuous(breaks=pretty_breaks(n=3))

sgs.plot <- ggplot(subset(sensitivity.all, Site=="sgs"), aes(x=par.value, y=Eco_Rt))+
  geom_point()+
  xlab("Parameter value")+
  ylab("Ecosystem residence time (years)")+
  ylim(0,100)+
  facet_grid(. ~ parameter, scales="free")+
  theme_few()+
  theme(axis.text.x = element_text(angle=90, hjust=1, size=8))+
  scale_x_continuous(breaks=pretty_breaks(n=3))

chy.plot <- ggplot(subset(sensitivity.all, Site=="chy"), aes(x=par.value, y=Eco_Rt))+
  geom_point()+
  xlab("Parameter value")+
  ylab("Ecosystem residence time (years)")+
  ylim(0,100)+
  facet_grid(. ~ parameter, scales="free")+
  theme_few()+
  theme(axis.text.x = element_text(angle=90, hjust=1, size=8))+
  scale_x_continuous(breaks=pretty_breaks(n=3))

hys.plot <- ggplot(subset(sensitivity.all, Site=="hys"), aes(x=par.value, y=Eco_Rt))+
  geom_point()+
  xlab("Parameter value")+
  ylab("Ecosystem residence time (years)")+
  ylim(0,100)+
  facet_grid(. ~ parameter, scales="free")+
  theme_few()+
  theme(axis.text.x = element_text(angle=90, hjust=1, size=8))+
  scale_x_continuous(breaks=pretty_breaks(n=3))

knz.plot <- ggplot(subset(sensitivity.all, Site=="knz"), aes(x=par.value, y=Eco_Rt))+
  geom_point()+
  xlab("Parameter value")+
  ylab("Ecosystem residence time (years)")+
  ylim(0,100)+
  facet_grid(. ~ parameter, scales="free")+
  theme_few()+
  theme(axis.text.x = element_text(angle=90, hjust=1, size=8))+
  scale_x_continuous(breaks=pretty_breaks(n=3))

# Save plots, commented out so I can source this script #
#ggsave("Figures//Sensitivity plots//sevblu_sensitivity_all params.pdf",
#       sevblu.plot, width=12, height=1.75, unit="in")
#ggsave("Figures//Sensitivity plots//sevblk_sensitivity_all params.pdf",
#       sevblk.plot, width=12, height=1.75, unit="in")
#ggsave("Figures//Sensitivity plots//sgs_sensitivity_all params.pdf",
       #       sgs.plot, width=12, height=1.75, unit="in")
pdf("Figures//Sensitivity plots//sgs_sensitivity_all params.pdf", width=12, height=1.75,  useDingbats = F)
print(sgs.plot)
dev.off()

#ggsave("Figures//Sensitivity plots//chy_sensitivity_all params.pdf",
       #       chy.plot, width=12, height=1.75, unit="in")
#ggsave("Figures//Sensitivity plots//hys_sensitivity_all params.pdf",
       #       hys.plot, width=12, height=1.75, unit="in")
#ggsave("Figures//Sensitivity plots//knz_sensitivity_all params.pdf",
#       knz.plot, width=12, height=1.75, unit="in")

## Calculate environmental scalar vectors ## 
#sevblu.tau <- tau.fxn(Nt=sevblu.Nt, moist=sevblu.moist, temp=sevblu.temp, mscut=sevblu.mscut, Q10=sevblu.Q10)
#sevblk.tau <- tau.fxn(Nt=sevblk.Nt, moist=sevblk.moist, temp=sevblk.temp, mscut=sevblk.mscut, Q10=sevblk.Q10)
#sgs.tau <- tau.fxn(Nt=sgs.Nt, moist=sgs.moist, temp=sgs.temp, mscut=sgs.mscut, Q10=sgs.Q10)
#chy.tau <- tau.fxn(Nt=chy.Nt, moist=chy.moist, temp=chy.temp, mscut=chy.mscut, Q10=chy.Q10)
#hys.tau <- tau.fxn(Nt=hys.Nt, moist=hys.moist, temp=hys.temp, mscut=hys.mscut, Q10=hys.Q10)
#knz.tau <- tau.fxn(Nt=knz.Nt, moist=knz.moist, temp=knz.temp, mscut=knz.mscut, Q10=knz.Q10)


