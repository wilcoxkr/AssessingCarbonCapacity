### Calculating effects of each parameter estimate on ecosystem residence time ###
### Author: Kevin Wilcox (wilcoxkr@gmail.com) ###
### Last updated: July 24, 2017 ###


# Workspace prep ----------------------------------------------------------

library(plyr)
library(stats)
require(ggplot2)
require(ggthemes)

setwd("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_July2017\\")

# Data prep ---------------------------------------------------------------
sensitivity.all <- read.csv("Sensitivity analysis\\Sensitivity output_all param_4 north sites_July2017.csv")

# Read in elements necessary for tau calculation (and tau function) #
source("C:\\Users\\WilcoxKR.WILCOXKR-LAPTOP\\Desktop\\EDGE modelling\\FULL_ANALYSES_Feb2017\\Scripts\\03_1_Calculating tau.R")


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


# Calculate means for c and f parameters ----------------------------------
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

cmean <- rowMeans(data.frame(max=cmax, min=cmin))
fmean <- rowMeans(data.frame(max=fmax, min=fmin))
mscut_mean <- 0.2
Q10_mean <- 2.2

baseParam.vals <- c(cmean,fmean,mscut_mean,Q10_mean)
baseParams <- data.frame(t(baseParam.vals))
colnames(baseParams) <- c(paste0("c",1:6), paste0("f",1:7),"mscut","Q10")

# Function for eco_rt response to particular shift in parameter ---------------------------------------

sensPredict <- function(par.set, Nt, moist, temp, b, baseParams, focus.par){
  Rt.eco <- data.frame(parameter=NULL, run.type=NULL, par.value=NULL,Eco_Rt=NULL)
  for(j in 1:length(focus.par)){  
    data.frame(par.type="site", par.set)
    par.set.replace <- par.set
    par.set.replace[grep(focus.par[j],colnames(par.set.replace))] <-
      baseParams[grep(focus.par[j],colnames(baseParams))]
    param.stack <- rbind(par.set, par.set.replace)
    param.stack$run.type <- c("siteMLE","Baseline")

      for(i in 1:nrow(param.stack)){
      param.temp <- param.stack[i,]
      tau.temp <- tau.fxn(Nt=Nt, moist=moist, temp=temp, mscut=param.temp$mscut, Q10=param.temp$Q10)
      c.temp <- c(param.temp$c1,param.temp$c2,param.temp$c3,param.temp$c4,param.temp$c5,param.temp$c6)
      f.temp <- c(param.temp$f1,param.temp$f2,param.temp$f3,param.temp$f4,param.temp$f5,param.temp$f6,param.temp$f7)
      Rt.all <- Res_time(tau.=tau.temp[-1], c_new=c.temp, f_new=f.temp, b=b)
      Rt.eco.temp <- data.frame(parameter=focus.par[j], 
                                run.type=param.temp$run.type,
                                par.value=param.temp[grep(focus.par[j],colnames(param.temp))],
                                Eco_Rt=as.data.frame(t(Rt.all))$Eco_Rt)
      colnames(Rt.eco.temp) <- c("parameter","run.type","par.value","Eco_Rt")
      Rt.eco <- rbind(Rt.eco, Rt.eco.temp)
    }
  }  
  return(Rt.eco)
}

# Generate observed effect of parameter changes #
sevblu.predict <- sensPredict(par.set=subset(pMLE, Site=="sevblu")[-1],
  Nt=sevblu.Nt,
  moist=sevblu.moist,
  temp=sevblu.temp,
  b=sevblu.b,
  focus.par=c("c1","c2","c3","c4","c5","c6",
            "f1","f2","f3","f4","f5","f6","f7",
            "mscut","Q10"),
  baseParams <- baseParams)

sevblk.predict <- sensPredict(par.set=subset(pMLE, Site=="sevblk")[-1],
                              Nt=sevblk.Nt,
                              moist=sevblk.moist,
                              temp=sevblk.temp,
                              b=sevblk.b,
                              focus.par=c("c1","c2","c3","c4","c5","c6",
                                          "f1","f2","f3","f4","f5","f6","f7",
                                          "mscut","Q10"),
                              baseParams <- baseParams)

sgs.predict <- sensPredict(par.set=subset(pMLE, Site=="sgs")[-1],
                              Nt=sgs.Nt,
                              moist=sgs.moist,
                              temp=sgs.temp,
                              b=sgs.b,
                              focus.par=c("c1","c2","c3","c4","c5","c6",
                                          "f1","f2","f3","f4","f5","f6","f7",
                                          "mscut","Q10"),
                              baseParams <- baseParams)

chy.predict <- sensPredict(par.set=subset(pMLE, Site=="chy")[-1],
                              Nt=chy.Nt,
                              moist=chy.moist,
                              temp=chy.temp,
                              b=chy.b,
                              focus.par=c("c1","c2","c3","c4","c5","c6",
                                          "f1","f2","f3","f4","f5","f6","f7",
                                          "mscut","Q10"),
                              baseParams <- baseParams)

hys.predict <- sensPredict(par.set=subset(pMLE, Site=="hys")[-1],
                              Nt=hys.Nt,
                              moist=hys.moist,
                              temp=hys.temp,
                              b=hys.b,
                              focus.par=c("c1","c2","c3","c4","c5","c6",
                                          "f1","f2","f3","f4","f5","f6","f7",
                                          "mscut","Q10"),
                              baseParams <- baseParams)

knz.predict <- sensPredict(par.set=subset(pMLE, Site=="knz")[-1],
                              Nt=knz.Nt,
                              moist=knz.moist,
                              temp=knz.temp,
                              b=knz.b,
                              focus.par=c("c1","c2","c3","c4","c5","c6",
                                          "f1","f2","f3","f4","f5","f6","f7",
                                          "mscut","Q10"),
                              baseParams <- baseParams)

sevblu.predict$Site <- "sevblu"
sevblk.predict$Site <- "sevblk"
sgs.predict$Site <- "sgs"
chy.predict$Site <- "chy"
hys.predict$Site <- "hys"
knz.predict$Site <- "knz"

# Combine all site variables #
predict.all <- rbind(sevblu.predict,
                     rbind(sevblk.predict,
                          rbind(sgs.predict,
                                rbind(chy.predict,
                                      rbind(hys.predict,
                                            knz.predict)))))

#write.csv(predict.all, file="Sensitivity analysis//EcoRt_using baseline and MLE parameters individually_July2017.csv",row.names=F)
#write.csv(predict.diffs, file="Sensitivity analysis//Individual parameter effects on EcoRt_July2017.csv",row.names=F)


# Getting the difference between MLE and baseline EcoRt values ------------

predict.diffs <- ddply(predict.all, .(Site, parameter), summarize,
      par.effect = diff(Eco_Rt)*-1) # represents the effect on ecoRt going from baseline to MLE estimate


# Plotting differences ----------------------------------------------------
head(predict.diffs)
predict.diffs$Site <- factor(predict.diffs$Site, levels=c("sevblu","sevblk","sgs","chy","hys","knz"))
parEffects.plot <- ggplot(predict.diffs, aes(x=parameter, y=par.effect))+
  geom_bar(stat="identity", width=.5, col="black", fill="black")+
  facet_grid(Site ~ .)+
  theme_few()+
  xlab("Parameter")+
  ylab("Effect on ecosystem residence time")+
  geom_hline(yintercept=0, col="darkgrey")+
  theme(text=element_text(size=12))+
  theme(axis.text.x = element_text(angle=60, hjust=1))

ggsave("Figures//Parameter effects on EcoRt_all sites_July2017.pdf", parEffects.plot, height=9, width=4, units="in")



