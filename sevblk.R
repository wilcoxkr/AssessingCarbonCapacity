### Modified Xu et al. (2006) program for grasslands, this version for Konza prairie based on EDGE and site level data ###
# July 7, 2015 -- modified Feb. 2016 KW#
# Konza #
rm(list=ls())
#install.packages("lubridate","ggplot2")
#require(lubridate)
#require(ggplot2)

### Set seed for random number generators ###
#options(digits.secs=4)
#seed.raw <- Sys.time()
#seed.1 <- second(seed.raw)
#set.seed(seed.1)

{######## Set variables #####################################################
  Nt = 		730
  m = 		6 	# 6 carbon compartments
  mscut = 	0.2
  cbnScale = 	1.0
}

{######## Generate function #################################################
  
  Generate.step <- function(c_op,f_op,mscut_op,D,cmin,cmax,fmin,fmax,mscut_min,mscut_max,Q10_min,Q10_max){
    while(TRUE){
      R_c <- runif(6,min=-0.5,max=0.5)
      R_f <- runif(7,min=-0.5,max=0.5)
      R_mscut <- runif(1,min=-0.5,max=0.5)
      R_Q10 <- runif(1,min=-0.5,max=0.5)
      c_new <- c_op + R_c * (cmax-cmin)/D 
      f_new <- f_op + R_f * (fmax-fmin)/D
      mscut_new <- mscut_op + R_mscut * (mscut_max-mscut_min)/D
      Q10_new <- Q10_op + R_Q10 * (Q10_max-Q10_min)/D
      if(	c_new[1] > cmin[1] & c_new[1] < cmax[1] &
          c_new[2] > cmin[2] & c_new[2] < cmax[2] &
          c_new[3] > cmin[3] & c_new[3] < cmax[3] &
          c_new[4] > cmin[4] & c_new[4] < cmax[4] &
          c_new[5] > cmin[5] & c_new[5] < cmax[5] &
          c_new[6] > cmin[6] & c_new[6] < cmax[6] &
          f_new[1] > fmin[1] & f_new[1] < fmax[1] &
          f_new[2] > fmin[2] & f_new[2] < fmax[2] &
          f_new[3] > fmin[3] & f_new[3] < fmax[3] &
          f_new[4] > fmin[4] & f_new[4] < fmax[4] &
          f_new[5] > fmin[5] & f_new[5] < fmax[5] &
          f_new[6] > fmin[6] & f_new[6] < fmax[6] &
          f_new[7] > fmin[7] & f_new[7] < fmax[7] &
          mscut_new > mscut_min & mscut_new < mscut_max &
          Q10_new > Q10_min & Q10_new < Q10_max){
        break
      }
    }
    return(c(c_new,f_new,mscut_new,Q10_new))
  }
  
}

{######## Solve_forward_2 function ##########################################
  solve_forward_2 <- function(tau.,c_new,f_new,b,u,x0,Nt,cbnScale){
    x <- matrix(0, nrow=length(c_new), ncol=Nt)
    ## These transfer proportions may need some tweaking, especially transfer from live roots - I put .6 to metabolic since this is the root turnover rate... 
    ## but I don't know that it necessarily translates, the rest I put towards structural litter
    AC <- matrix(0,nrow=length(c_new), ncol=length(c_new))
    AC[1,] <- c(    -c_new[1],    0,             0,               0,                   0,                0)
    AC[2,] <- c(	0,           -c_new[2],      0,               0,                   0,                0) ## KW Feb16: removed c_new[1] from adding to litter pool because site is burned annually
    AC[3,] <- c(	c_new[1],  	 c_new[2],    -c_new[3],      	  0,                   0,                0) # I'm not sure that all of c2 should go into litter... if plant respiration is occurring, this should detract from the amount of C in this pool... and since we put GPP into the model, this should be accounted for... ask Yiqi Zheng about this.
    AC[4,] <- c(	0,  		  0,   	   f_new[1]*c_new[3],   -c_new[4],       f_new[5]*c_new[5], f_new[7]*c_new[6])
    AC[5,] <- c(	0,            0,       f_new[2]*c_new[3], f_new[3]*c_new[4],   -c_new[5],            0)
    AC[6,] <- c(    0,      	  0,             0,           f_new[4]*c_new[4], f_new[6]*c_new[5],     -c_new[6]) #the constant matrix A
    
    x_last <- x0
    for(i in 1:Nt){
      x_present <- (diag(6) + AC*tau.[i]) %*% x_last + b*u[i]*cbnScale # Corresponds to Eq 1 in Xu et al (2006)
      x[,i] <- x_present
      x_last <- x_present
    }	
    return(x)
  }		
}

{######## Prior c and f -- i.e. transfer and efflux coefficients #############################
  c. <- c(1.0e-3, 9.0e-3, 9.0e-3, 1.5e-2, 6.0e-4, 2.0e-5)
  #	f. <- c(0.45, 0.10, 0.296, 0.004, 0.42, 0.01, 0.45) ## old transfer coefficients from Zheng's paper
  f. <- c(0.25, 0.10, 0.5, 0.004, 0.42, 0.05, 0.45) ## Based on findings from Zhou ##
  ######### Prior soil temp and moisture modifiers for tau #####################################
  mscut <- 0.2
  Q10 <- 2.2
  # Set minimum limits of c flux ranges # -- Need to figure out where Xu got these numbers #
  cmin <- numeric(6)
  cmin[1] <- 1.0e-4 
  cmin[2] <- 1.0e-4 # all same as Zheng used
  cmin[3] <- 5.0e-4 # changed from 1e-4 -- set at 5.0e-4 to limit to 5 year residence time
  cmin[4] <- 5.0e-3 # changed from 1e-4 -- set at 2.7e-3 to limit to 1 year residence time
  cmin[5] <- 1.0e-5 # 
  cmin[6] <- 1.0e-8
  #cmin[7] <- 0.47
  # Set maximum limits of c flux ranges #
  cmax <- numeric(6)
  cmax[1] <- 1.0e-2
  cmax[2] <- 1.0e-2 ## KW: increased from 1.0e-2 based on black grama output
  cmax[3] <- 2.0e-2 # KW I decreased this from 2.0e-2 based on black grama output
  cmax[4] <- 5.0e-2 # decreased this from 5.0e-2  for blue
  cmax[5] <- 2.0e-3 # KW: I increased this from 2.0e-3 based on blue grama output
  cmax[6] <- 3.0e-5
  #cmax[7] <- 0.77
  
  # prior f -- actually i think this is just the individual transfer rates among pools #
  # f43=0.45;f53=0.10;f54=0.296;f64=0.004;f45=0.42;f65=0.03;f46=0.45 #
  # f=[0.45 0.10 0.296 0.004 0.42 0.01 0.45]' #
  fmin <- numeric(7)
  fmin[1] <- 3.0e-1 ## changed from 3.0e-1 
  fmin[2] <- 5.0e-2 # same as Zheng used
  fmin[3] <- 2.5e-1 # modified from 2.0e-1 
  fmin[4] <- 1.0e-3 # Changed from 0.0
  fmin[5] <- 1.0e-1
  fmin[6] <- 0.2e-2 # changed from 0.0
  fmin[7] <- 3.0e-1
  # Max C transfer #
  fmax <- numeric(7)
  fmax[1] <- 7.0e-1 ## changed from 7.0e-1
  fmax[2] <- 1.5e-1 # same as Zheng used
  fmax[3] <- 6.5e-1 # modified from 7.0e-1 
  fmax[4] <- 8.0e-3 # same as Zheng used
  fmax[5] <- 6.0e-1 # same as Zheng used
  fmax[6] <- 7.0e-2 # changed from 2.0e-2
  fmax[7] <- 7.0e-1 # same as Zheng used
  ## soil moisture cutoff max and min ##
  mscut_min <- .1 ## WILTING POINT-ISH... this is the lowest soil moisture gets.
  mscut_max <- .40
  ## soil temperature tau modifier Q10 -- max and min ##
  Q10_min <- 1.0
  Q10_max <- 4.0
}


{######### Set starting C pools ##############################################
  ### SEV black C pools: 
  ### X1foliage - 22.7 g/m2 -- this is standing aboveground biomass for 2014 (since that is the only year I have belowground biomass then 50.54 x 0.45
  ### X2fineroots - 27.1 g/m2 -- this is standing crop root biomass from 2014 = 60.33 * 0.45 =  27.1gC/m2
  ### X3litter - 41.0 g C m-2 -- got this (fine litter #) from running the SEV forcing data through TECO
  ### X4 microbial C - 64.5 g C m-2 -- From Kieft 1994 paper: microbial C = 6% of TOC.. so applying Laura's TOC, we get 64.5g microbial C/m2 for black grama 
  ### X5 Slow SOM -- 631.7 g C m-2 -- using proportions from Parton et al. 1988 for slow and passive C applied to Laura's TOC data -- see analysis log for more details
}### X6 Passive SOM -- 379.0 g C m-2 -- using proportions from Parton et al. 1988 for slow and passive C applied to Laura's TOC data -- see analysis log for more details

x0 =		c(22.7,27.1,41.0,64.5,631.7,379.0) # Initial values of C pools

{######## Input matrix -- The proportion of C from Phs to above versus belowground pools... Using EDGE knz data and data from Wilcox et al. 2015 GCB to estimate average BNPP:ANPP -- mean(.55, .55, 0.50, 1.0) = 0.65
  ### So, root:shoot=2.1 meaning proportions of C aboveground is 1/3.1=0.32 and belowground is 2.1/3.1=0.68
  # aboveground gets 71% biomass C and belowground gets 29% -- Aboveground: 0.55*.71= .39, belowground: .55*0.29=0.16
  b = c(0.18,0.37,0,0,0,0) # Took .55 which is what Xu et al 2006 put as total Phs going to biomass (I could always look this up for Konza) and divided it up based on root:shoot ratio from the site - 0.65 for sev
  # Read in Carbon (from phs) data set --  % u1 is C input in ambient, u2 is C in elevated #
  cbn = read.csv( "gpp_daily_black_modbyyr.csv") # Run climate forcing data through TECO to get gpp -- having trouble getting it to match up quite right due to very high production values in 2013... #
  u = cbn$gpp
}

{######## The following code is from the temp_moist subroutine ##############	
  # Read in and set soil moisture data #
  moist_1 <- read.csv("moist5cm_black_sev13_14_fromSite_tao.csv")
  moist <- moist_1$Smoist
  # Read in and set soil temperature data #
  temp_1 <- read.csv("temp5cm_black_sev13_14_fromSite_tao.csv") 
  temp <- temp_1$Stemp

  tau.fxn <- function(Nt, temp, moist, mscut, Q10){ # Function to create tau for each day in simulation
    product <- numeric(Nt+1)
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
    }
    return(product)
  }	
}############## End of temp_moist subroutine ##################

tau. <- tau.fxn(Nt,temp,moist,mscut,Q10) # This is the turnover rate of C across all pools

{######## Read in data sets #################################################
  soil_respiration <- read.csv("soil_resp_black.csv")
  soil_respiration <- soil_respiration[!is.na(soil_respiration$FlxCO2),] 
  soilTime <- soil_respiration$days
  soilResValue <- soil_respiration$FlxCO2
  SCabove <- read.csv("SCabove_black.csv")
  SCaboveTime <- SCabove$day
  SCaboveValue <- SCabove$stcrop *0.45## Convert to g C m-2
  SCbelow <- read.csv("SCbelow_black.csv") # 
  SCbelowTime <- SCbelow$day
  SCbelowValue <- SCbelow$stcrop *0.45 ## Convert to g C m-2
  
  # Soil carbon # 
  soilMineralCarbonTime <- 549 # Lara Ladwig's sev measurement ... assuming it's stable
  mineralCbnValue <- 1075.2 # sev measurement ... assuming it's stable
}

{######## Mappings of which C pools (from model) make up each value given by each data set ####################
  #phi_slResp          <- c(0, 	0.25*c.[2],  0.55*c.[3],   0.7*c.[4],   0.55*c.[5],    0.55*c.[6]) Maybe I should add root respiration into this? although perhaps all roots are dead in the CO2 collars
  phi_slResp          <- c(0, 	    0,
                           (1-f.[1]-f.[2])*c.[3],
                           (1-f.[3]-f.[4])*c.[4],
                           (1-f.[5]-f.[6])*c.[5],
                           (1-f.[7]-f.[6])*c.[6])
  phi_below        	<- c(0,         1,         0,         0,         0,        0)
  phi_above 		    <- c(1,       0,         0,         0,         0,        0)
  # Will want to incorporate turnover here once we have standing crop data; 	phi_litterfall      <- c(0.75*c.[1], 0.75*c.[2], 0,         0,         0,        0,         0)
  phi_cMineral        <- c(0,         0,         0,         1,         1,        1)
  #phi_litter 			<- c(0,			0,			1,		0,			0,			0)			
  cdif  <- (cmax-cmin)
  fdif <- fmax-fmin
  mscut_dif <- mscut_max-mscut_min
  Q10_dif <- Q10_max-Q10_min
}

{######## Simulation variables ##############################################
  c_op <- cmin+runif(1,min=0,max=1)*cdif
  f_op <- fmin+runif(1,min=0,max=1)*fdif
  mscut_op <- mscut_min+runif(1,min=0,max=1)*mscut_dif
  Q10_op <- Q10_min+runif(1,min=0,max=1)*Q10_dif
  
  J_last <- 300000 # Cost metric -- starts high then minimizes as simulation runs (I think!)
  c_new <- numeric(6)
  f_new <- numeric(7)
  mscut_new <- 0
  Q10_new <- 0
  nsim <- 360000
  nchains <- 4
  record_index <- 1
  # Include once I get data; 	
  DJ1 <- 2*var(soilResValue, na.rm=TRUE) # Error of the observed data
  DJ2 <- 2*var(SCaboveValue)
  DJ3 <- 50 # SCbelowValue -- # Constrain variance at 50 since I only have one measurement #
  DJ4 <- 10000 # mineralCbnValue -- #  Only have one measurement
} ### End sim var


{######## Simulation starts #################################################
  c_array <- array(0,c(nrow=nsim+1,ncol=6,nchains)) # Create array to populate with chains of parameter estimates #
  f_array <- array(0,c(nrow=nsim+1,ncol=7,nchains)) # Create array to populate with chains of parameter estimates #
  mscut_array <- array(0,c(nrow=nsim+1,ncol=1,nchains)) # Create array to populate with chains of parameter estimates #
  Q10_array <- array(0,c(nrow=nsim+1,ncol=1,nchains)) # Create array to populate with chains of parameter estimates #
  
  for(chain in 1:nchains){
    ### Set seed for random number generators ###
    #options(digits.secs=4)
    #seed.raw <- Sys.time()
    #seed.1 <- second(seed.raw)
    #set.seed(seed.1)
    # Reset counters etc. #
    c_op <- cmin+runif(1,min=0,max=1)*cdif # randomly chooses a number in the coefficient limits
    f_op <- fmin+runif(1,min=0,max=1)*fdif # randomly chooses a number in the coefficient limits
    mscut_op <- mscut_min+runif(1,min=0,max=1)*mscut_dif
    Q10_op <- Q10_min+runif(1,min=0,max=1)*Q10_dif
    J_last <- 300000 # Not sure what this is...
    c_new <- numeric(6)
    f_new <- numeric(7)
    mscut_new <- 0
    Q10_new <- 0
    record_index <- 1
    upgraded <- 0
    c_upgraded <- t(c_op)
    f_upgraded <- t(f_op)
    mscut_upgraded <- t(mscut_op)
    Q10_upgraded <- t(Q10_op)
    c_rec <- t(c_op)
    f_rec <- t(f_op)
    mscut_rec <- mscut_op
    Q10_rec <- Q10_op
    J_record <- J_last
    
    for(simu in 1:nsim){
      counter <- simu
      upgradedd <- upgraded
      #	c_new <- Generate(c_op,transT,eigV,cmin,cmax)	# Randomly generates a number within parameter limits - uses covariance matrix
      gen.temp <- Generate.step(c_op=c_op,f_op=f_op,mscut_op=mscut_op,D=15,cmin=cmin,cmax=cmax,fmin=fmin,fmax=fmax,mscut_min=mscut_min,mscut_max=mscut_max,Q10_min=Q10_min,Q10_max=Q10_max)	# Randomly generates a number within parameter limits - uses step size of 5
      c_new <- gen.temp[1:6] # Output from generate function is all together so I needed to separate out c and f coefficients
      f_new <- gen.temp[7:13]
      mscut_new <- gen.temp[14]
      Q10_new <- gen.temp[15]
      phi_slResp          <- c(0, 	    0,
                               (1-f_new[1]-f_new[2])*c_new[3],
                               (1-f_new[3]-f_new[4])*c_new[4],
                               (1-f_new[5]-f_new[6])*c_new[5],
                               (1-f_new[7]-f_new[6])*c_new[6])
      #	phi_litterfall <- c(0.75*c_new[1], 	0.75*c_new[2], 	0,0,0,0,0)
      tau.new <- tau.fxn(Nt,temp,moist,mscut=mscut_new,Q10=Q10_new) # new tau with mscut_new
      
      x <- solve_forward_2(tau.new, c_new, f_new, b, u, x0, Nt, cbnScale) # Solves forward problem (whatever that is: KW)
      
      # Simulations of data #
      soilResp_simu <- numeric(length(soilTime)) # Simulate soil respiration
      SCabove_simu <- numeric(length(SCaboveTime))
      SCbelow_simu <- numeric(length(SCbelowTime))
      cMineral_simu <- numeric(length(soilMineralCarbonTime))
      for(i in 1:length(soilTime)){
        soilResp_simu[i] <- tau.new[soilTime[i]] * phi_slResp %*% x[,soilTime[i]] + 0.25*(1-b[1] - b[2])*u[soilTime[i]]
      }
      for(i in 1:length(SCaboveTime)){
        SCabove_simu[i] <- phi_above %*% x[,SCaboveTime[i]]
      }
      for(i in 1:length(SCbelowTime)){
        SCbelow_simu[i] <- phi_below %*% x[,SCbelowTime[i]]
      }
      for(i in 1:length(soilMineralCarbonTime)){
        cMineral_simu[i] <- phi_cMineral %*% x[,soilMineralCarbonTime[i]]
      }
      
      J <- numeric(4) # Takes the e(t) -- the observed minus modelled value at time t -- 
      J[1]  =   (norm(as.matrix(soilResp_simu-soilResValue),"f"))^2 # simulated versus observed soil respiration squared
      J[2]  =   (norm(as.matrix(SCabove_simu-SCaboveValue),"f"))^2
      J[3]  =   (norm(as.matrix(SCbelow_simu-SCbelowValue),"f"))^2
      J[4]  =   (norm(as.matrix(cMineral_simu-mineralCbnValue),"f"))^2		
      
      J_new <- (J[1]/DJ1+J[2]/DJ2+J[3]/DJ3+J[4]/DJ4)
      delta_J <- J_new-J_last	
      
      if (min(1,exp(-delta_J)) > runif(1,min=0,max=1)){ # 
        c_op <- c_new
        f_op <- f_new
        mscut_op <- mscut_new
        Q10_op <- Q10_new
        J_last <- J_new
        upgraded <- upgraded+1
        c_upgraded <- rbind(c_upgraded, t(c_op)) # The first row of this matrix will be the c_op calculated outside this loop
        f_upgraded <- rbind(f_upgraded, t(f_op)) # The first row of this matrix will be the c_op calculated outside this loop
        mscut_upgraded <- c(mscut_upgraded, mscut_op) # The first row of this matrix will be the c_op calculated outside this loop
        Q10_upgraded <- c(Q10_upgraded, Q10_op) # The first row of this matrix will be the c_op calculated outside this loop
      }		
      c_rec <- rbind(c_rec, t(c_op)) # The first row of this matrix will be the c_op calculated outside this loop
      f_rec <- rbind(f_rec, t(f_op)) # The first row of this matrix will be the c_op calculated outside this loop
      mscut_rec <- c(mscut_rec, mscut_op) # The first row of this matrix will be the c_op calculated outside this loop
      Q10_rec <- c(Q10_rec, Q10_op) # The first row of this matrix will be the c_op calculated outside this loop
      J_record <- c(J_record,J_last) # Same as above line but first entry will be first J_last
      record_index <- record_index+1
      print(paste("chain",chain,"sim",record_index))
    }	### End simulation loop ###
    c_array[,,chain] <- c_rec
    f_array[,,chain] <- f_rec
    mscut_array[,,chain] <- mscut_rec
    Q10_array[,,chain] <- Q10_rec
  }	### End Chain loop ###
  
} ### End Sim section ###	

############# Write arrays to excel file ##################
c_4write <- rbind(data.frame(sim=1:nrow(c_array[,,1]),chain=1,c_array[,,1],J_record),data.frame(sim=1:nrow(c_array[,,2]),chain=2,c_array[,,2],J_record),data.frame(sim=1:nrow(c_array[,,3]),chain=3,c_array[,,3],J_record),data.frame(sim=1:nrow(c_array[,,4]),chain=4,c_array[,,4],J_record))
write.csv(c_4write, file="sevblk_c params_360000 4chains_13-15.csv", row.names=FALSE)
f_4write <- rbind(data.frame(sim=1:nrow(f_array[,,1]),chain=1,f_array[,,1]),data.frame(sim=1:nrow(f_array[,,2]),chain=2,f_array[,,2]),data.frame(sim=1:nrow(c_array[,,3]),chain=3,f_array[,,3]),data.frame(sim=1:nrow(c_array[,,4]),chain=4,f_array[,,4]))
write.csv(f_4write, file="sevblk_f params_360000 4chains_13-15.csv", row.names=FALSE)
mscut_4write <- rbind(data.frame(sim=1:length(mscut_array[,,1]),chain=1,mscut=mscut_array[,,1]),data.frame(sim=1:length(mscut_array[,,2]),chain=2,mscut=mscut_array[,,2]),data.frame(sim=1:length(mscut_array[,,3]),chain=3,mscut=mscut_array[,,3]),data.frame(sim=1:length(mscut_array[,,4]),chain=4,mscut=mscut_array[,,4]))
write.csv(mscut_4write, file="sevblk_mscut params_360000 4chains_13-15.csv", row.names=FALSE)
Q10_4write <- rbind(data.frame(sim=1:length(Q10_array[,,1]),chain=1,Q10=Q10_array[,,1]),data.frame(sim=1:length(Q10_array[,,2]),chain=2,Q10=Q10_array[,,2]),data.frame(sim=1:length(Q10_array[,,3]),chain=3,Q10=Q10_array[,,3]),data.frame(sim=1:length(Q10_array[,,4]),chain=4,Q10=Q10_array[,,4]))
write.csv(Q10_4write, file="sevblk_Q10 params_360000 4chains_13-15.csv", row.names=FALSE)
