
################################################################
# Script to obtain marginal and dependence parameters and then #
# create contour plot from temperature observations            #
# DATE: 06/08/14                                               #
################################################################

# source("obs-sim-contour-RUN_gem2-multi.R")

# 2014.10.27 gem2 version to take hadGEM2 data

# What packages/libraries are required?
source(file="~/extremes/tawn/hugo/time_mk1/SmallFuncs.R")
source(file="~/extremes/tawn/hugo/time_mk1/PotNllhGpd.R")
source(file="~/extremes/tawn/hugo/time_mk1/EcdfWithGpd.R")
source(file="~/extremes/tawn/hugo/time_mk1/BveHTDepPen.R")
source(file="~/extremes/tawn/hugo/time_mk1/DurSevFromObs_mk2_sjb.R")
source(file="~/extremes/tawn/hugo/time_mk1/DurSevFromSim.R")
source(file="~/extremes/tawn/hugo/time_mk1/ContourFromSevDur.R")
source(file="~/extremes/tawn/hugo/time_mk1/FwdBwdSimWithIntensityDiffPV.R")

library(VGAM)
library(extRemes)
library(MASS)

DOPLOT <- FALSE
nsim   <- 1e5
nfiles <- 20

### orleans
#infile    <- '~/extremes/tawn/hugo/heatwave/gem2/orleans/nat_1234_allyr_daily_tmax_1950_2011.csv'
#savefile0 <- '/project/extremes1/hadsx/RSave.data/orleans_gem2_nat_temp_dat_sim_1e5_xxx.RSave'
#psout      <- '~/extremes/tawn/hugo/time_mk1/severity_duration_orleans_gem2_nat.ps'

#infile    <- '~/extremes/tawn/hugo/heatwave/gem2/orleans/ghg_1234_allyr_daily_tmax_1950_2005.csv'
#savefile0 <- '/project/extremes1/hadsx/RSave.data/orleans_gem2_ghg_temp_dat_sim_1e5_xxx.RSave'
#psout      <- '~/extremes/tawn/hugo/time_mk1/severity_duration_orleans_gem2_ghg.ps'

infile    <- '~/extremes/tawn/hugo/heatwave/gem2/orleans/ant_1234_allyr_daily_tmax_1950_2005.csv'
savefile0 <- '/project/extremes1/hadsx/RSave.data/orleans_gem2_ant_temp_dat_sim_1e5_xxx.RSave'
psout      <- '~/extremes/tawn/hugo/time_mk1/severity_duration_orleans_gem2_ant_mk2.ps'

### toulouse
#infile    <- '~/extremes/tawn/hugo/heatwave/gem2/toulouse/nat_1234_allyr_daily_tmax_1950_2011.csv'
#savefile0 <- '/project/extremes1/hadsx/RSave.data/toulouse_gem2_nat_temp_dat_sim_1e5_xxx.RSave'
#psout     <- '~/extremes/tawn/hugo/time_mk1/severity_duration_toulouse_gem2_nat.ps'

#infile    <- '~/extremes/tawn/hugo/heatwave/gem2/toulouse/ghg_1234_allyr_daily_tmax_1950_2005.csv'
#savefile0 <- '/project/extremes1/hadsx/RSave.data/toulouse_gem2_ghg_temp_dat_sim_1e5_xxx.RSave'
#psout     <- '~/extremes/tawn/hugo/time_mk1/severity_duration_toulouse_gem2_ghg.ps'

hw.data <- read.csv(infile,header=T)   # Read in data from text file
seas_length <- 90

# select summer data
JJA.data.Tx <- hw.data$Tmax[hw.data$Month>=6 & hw.data$Month<=8]-273.15

dat <- getTimeLagkData(dat=JJA.data.Tx,k=1,seas.length=seas_length)
#dat <- dat[-(1:dim(dat)[1])[apply(X=dat< -100,MARGIN=1,FUN=any)],]   # Remove any missing values


##########################################
# Fit the marginal dependence parameters #
##########################################

u.threshu <- 0.905   # Modelling threshold u on uniform scale 
retPeriod <- 1     # The return period (in years) associated with the desired critical level (in the paper v_j corresponds to critical level associated with j year return period)
v.threshu <- 1 - 1/(seas_length*retPeriod)   # Critical level v on uniform scale

u.thresh <- quantile(dat[,1],probs=u.threshu)   # The modelling threshold on the original margins
#u.thresh <- quantile(JJA.temp,probs=u.threshu)   # Can go back to the original data to define quantile instead (better practice!)
print(u.thresh)

# Bounds and starting value for L-BFGS-B style optimization #
bl <- c(0.001,0.001,-Inf,0.001)
bu <- c(1,Inf,Inf,0.9999)           
start <- c(0.1,0.1,0.1,0.1)

marg.gpd.fit <- optim(start,fn=PotNllhGpd,method="L-BFGS-B",data=dat,u=u.thresh,lower=bl,upper=bu,hessian=TRUE)

# Store the parameter outputs from the optim call. Gamma is the logistic dependence parameter #
# and (sig.u,xi,lambda.u) are the scale, shape and rate parameters respectively               #
gamma <- marg.gpd.fit$par[1]
sig.u <- marg.gpd.fit$par[2]
xi <- marg.gpd.fit$par[3]
lambda.u <- marg.gpd.fit$par[4]


# To obtain dependence parameters for semi-parametric conditional extremes approach need to transform #
# data onto uniform margins by fitting eqn (10) of my paper above u and the empirical cdf below u     #
# The work around added below means that there won't be a problem with slightly different ECDF functions
# being fitted below the threshold
# Remove the missing values before using GPD with ECDF to transform values 
udat1 <- EcdfWithGpd(data=(JJA.data.Tx)[JJA.data.Tx>-100],p=c(sig.u,xi,lambda.u),u=u.thresh)
udat2 <- JJA.data.Tx   # Need to re-paste in the missing values before the lagging is done
udat2[udat2>-100] <- udat1   # Paste the unit values onto the original array
udat <- getTimeLagkData(dat=udat2,k=1,seas.length=92)   # Create the lagged data and remove any overlapping values between years
udat <- udat[apply(udat>-100,MARGIN=1,FUN=all),]   # Remember to remove the missing values again!


#################################
# Fit the dependence parameters #
#################################

# Use hessian to obtain 95% CIs for each of the parameters #
ht.dep.fit <- BveHTDepPen(dat=udat,u=u.threshu,vx=v.threshu,vy=v.threshu,nsim=10000,sim.exc=rexp(10000),
                          bwd=0,lamPen=0)  # Fit the penalised version of HT

# For the peak value simulation method will need to fit the H+T approach to the reversed data
ht.dep.fit.bwd <- BveHTDepPen(dat=udat[,c(2,1)],u=u.threshu,vx=v.threshu,vy=v.threshu,nsim=10000,
                              sim.exc=rexp(10000),bwd=0,lamPen=0)


###########################################################
# Get the severities and durations for the                #
# observed and simulated events and make the contour plot #
###########################################################

#event.max <- c(33,max(JJA.data.Tx))   # Define the min and max peak values in the range to be tested over
#event.max <- c(38,max(JJA.data.Tx))   # Define the min and max peak values in the range to be tested over
event.max <- c(quantile(dat[,1],probs=.95),max(JJA.data.Tx))   # Define the min and max peak values in the range to be tested over

obs.events <- DurSevFromObs_mk2_sjb(dat=JJA.data.Tx,thresh=u.thresh,max.lb=event.max[1],max.ub=event.max[2],pts.ba=9,seas.length=seas_length)

cat("Starting simulation",cr)
for(b in 1:nfiles) {
  sim.events <- DurSevFromSim(data=dat[,1],marg.obj=marg.gpd.fit,dep.obj=ht.dep.fit,
                      dep.obj.bwd=ht.dep.fit.bwd,thresh.u=u.threshu,crit.lev.u=v.threshu,max.lb=event.max[1],
                      max.ub=event.max[2],nsim=nsim)
#readline('Stop')

  sa <- '0000'
  sb <- paste(b)
  sc <- paste(sprintf(paste("%.",nchar(sa)-nchar(sb),"s",sep=""),sa),sb,sep='')

  savefile <- sub('xxx',sc,savefile0)
  save(file=savefile,obs.events,sim.events)
  cat("Saved ",b,savefile,cr)
}

if(DOPLOT) {
postscript(file=psout,horiz=F)
  up.3()
  ContourFromSevDur(obs.obj=obs.events,sim.obj=sim.events,dur.bwd=2.2,grid.res=35,probs=c(0.75,0.9,0.95,0.99,0.997))
dev.off()
}