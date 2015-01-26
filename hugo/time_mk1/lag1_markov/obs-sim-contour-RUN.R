
################################################################
# Script to obtain marginal and dependence parameters and then #
# create contour plot from temperature observations            #
# DATE: 06/08/14                                               #
################################################################

# source("obs-sim-contour-RUN.R")

# What packages/libraries are required?
source(file="~/extremes/tawn/hugo/time_mk1/SmallFuncs.R")
source(file="~/extremes/tawn/hugo/time_mk1/PotNllhGpd.R")
source(file="~/extremes/tawn/hugo/time_mk1/EcdfWithGpd.R")
source(file="~/extremes/tawn/hugo/time_mk1/BveHTDepPen.R")
source(file="~/extremes/tawn/hugo/time_mk1/DurSevFromObs.R")
source(file="~/extremes/tawn/hugo/time_mk1/DurSevFromSim.R")
source(file="~/extremes/tawn/hugo/time_mk1/ContourFromSevDur.R")
source(file="~/extremes/tawn/hugo/time_mk1/FwdBwdSimWithIntensityDiffPV.R")

library(VGAM)
library(extRemes)
library(MASS)

DOPLOT <- FALSE
nsim   <- 1e6
savefile <- '/project/extremes1/hadsx/RSave.data/orleans_temp_dat_sim_1e6_b.RSave'

hw.data <- read.table("~/extremes/tawn/hugo/time_mk1/orleans_temp_dat.txt",header=T)   # Read in data from text file

years <- seq(1937,2013,by=1)
new.date <- numeric(0)


# The date is given in the form yyyymmdd where the example 19370101 #
# would imply 1st January 1937. The aim is to look solely at the    #
# summer months and as such it is necessary to look at months from  #
# yyyy0601-yyyy0831. To do this I create a new vector 'newdate' by  #
# removing the year*10000. In this way only pick the rows that are  #
# associated with the summer months.                                #

for (j in 1:length(hw.data$DATE)){
  for (i in 1:(length(years)-1)){
    if (hw.data$DATE[j]>=(years[i]*10000) && hw.data$DATE[j]<(years[i+1]*10000)){
      new.date <- c(new.date,(hw.data$DATE[j]-(years[i]*10000)))
    }
  }
}

hw.data.mat <- cbind(hw.data,new.date)

JJA.data <- hw.data.mat[which(hw.data.mat$new.date>531 & hw.data.mat$new.date<901),]

JJA.data <- JJA.data[JJA.data$DATE>19460000,]   # Choose to use data commencing on 1946 to avoid the missing data at the start of the series

dat <- getTimeLagkData(dat=JJA.data$TX*0.1,k=1,seas.length=92)
dat <- dat[-(1:dim(dat)[1])[apply(X=dat< -100,MARGIN=1,FUN=any)],]   # Remove any missing values


##########################################
# Fit the marginal dependence parameters #
##########################################

u.threshu <- 0.9   # Modelling threshold u on uniform scale 
retPeriod <- 1     # The return period (in years) associated with the desired critical level (in the paper v_j corresponds to critical level associated with j year return period)
v.threshu <- 1 - 1/(92*retPeriod)   # Critical level v on uniform scale

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
udat1 <- EcdfWithGpd(data=(JJA.data$TX*0.1)[JJA.data$TX>-100],p=c(sig.u,xi,lambda.u),u=u.thresh)
udat2 <- JJA.data$TX*0.1   # Need to re-paste in the missing values before the lagging is done
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

event.max <- c(33,40)   # Define the min and max peak values in the range to be tested over

obs.events <- DurSevFromObs(dat=JJA.data$TX*0.1,thresh=u.thresh,max.lb=event.max[1],max.ub=event.max[2],pts.ba=9)

cat("Starting simulation",cr)
sim.events <- DurSevFromSim(data=dat[,1],marg.obj=marg.gpd.fit,dep.obj=ht.dep.fit,
                      dep.obj.bwd=ht.dep.fit.bwd,thresh.u=u.threshu,crit.lev.u=v.threshu,max.lb=event.max[1],
                      max.ub=event.max[2],nsim=nsim)

save(file=savefile,obs.events,sim.events)

if(DOPLOT) {
postscript(file='~/extremes/tawn/hugo/time_mk1/severity_duration_orleans.ps',horiz=F)
  up.3()
  ContourFromSevDur(obs.obj=obs.events,sim.obj=sim.events,dur.bwd=2.2,grid.res=35,probs=c(0.75,0.9,0.95,0.99,0.997))
dev.off()
}