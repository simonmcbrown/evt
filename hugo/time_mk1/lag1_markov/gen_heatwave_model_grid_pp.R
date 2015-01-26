# source("gen_heatwave_model_grid_pp.R")

source("/home/h03/hadsx/extremes/R/libs/pp_io.R")

source(file="~/extremes/tawn/hugo/time_mk1/SmallFuncs.R")
source(file="~/extremes/tawn/hugo/time_mk1/PotNllhGpd.R")
source(file="~/extremes/tawn/hugo/time_mk1/EcdfWithGpd.R")
source(file="~/extremes/tawn/hugo/time_mk1/BveHTDepPen.R")
source(file="~/extremes/tawn/hugo/time_mk1/DurSevFromObs_mk2_sjb.R")
source(file="~/extremes/tawn/hugo/time_mk1/DurSevFromSim_mk2.R")
source(file="~/extremes/tawn/hugo/time_mk1/ContourFromSevDur.R")
source(file="~/extremes/tawn/hugo/time_mk1/FwdBwdSimWithIntensityDiffPV.R")

library(VGAM)
library(extRemes)
library(MASS)
library(ismev)

DOSAVE <- FALSE

seas_length <- 90

stin   <- '/data/local/hadsx/model_data/gem2/es/ant/unpacked/all.ajayoa.pa.pp'
dirout <- paste(system('pwd',TRUE),'/hw_models/gem2/ant/',sep='')
stout0 <- 'DurSevFromObs_MMM_yYYY_xXXX.Rsave'

# Orleans 47.9025° N, 1.9090° E
# Value  277.562, (x,y)   1 110, (lat,long)   47.59    1.95 # nb idl indecies start from 0
#WAVEOFF-TIDL> print,whereis(pp1,0,99)     Longitude: 0,    Latitude: 33.75
#WAVEOFF-TIDL> print,whereis(pp1,0,121)    Longitude: 0,    Latitude: 61.25
#WAVEOFF-TIDL> print,whereis(pp1,20,99)    Longitude: 37.5, Latitude: 33.75
#WAVEOFF-TIDL> print,whereis(pp1,20,121)   Longitude: 37.5, Latitude: 61.25
# -> pp2 = pp_extract(pp1,[0,33.75,37.5,61.25])

#WAVEOFF-TIDL> print,whereis(pp1,-6,99)    Longitude: 348.75, Latitude: 33.75
# -> pp3 = pp_extract(pp1,[-12,33.75,37.5,61.25])
# SO NEED TO CALCULATE PARAMETER FOR POINTS WEST OF 0deg LONG -6 in IDL space
# Gem2 data [192, 145]

x0 <- 192-6
nx <- 6
y0 <- 100 # 100
ny <- 22

# prob point (x,y) (6,109)

#read.row.pp <- function(pp.file, irow, idxfields=NA, quiet=TRUE) {
#read.point.pp <- function(pp.file, ipt, irow, idxfields=NA, quiet=TRUE) {

for (y in y0:(y0+ny) ) {
#for (y in 109 ) {
    
    pprow0 <- read.row.pp(stin, y, quiet=FALSE)
    if (length(unique(pprow0$lbuser[4,])) !=1 | unique(pprow0$lbuser[4,]) != 3236) exit('ERROR in stash')

    #for (x in x0:(x0+nx) ) {
    for (x in 6 ) {
    
        cat('Doing (y,x) ',y,x,cr)
        
        idxm        <- which(pprow0$lbmon>=6 & pprow0$lbmon<=8)
        JJA.data.Tx <- pprow0$data[x,idxm]
        
        dat <- getTimeLagkData(dat=JJA.data.Tx,k=1,seas.length=seas_length)
        #dat <- dat[-(1:dim(dat)[1])[apply(X=dat< -100,MARGIN=1,FUN=any)],]   # Remove any missing values


        ##########################################
        # Fit the marginal dependence parameters #
        ##########################################

        # Bounds and starting value for L-BFGS-B style optimization #
        bl <- c(0.001,0.001,-Inf,0.001)
        bu <- c(1,Inf,Inf,0.9999)           
        start <- c(0.1,0.1,0.1,0.1)
        
        marg.gpd.fit <- NULL
        u.threshu    <- 0.905   # Modelling threshold u on uniform scale 
        retPeriod <- 1     # The return period (in years) associated with the desired critical level (in the paper v_j corresponds to critical level associated with j year return period)
        count     <- 0
        while( length(marg.gpd.fit) != 16 & count < 10) {
            v.threshu <- 1 - 1/(seas_length*retPeriod)   # Critical level v on uniform scale
            u.thresh  <- quantile(dat[,1],probs=u.threshu)   # The modelling threshold on the original margins
            #u.thresh <- quantile(JJA.temp,probs=u.threshu)   # Can go back to the original data to define quantile instead (better practice!)
            cat('u.thresh ',u.thresh,cr)
        
            #marg.gpd.fit <- try(optim(start,fn=PotNllhGpd,method="L-BFGS-B",data=dat,u=u.thresh,lower=bl,upper=bu,hessian=TRUE))
            marg.gpd.fit <- try(gpd.fit(JJA.data.Tx,u.thresh,show=FALSE))
 
            if (length(marg.gpd.fit) != 16) {
                u.threshu    <-  u.threshu + 0.005
                u.thresh  <- quantile(dat[,1],probs=u.threshu)   # The modelling threshold on the original margins
                count = count +1
            }
        }
        
        # Store the parameter outputs from the optim call. Gamma is the logistic dependence parameter #
        # and (sig.u,xi,lambda.u) are the scale, shape and rate parameters respectively               #
        ###gamma <- marg.gpd.fit$par[1]
        sig.u    <- marg.gpd.fit$mle[1]
        xi       <- marg.gpd.fit$mle[2]
        lambda.u <- marg.gpd.fit$rate

        # To obtain dependence parameters for semi-parametric conditional extremes approach need to transform #
        # data onto uniform margins by fitting eqn (10) of my paper above u and the empirical cdf below u     #
        # The work around added below means that there won't be a problem with slightly different ECDF functions
        # being fitted below the threshold
        # Remove the missing values before using GPD with ECDF to transform values 
        udat1 <- EcdfWithGpd(data=(JJA.data.Tx)[JJA.data.Tx>-100],p=c(sig.u,xi,lambda.u),u=u.thresh)
        udat2 <- JJA.data.Tx   # Need to re-paste in the missing values before the lagging is done
        udat2[udat2>-100] <- udat1   # Paste the unit values onto the original array
        udat <- getTimeLagkData(dat=udat2,k=1,seas.length=seas_length)   # Create the lagged data and remove any overlapping values between years
        udat <- udat[apply(udat>-100,MARGIN=1,FUN=all),]   # Remember to remove the missing values again!

        #################################
        # Fit the dependence parameters #
        #################################
        # Use hessian to obtain 95% CIs for each of the parameters #
        # Fit the penalised version of HT
        ht.dep.fit <- BveHTDepPen(dat=udat,u=u.threshu,vx=v.threshu,vy=v.threshu,nsim=10,sim.exc=rexp(10),bwd=0,lamPen=0)
        # ht.dep.fit$par == [alpha, beta, mu, sigma] - just want alpha & beta for forward fit
        
        # For the peak value simulation method will need to fit the H+T approach to the reversed data
        ht.dep.fit.bwd <- BveHTDepPen(dat=udat[,c(2,1)],u=u.threshu,vx=v.threshu,vy=v.threshu,nsim=10,sim.exc=rexp(10),bwd=0,lamPen=0)
        # ht.dep.fit.bwd$par == [alpha, beta, mu, sigma] - just want alpha & beta for backwards fit

        ###########################################################
        # Get the severities and durations for the                #
        # observed and simulated events and make the contour plot #
        ###########################################################
        #event.max <- c(33,max(JJA.data.Tx))   # Define the min and max peak values in the range to be tested over
        #event.max <- c(38,max(JJA.data.Tx))   # Define the min and max peak values in the range to be tested over
        event.max <- c(quantile(dat[,1],probs=.95),max(JJA.data.Tx))   # Define the min and max peak values in the range to be tested over

        obs.events <- DurSevFromObs_mk2_sjb(dat=JJA.data.Tx,thresh=u.thresh,max.lb=event.max[1],max.ub=event.max[2],pts.ba=9,seas.length=seas_length)
        
        sim.events <- DurSevFromSim_mk2(data=dat[,1],marg.obj=marg.gpd.fit,dep.obj=ht.dep.fit,
                      dep.obj.bwd=ht.dep.fit.bwd,thresh.u=u.threshu,crit.lev.u=v.threshu,max.lb=event.max[1],
                      max.ub=event.max[2],nsim=nsim)
        
        m1  <- strsplit(basename(stin),'.',fixed=T)[[1]][2]
        a   <- '0000'
        sx  <- as.character(x)
        sx2 <- paste(sprintf(paste("%.",nchar(a)-nchar(sx),"s",sep=""),a),sx,sep='')
        sy  <- as.character(y)
        sy2 <- paste(sprintf(paste("%.",nchar(a)-nchar(sy),"s",sep=""),a),sy,sep='')
        stout1 <- sub('MMM',m1,stout0)
        stout2 <- sub('XXX',sx2,stout1)
        stout3 <- sub('YYY',sy2,stout2)
        stout4 <- paste(dirout,stout3,sep='')
        
        if(DOSAVE) {
            save(file=stout4,obs.events,u.thresh,event.max,dat,marg.gpd.fit,ht.dep.fit,ht.dep.fit.bwd,u.threshu,v.threshu,seas_length)
            cat('Saved: ',basename(stout4),cr)
        } else readline('continue?') 
        
          # obs.event:
          #   (1) The severity values of each observed event
          #   (2) The duration distribution
          #   (3) The duration values of each observed event
          #   (4) The peak values for each observed cluster
          #   (5) The modelling threshold on original margins
    }

}