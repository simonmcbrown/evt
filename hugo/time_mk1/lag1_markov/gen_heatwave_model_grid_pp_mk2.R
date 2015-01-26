# source("gen_heatwave_model_grid_pp_mk2.R")

# mk2 puts chi into model fields directly

source("/home/h03/hadsx/extremes/R/libs/pp_io.R")

source(file="~/extremes/tawn/hugo/time_mk1/SmallFuncs.R")
#source(file="~/extremes/tawn/hugo/time_mk1/PotNllhGpd.R")
source(file="~/extremes/tawn/hugo/time_mk1/EcdfWithGpd.R")
source(file="~/extremes/tawn/hugo/time_mk1/BveHTDepPen.R")
#source(file="~/extremes/tawn/hugo/time_mk1/DurSevFromObs_mk2_sjb.R")
#source(file="~/extremes/tawn/hugo/time_mk1/DurSevFromSim_mk2.R")
#source(file="~/extremes/tawn/hugo/time_mk1/ContourFromSevDur.R")
#source(file="~/extremes/tawn/hugo/time_mk1/FwdBwdSimWithIntensityDiffPV.R")

library(VGAM)
library(extRemes)
library(MASS)
library(ismev)

DOSAVE <- FALSE

seas_length <- 90
nsim        <- 1e5

#stin   <- '/data/local/hadsx/model_data/gem2/es/ant/unpacked/all.ajayoa.pa.pp'
flist   <- system("ls -1 /data/local/hadsx/model_data/gem2/es/*/unpacked/all.*.pp", intern=TRUE) 
stout0  <- '/net/home/h03/hadsx/extremes/tawn/hugo/time_mk1/hw_models/gem2/'

for (f in flist) {

    stin   <- f
    runid  <- strsplit(stin,'.',fixed=T)[[1]][2]
    sthw1  <- sub('MMMM',runid,'DurSevFromObs_MMMM_')
    fcing  <- tail(strsplit(stin,'/',fixed=T)[[1]],3)[1]
    dirout <- paste(system('pwd',TRUE),sub('FFFF',fcing,'/hw_models/gem2/FFFF/'),sep='')

    # make pp structure to paste output into
    stpp <- '/net/home/h03/hadsx/extremes/tawn/hugo/heatwave/europe_bigger.pp'
    pp1  <- read.pp(stpp)
    pp1$data[,,] <- NA
    pp2  <- clone.pp(pp1, 16)
    stppout <- paste(stout0,sthw1,fcing,'_europe_bigger.pp',sep='')

    # Orleans 47.9025° N, 1.9090° E
    # Value  277.562, (x,y)   1 110, (lat,long)   47.59    1.95 # nb idl indecies start from 0
    # Gem2 data [192, 145]
    #WAVEOFF-TIDL> print,whereis(pp1,-6,99)    Longitude: 348.75, Latitude: 33.75

    # -> pp3 = pp_extract(pp1,[-12,33.75,37.5,61.25])
    # WAVEOFF-TIDL> info,pplandeu(0).data
    # <Expression>   FLOAT     = Array[27, 23]
    # bot l Value ********, (x,y)   0   0, (lat,long)   33.54  -11.81
    # bot r Value   14.528, (x,y)  26   0, (lat,long)   33.62   38.06
    # top l Value   42.862, (x,y)  26  22, (lat,long)   61.66   38.06
    # top r Value ********, (x,y)   0  22, (lat,long)   61.51  -11.81
    # -> gem2 coords for bot left is IDL[186,99] R[187,100]


    #x0 <- 192-6
    #nx <- 6
    x0 <- c(187:192,1:21)
    y0 <- 100 # 100
    ny <- 22

    # prob point (x,y) (6,109)

    #read.row.pp <- function(pp.file, irow, idxfields=NA, quiet=TRUE) {
    #read.point.pp <- function(pp.file, ipt, irow, idxfields=NA, quiet=TRUE) {

    for (y in y0:(y0+ny) ) {
    #for (y in 107 ) {
        
        pprow0 <- read.row.pp(stin, y, quiet=FALSE)
        if (length(unique(pprow0$lbuser[4,])) !=1 | unique(pprow0$lbuser[4,]) != 3236) exit('ERROR in stash')

        for (x in x0) {
        #for (x in 10:15 ) {
            #cat('Doing (y,x) ',y,x,cr)
            
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
            #u.threshu    <- 0.905   # Modelling threshold u on uniform scale 
            u.threshu    <- 0.90   # Modelling threshold u on uniform scale 
            retPeriod <- 1     # The return period (in years) associated with the desired critical level (in the paper v_j corresponds to critical level associated with j year return period)
            count     <- 0
            while( length(marg.gpd.fit) != 16 & count < 10) {
                v.threshu <- 1 - 1/(seas_length*retPeriod)   # Critical level v on uniform scale
                u.thresh  <- quantile(dat[,1],probs=u.threshu)   # The modelling threshold on the original margins
                #u.thresh <- quantile(JJA.temp,probs=u.threshu)   # Can go back to the original data to define quantile instead (better practice!)
                #cat('u.thresh ',u.thresh,cr)
            
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
            ht.dep.fit <- BveHTDepPen(dat=udat,u=u.threshu,vx=v.threshu,vy=v.threshu,nsim=nsim,sim.exc=rexp(nsim),bwd=0,lamPen=0)
            # ht.dep.fit$par == [alpha, beta, mu, sigma] - alpha & beta for forward fit
            # ht.dep.fit$x == X(t)
            # ht.dep.fit$y == X(t+1)
            chi.fwd  <- length(which(ht.dep.fit$y>v.threshu))/nsim
            
            # For the peak value simulation method will need to fit the H+T approach to the reversed data
            ht.dep.fit.bwd <- BveHTDepPen(dat=udat[,c(2,1)],u=u.threshu,vx=v.threshu,vy=v.threshu,nsim=nsim,sim.exc=rexp(nsim),bwd=0,lamPen=0)
            chi.bwd  <- length(which(ht.dep.fit.bwd$y>v.threshu))/nsim

            if(x>=x0[1]) x2 <- x - x0[1] + 1 else x2 <- x + 6
            cat(' x y ',x,y,x2,(y-y0+1),cr)
            pp2$data[ x2, (y-y0+1), 1] <- chi.fwd
            pp2$data[ x2, (y-y0+1), 2] <- chi.bwd
            pp2$data[ x2, (y-y0+1), 3] <- sig.u
            pp2$data[ x2, (y-y0+1), 4] <- xi
            pp2$data[ x2, (y-y0+1), 5] <- lambda.u
            pp2$data[ x2, (y-y0+1), 6:9] <- ht.dep.fit$par
            pp2$data[ x2, (y-y0+1), 10:13] <- ht.dep.fit.bwd$par
            pp2$data[ x2, (y-y0+1), 14] <- u.thresh 
            pp2$data[ x2, (y-y0+1), 15] <- u.threshu
            pp2$data[ x2, (y-y0+1), 16] <- v.threshu
            #cat('chi.bwd ',chi.bwd,cr)
            #readline("Continue 3")

        } # x

    } # y

    cat("Writing to ",stppout,cr)
    write.pp(pp2, stppout)

} # f loop