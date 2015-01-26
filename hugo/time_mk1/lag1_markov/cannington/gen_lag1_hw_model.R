# source("gen_lag1_hw_model.R")

#savefile0 <- 'test_cannington_xxx.RSave'
savefile0 <- '/project/extremes1/hadsx/RSave.data/cannington_sim_1e5_xxx.RSave'


st1 <- '/net/home/h03/hadsx/extremes/consulting/edf/data/BrymoreCannington_tmax_tmin.csv'
DOPLOT <- T

df1 <- read.csv(st1,as.is=T)

can.tx  <- df1[[13]]
can.tn  <- df1[[18]]
can.dtx <- strptime(df1[[12]], "%d-%b-%y")
can.dtn <- strptime(df1[[17]], "%d-%b-%y")
#plot(can.dtx, can.tx

bry.tx <- df1[[3]]
bry.tn <- df1[[8]]
bry.dtx <- strptime(df1[[2]], "%d-%b-%y")
bry.dtn <- strptime(df1[[7]], "%d-%b-%y")
#plot(bry.dtx, bry.tx)
if(DOPLOT) plot(can.dtx, can.tx,xlim=as.double(c(bry.dtx[1],max(can.dtx,na.rm=T))))
if(DOPLOT) points(bry.dtx, bry.tx,cex=.3,col=2)

# to cope with missing data create a contiguous timeseries from start_date to end_date and then fill in data.  Set missing to -100
dsta <- min(c(bry.dtx,can.dtx),na.rm=T)
dend <- max(c(bry.dtx,can.dtx),na.rm=T)
dtx0 <- seq(from=dsta,to=dend,by="day")
temp0 <- strftime(dtx0,format="%Y-%m-%d") #  need this shimmy to get rid of BST
dtx0  <- strptime(temp0,"%Y-%m-%d")
temp0 <- NA
tx0  <- rep(-100.0,length=length(dtx0))

ix2d <-indexforcommon(as.integer(as.double(dtx0)),as.integer(as.double(bry.dtx)))
 tx0[ix2d[,1]]  <-  bry.tx[ix2d[,2]]
dtx0[ix2d[,1]]  <- bry.dtx[ix2d[,2]]
ix2d <-indexforcommon(as.integer(as.double(dtx0)),as.integer(as.double(can.dtx)))
 tx0[ix2d[,1]]  <-  can.tx[ix2d[,2]]
dtx0[ix2d[,1]]  <- can.dtx[ix2d[,2]]

if(DOPLOT) points(dtx0, tx0,col=3,cex=.3)

mtx    <- as.integer(format(dtx0,"%m")) 
imjjas <- which(mtx >= 5 & mtx <= 9)
if(DOPLOT) points(dtx0[imjjas], tx0[imjjas],col=4,cex=.3)
tx  <- tx0[imjjas]
dtx <- dtx0[imjjas]
seas_length <- 31+30+31+31+30
uy    <- unique(as.integer(format(dtx,"%Y")) )
for(y in uy) {
    nday <- length(which(as.integer(format(dtx,"%Y")) == y))
    if(nday != seas_length) cat(y,' has only ',nday, ' days',cr)
}

# remove missing values
i   <- which(!is.finite(tx))
tx[i]  <- -100

#source(file="~/extremes/tawn/hugo/libs_git_working_copies/key_code/Rcode/functions/GetTimeLagkData.R")
source(system("echo $GITHUGO/key_code/Rcode/functions/GetTimeLagkData.R",TRUE))
source(system("echo $GITHUGO/key_code/Rcode/functions/EcdfWithGpd.R",TRUE))
source(system("echo $GITHUGO/key_code/Rcode/functions/BveHTDepPen.R",TRUE))
source(system("echo $GITHUGO/temporal/Rcode/functions/DurSevFromObs.R",TRUE))
source(system("echo $GITHUGO/temporal/Rcode/functions/DurSevFromSim.R",TRUE))
source(system("echo $GITHUGO/temporal/Rcode/functions/SmallFuncs.R",TRUE))
source(system("echo $GITHUGO/temporal/Rcode/functions/FwdBwdSimWithIntensityDiffPV.R",TRUE))
source(system("echo $GITHUGO/temporal/Rcode/functions/CondDepToDurDistFuncs.R",TRUE))
#source(system("echo $GITHUGO/",TRUE))
source(system("echo $HOME/extremes/tawn/hugo/time_mk1/sjb_fn/decluster.runs.R",TRUE))

library(VGAM)
library(ismev)

nsim    <- 1e5
nfiles  <- 20

##########################################
# get lag 1 dependence
##########################################
dat <- GetTimeLagkData(dat=tx,k=1,seas.length=seas_length)

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
    marg.gpd.fit <- try(gpd.fit(tx,u.thresh,show=FALSE))

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
udat1 <- EcdfWithGpd(data=(tx)[tx>-100],p=c(sig.u,xi,lambda.u),u=u.thresh)
udat2 <- tx   # Need to re-paste in the missing values before the lagging is done
udat2[udat2>-100] <- udat1   # Paste the unit values onto the original array
udat <- GetTimeLagkData(dat=udat2,k=1,seas.length=seas_length)   # Create the lagged data and remove any overlapping values between years
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
chi.bwd        <- length(which(ht.dep.fit.bwd$y>v.threshu))/nsim

event.max <- c(quantile(dat[,1],probs=.95),max(tx))   # Define the min and max peak values in the range to be tested over

obs.events <- DurSevFromObs(dat=tx,thresh=u.thresh,max.lb=event.max[1],max.ub=event.max[2],pts.ba=9,seas.length=seas_length)

cat("Starting simulation",cr)
for(b in 1:nfiles) {
  dummy.marg.gpd.fit <- list(par=c(NA,sig.u,xi,lambda.u))
  sim.events <- DurSevFromSim(data=dat[,1],marg.obj=dummy.marg.gpd.fit,dep.obj=ht.dep.fit,
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





