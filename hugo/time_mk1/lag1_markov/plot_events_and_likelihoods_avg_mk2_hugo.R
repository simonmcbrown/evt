# source("plot_events_and_likelihoods_mk2_hugo.R")

# here we take the output from obs-sim-contour-RUN-multi.R, plot the events and then add lines of equal probability

#pathin <- '/home/hugo/Documents/temporal/Rcode/scripts/simon/'
pathin <- '/project/extremes1/hadsx/RSave.data/'
strin0 <- 'orleans_temp_dat_sim_1e5_'

TESTING   <- F
PRINTSAVE <- F

if (PRINTSAVE) {
  postscript('drought_likelihood.ps',horiz=F)
  up.2()
  }

#if (TESTING) a4p2x3() else a4p1x2()

if(F) {  # load simulated evetns
sim.events.3 <- NULL
sim.events.1 <- NULL
for (f in list.files(path=pathin,pattern=strin0, full.names=TRUE)[1:2]) {
  cat('Loading ',f,cr)
  load(f)
  sim.events.3 <- c(sim.events.3,sim.events[[3]])
  #sim.events.1 <- c(sim.events.1,sim.events[[1]])
  sim.events.1 <- c(sim.events.1,sim.events[[1]]+(sim.events[[3]]*sim.events[[5]]))   # : anomaly*duration
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## Load in one of Simon's simulated events
#load(file = '/home/hugo/Documents/temporal/Rcode/scripts/simon/orleans_temp_dat_sim_1e5_0001.RSave')
#sim.events.1 <- sim.events[[1]]
#sim.events.3 <- sim.events[[3]]
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#readline("Stop")

### convert to average severity per day
uday    <- sort(unique(sim.events.3))
for (d in uday) {
  ix         <- which(sim.events.3 == d)
  sim.events.1[ix] <- sim.events.1[ix]/d
  ix         <- which(obs.events[[3]] == d)
  obs.events[[1]][ix] <- (obs.events[[1]][ix]+obs.events[[5]]*obs.events[[3]][ix])/d  # bodged this without checking
}

# Plot the underlying data along with associated contour values
#par(mfrow=c(1,1),mar=c(5,5,2,2))
xlim <- extend.range(c(0,obs.events[[3]],sim.events.3))
ylim <- extend.range(c(0,obs.events[[1]],sim.events.1))

}

# Have made a slight adjustment to the plotting command since the
# 'extend.range' function did not work for me
plot(sim.events.3[1:1e5]+0.6*runif(length(sim.events.3[1:1e5]))-0.3,sim.events.1[1:1e5],xlab="Duration (days)",ylab="Mean daily severity (arbitrary units)",main="Heatwaves: Central France",xlim=c(0,40),ylim=c(0,11.5),pch=46)
#plot(sim.events.3[1:1e5]+0.6*runif(length(sim.events.3[1:1e5]))-0.3,sim.events.1[1:1e5],xlab="Duration (days)",ylab="Mean daily severity (arbitrary units)",main="Heatwaves: Central France",xlim=xlim,ylim=ylim,pch=46)
#id <- which(sim.events.3>10)
#points(sim.events.3[id]+0.6*runif(length(id))-0.3,sim.events.1[id],pch=46)
points(obs.events[[3]],obs.events[[1]],cex=1,col=1,pch=21,bg='red')

n0      <- length(sim.events.3)
n.day   <- NULL
p.n.day <- NULL
n2.day   <- NULL

p2.n.day <- NULL
p       <- c(0.01*(5*1:19),0.99,0.999,0.9999,0.99999)
q.x     <- array(NA,dim=c(max(uday),length(p)))
p.x     <- array(NA,dim=c(max(uday),length(p)))
p.plot  <- c(1e-5,1e-4,1e-3,1e-2)
l.pq.fn <- list()
pq.xn   <- array(NA,dim=c(max(uday),length(p.plot)))

q2.x     <- array(NA,dim=c(max(uday),length(p)))
p2.x     <- array(NA,dim=c(max(uday),length(p)))
l2.pq.fn <- list()
pq2.xn   <- array(NA,dim=c(max(uday),length(p.plot)))
x.save <- list()

for (d in uday) {
    ## mk2 this version just works in d==d space
  ix         <- which(sim.events.3 == d)
  x          <- sim.events.1[ix]
  x.save[[d]]   <- x                           # Add line that saves the data for each duration d 
  n2.day[d]     <- length(ix)                  # number of events with heatwave length of =d days
  p2.n.day[d]   <- n2.day[d]/n0                # probability of having a heatwave of length = d days
  q2.x[d,]      <- quantile(x,p)             # quantiles of average severity of heatwave for HW of length d days
  p2.x[d,]      <- (1-p)*p2.n.day[d]           # probability of these quantiles factoring in the probability of haveing a HW of length d days
  l2.pq.fn[[d]] <- smooth.spline(log(p2.x[d,]),q2.x[d,],spar=0.1) # fn to interpolate to a quantile for a common probability for each duration
  pq2.xn[d,]    <- predict(l2.pq.fn[[d]],log(p.plot))$y          # predicted quantiles for a given common probability for each duration
                                       
}
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Additions by Hugo to create more realistic check marks
# DATE: 22/10/14
p3.x <- q3.x <- array(NA,dim=c(25,length(p)))
x.save.no.list <- unlist(x.save)
for (d in 1:25){   # Only do this for the first 25 duration values
  p3.x[d,] <- 1 -(1-p)/(1-cumsum(c(0,p2.n.day))[d])   # P(S<=s|D>=d) calculated as P(S<=s,D>=d)/P(D>=d)
      # P(S<=s|D>=d) calculated as P(S<=s,D>=d)/P(D>=d)
      # 1-p                                     is: P(Event>e)
      # (1-cumsum(c(0,p2.n.day))[d]             is: P(D>=d)
      # (1-p)/(1-cumsum(c(0,p2.n.day))[d])      is: P(S>=s|D>=d)
      # 1 - (1-p)/(1-cumsum(c(0,p2.n.day))[d])  is: P(S<=s|D>=d)  # need S<=s for the quantile function below
  p3.x.temp <- p3.x[d,]
  p3.x.temp[p3.x.temp<0] <- NA   # Will get negative values that are just ignored
  # Estimate the quantile using all the data for which D>=d and the probabilities calculated above
  q3.x[d,] <- quantile(x.save.no.list[(cumsum(c(0,n2.day))[d]+1):n0],probs = p3.x.temp)
  readline("Stop")
}

#lines(uday[1:25],finite(pq2.xn[,1][1:25]),col=2,lwd=2,lty=3)
#lines(uday[1:21],finite(pq2.xn[,2][1:21]),col=3,lwd=2,lty=3)
#lines(uday[1:16],finite(pq2.xn[,3][1:16]),col=4,lwd=2,lty=3)
#lines(uday[1:11],finite(pq2.xn[,4][1:11]),col=5,lwd=2,lty=3)
#points(uday[1:25],pq2.xn[,1][1:25],col=2,pch='-',cex=2)
#points(uday[1:21],pq2.xn[,2][1:21],col=3,pch='-',cex=2)
#points(uday[1:16],pq2.xn[,3][1:16],col=4,pch='-',cex=2)
#points(uday[1:10],pq2.xn[,4][1:10],col=5,pch='-',cex=2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# My new plotting commands, combine these with the 
# original plot command above
points(uday[1:25],q3.x[1:25,23],col=2,pch='-',cex=2)
points(uday[1:23],q3.x[1:23,22],col=3,pch='-',cex=2)
points(uday[1:17],q3.x[1:17,21],col=4,pch='-',cex=2)
points(uday[1:12],q3.x[1:12,20],col=5,pch='-',cex=2)

# Test that I am getting the right values for the simulated sample
# Is 0.01 (or equivalent quantile) of the simulated sample falling
# above the check marks?

# This problem is set up for relative probability 1e-2
exc.vals <- numeric(12)
for (i in 1:12){
  exc.vals[i] <- sum(x.save[[i]]>q3.x[i,20])
}

# 0.0113 of data falls above the check marks where the check values
# can be defined
sum(n2.day[13:length(n2.day)],na.rm = TRUE)/n0
# But surely we have to include all values for which larger so 
# add on all other data with duration longer than 12 days
# Turns out that 0.035 of sample falls outside the check marks
(sum(n2.day[13:length(n2.day)],na.rm = TRUE) + sum(exc.vals))/n0
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

legend(23,11.2,c('Relative probability','1e-5','1e-4','1e-3','1e-2'),col=1:5,lty=c(0,3,3,3,3))

if(TESTING) {  
  plot(uday,p2.n.day[uday],main='Prob of heatwave of length = d days',xlab="Duration (days)" )
  plot(0,0,xlim=range(uday),ylim=c(0,11),type='n')
  for( d in uday ) points(rep(d,length(q2.x[d,])),q2.x[d,],pch=3) 
}

if (PRINTSAVE) dev.off()