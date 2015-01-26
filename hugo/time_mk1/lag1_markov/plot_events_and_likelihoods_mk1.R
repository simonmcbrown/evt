# source("plot_events_and_likelihoods.R")

# here we take the output from obs-sim-contour-RUN-multi.R, plot the events and then add lines of equal probability

pathin <- '/project/extremes1/hadsx/RSave.data/'
strin0 <- 'orleans_temp_dat_sim_1e5_'

if(T) {
sim.events.3 <- NULL
sim.events.1 <- NULL
for (f in list.files(path=pathin,pattern=strin0, full.names=TRUE)[1:50]) {
  cat('Loading ',f,cr)
  load(f)
  sim.events.3 <- c(sim.events.3,sim.events[[3]])
  ERROR SEE MK2_HUGO sim.events.1 <- c(sim.events.1,sim.events[[1]])
}

#readline("Stop")

### convert to average severity per day
uday    <- sort(unique(sim.events.3))
for (d in uday) {
  ix         <- which(sim.events.3 == d)
  sim.events.1[ix] <- sim.events.1[ix]/d
  ix         <- which(obs.events[[3]] == d)
  obs.events[[1]][ix] <- obs.events[[1]][ix]/d
}

# Plot the underlying data along with associated contour values
#par(mfrow=c(1,1),mar=c(5,5,2,2))
xlim <- extend.range(c(0,obs.events[[3]],sim.events.3))
ylim <- extend.range(c(0,obs.events[[1]],sim.events.1))

plot(sim.events.3[1:1e5]+0.6*runif(length(sim.events.3[1:1e5]))-0.3,sim.events.1[1:1e5],xlab="Duration (days)",ylab="Mean daily severity (arbitrary units)",main="Heatwaves: Central France",xlim=xlim,ylim=ylim,pch=46)
#id <- which(sim.events.3>10)
#points(sim.events.3[id]+0.6*runif(length(id))-0.3,sim.events.1[id],pch=46)
points(obs.events[[3]],obs.events[[1]],cex=1,col=1,pch=21,bg='red')
}

n0      <- length(sim.events.3)
n.day   <- NULL
p.n.day <- NULL
n2.day   <- NULL

p2.n.day <- NULL
p       <- c(0.01*(5*1:19),0.99,0.999)
q.x     <- array(NA,dim=c(max(uday),length(p)))
p.x     <- array(NA,dim=c(max(uday),length(p)))
p.plot  <- c(1e-5,1e-4,1e-3,1e-2)
l.pq.fn <- list()
pq.xn   <- array(NA,dim=c(max(uday),length(p.plot)))

q2.x     <- array(NA,dim=c(max(uday),length(p)))
p2.x     <- array(NA,dim=c(max(uday),length(p)))
l2.pq.fn <- list()
pq2.xn   <- array(NA,dim=c(max(uday),length(p.plot)))

for (d in uday) {
  ## mk1 - dont remember the rational for mixing d>=3 and d==3
  #ix         <- which(sim.events.3 >= d)  # select events with duration >= d
  #n.day[d]   <- length(ix)                # number of events with heatwave length of >=d days
  #p.n.day[d] <- n.day[d]/n0               # probability of having a heatwave of length >= d days
  #ix         <- which(sim.events.3 == d)
  #x          <- sim.events.1[ix]
  #q.x[d,]    <- quantile(x/d,p)             # quantiles of average severity of heatwave for HW of length d days
  #p.x[d,]    <- (1-p)*p.n.day[d]          # probability of these quantiles factoring in the probability of haveing a HW of length d days
  #l.pq.fn[[d]] <- smooth.spline(p.x[d,],q.x[d,],spar=0.1) # fn to interpolate to a quantile for a common probability for each duration
  #pq.xn[d,]    <- d*predict(l.pq.fn[[d]],p.plot)$y          # predicted quantiles for a given common probability for each duration

  ## mk2 this version just works in d==d space
  ix         <- which(sim.events.3 == d)
  x          <- sim.events.1[ix]
  n2.day[d]     <- length(ix)                  # number of events with heatwave length of =d days
  p2.n.day[d]   <- n2.day[d]/n0                # probability of having a heatwave of length = d days
  q2.x[d,]      <- quantile(x/d,p)             # quantiles of average severity of heatwave for HW of length d days
  p2.x[d,]      <- (1-p)*p2.n.day[d]           # probability of these quantiles factoring in the probability of haveing a HW of length d days
  l2.pq.fn[[d]] <- smooth.spline(p2.x[d,],q2.x[d,],spar=0.1) # fn to interpolate to a quantile for a common probability for each duration
  pq2.xn[d,]    <- d*predict(l2.pq.fn[[d]],p.plot)$y          # predicted quantiles for a given common probability for each duration
               ### d* DONT GET THIS.  WHY MULTIPLY BY d?
}
  
#lines(uday,finite(pq.xn[,1]),col=1,lwd=2,lty=2)
#lines(uday,finite(pq.xn[,2]),col=2,lwd=2,lty=2)
#lines(uday,finite(pq.xn[,3]),col=3,lwd=2,lty=2)

lines(uday[1:25],finite(pq2.xn[,1][1:25]),col=2,lwd=2,lty=3)
lines(uday[1:21],finite(pq2.xn[,2][1:21]),col=3,lwd=2,lty=3)
lines(uday[1:16],finite(pq2.xn[,3][1:16]),col=4,lwd=2,lty=3)
lines(uday[1:11],finite(pq2.xn[,4][1:11]),col=5,lwd=2,lty=3)

legend(23,11.2,c('Relative probability','1e-5','1e-4','1e-3','1e-2'),col=1:5,lty=c(0,3,3,3,3))
  




