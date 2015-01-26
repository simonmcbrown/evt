# source("plot_cannington.R")

# here we take the output from gen_lag1_hw_model.R, plot the events and then add lines of equal probability

pathin <- '/project/extremes1/hadsx/RSave.data/'
strin0 <- 'cannington_sim_1e5_'

PRINTSAVE <- T
DOLOAD    <- F

nsim      <- 1e5
p         <- c(0.01*(5*1:19),0.99,0.999,0.9999,0.99999) # define some probabilities that we want quantiles for
ip        <- c(20,21,22)  # probabilities to plot
id        <- c(15,19,23)  # 1 to id days to plot for each ip

#if (TESTING) a4p2x3() else a4p1x2()
### load in stacks of simulated events
if(DOLOAD) {  # do load
sim.events.4 <- NULL # peak
sim.events.3 <- NULL # duratino
sim.events.1 <- NULL # severity
for (f in list.files(path=pathin,pattern=strin0, full.names=TRUE)) {
  cat('Loading ',f,cr)
  load(f)
  sim.events.4 <- c(sim.events.4,sim.events[[4]])
  sim.events.3 <- c(sim.events.3,sim.events[[3]])
  # this converts the severity from anomalies to absolutes
  sim.events.1 <- c(sim.events.1,sim.events[[1]]+(sim.events[[3]]*sim.events[[5]]))   # : anomaly*duration
}
  # this converts the severity from anomalies to absolutes
  obs.events[[1]] <- obs.events[[1]]+obs.events[[3]]*obs.events[[5]]

# structure of thie simulated events
#> str(sim.events)
#List of 5
# $ : num [1:100000] 22.42 8.09 14.52 151.88 31.36 ...    (1) The severity values of each simulated event: anomaly*duration
# $ : num [1:35] 0.0213 0.083 0.138 0.1541 0.139 ...      (2) The duration distribution
# $ : int [1:100000] 7 3 4 21 6 5 2 6 1 3 ...             (3) The duration values of each simulated event
# $ : num [1:100000] 33.6 33.7 36.5 45.5 35.9 ...         (4) The peak values used to simulate from
# $ : Named num 27.2                                      (5) The modelling threshold (and %ile) on original margins


#readline("Stop")

### convert to average severity per day
uday    <- sort(unique(sim.events.3))
for (d in uday) {
  ix                  <- which(sim.events.3 == d)
  sim.events.1[ix]    <- sim.events.1[ix]/d
}
udayo   <- sort(unique(obs.events[[3]]))
for (d in udayo) {
  ix                  <- which(obs.events[[3]] == d)
  obs.events[[1]][ix] <- obs.events[[1]][ix]/d
}

# Plot the underlying data along with associated contour values
#par(mfrow=c(1,1),mar=c(5,5,2,2))
xlim <- extend.range(c(0,obs.events[[3]],sim.events.3))
ylim <- extend.range(c(0,obs.events[[1]],sim.events.1))

} # end do load


### calculate quantiles
n0       <- length(sim.events.3)
n.day    <- NULL
p.n.day  <- NULL
n2.day   <- NULL
p2.n.day <- NULL
x.save   <- list()
xx.save   <- list()  # structure for the peak temperature during the heatwave
for (d in uday) {
  ix           <- which(sim.events.3 == d)
  x            <- sim.events.1[ix]
   x.save[[d]] <- x                           # Add line that saves the severity for each duration d 
  xx.save[[d]] <- sim.events.4[ix]            # Add line that saves the peak     for each duration d 
  n2.day[d]    <- length(ix)                  # number of events with heatwave length of =d days
  p2.n.day[d]  <- n2.day[d]/n0                # probability of having a heatwave of length = d days
}

# obs severity and peak temp for D>=d
xx.obs.gtd <- rep(0,max(udayo))
x.obs.gtd  <- rep(0,max(udayo))
for (d in 1:max(udayo)) {
  ixo           <- which(obs.events[[3]] >= d)
   x.obs.gtd[d] <- max(obs.events[[1]][ixo],na.rm=T)
  xx.obs.gtd[d] <- max(obs.events[[4]][ixo],na.rm=T)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Additions by Hugo to create more realistic check marks DATE: 22/10/14
p3.x <- q3.x <- q4.x <- array(NA,dim=c(25,length(p)))
 x.save.no.list <- unlist(x.save)
xx.save.no.list <- unlist(xx.save)
for (d in 1:25){   # Only do this for the first 25 duration values
  p3.x[d,] <- 1 -(1-p)/(1-cumsum(c(0,p2.n.day))[d])   # P(S<=s|D>=d) calculated as P(S<=s,D>=d)/P(D>=d)
                                                      # 1-p                                     is: P(Event>e)
                                                      # (1-cumsum(c(0,p2.n.day))[d]             is: P(D>=d)
                                                      # (1-p)/(1-cumsum(c(0,p2.n.day))[d])      is: P(S>=s|D>=d)
                                                      # 1 - (1-p)/(1-cumsum(c(0,p2.n.day))[d])  is: P(S<=s|D>=d)  # need S<=s for the quantile function below
  p3.x.temp <- p3.x[d,]
  p3.x.temp[p3.x.temp<0] <- NA   # Will get negative values that are just ignored
  # Estimate the quantile using all the data for which D>=d and the probabilities calculated above
  q3.x[d,] <- quantile(x.save.no.list[(cumsum(c(0,n2.day))[d]+1):n0],probs = p3.x.temp)
  q4.x[d,] <- quantile(xx.save.no.list[(cumsum(c(0,n2.day))[d]+1):n0],probs = p3.x.temp)
  #readline("Stop")
}

# do plotting
if (PRINTSAVE) {
  postscript('cannington_heatwaves_markov1.ps',horiz=F)
  up.2()
  }

xlim <- extend.range(c(obs.events[[3]],sim.events.3))
ylim <- extend.range(c(obs.events[[4]],sim.events.4)) # peak temps

plot(sim.events.3[1:nsim]+0.6*runif(length(sim.events.3[1:nsim]))-0.3,sim.events.1[1:nsim],
        xlab="Duration (days)",ylab="Mean daily severity & Peak temp (C)",main="Markov(1) Heatwaves: Cannington",xlim=xlim,ylim=ylim,pch=46)
    for(j in 1:length(ip)) points(uday[1:(id[j])],q3.x[1:(id[j]),ip[j]],col=j+1,pch=45,cex=1.5)  # severity
    for(j in 1:length(ip)) points(uday[1:(id[j])],q4.x[1:(id[j]),ip[j]],col=j+1,pch=4,  cex=1)  # peak
    points(obs.events[[3]],obs.events[[1]],cex=.7, col=5,pch=45)
    points(obs.events[[3]],obs.events[[4]],cex=.7, col=5,pch=3)
    points(1:max(udayo), x.obs.gtd,        cex=1.5,col=5,pch=45)
    points(1:max(udayo),xx.obs.gtd,        cex=1,  col=5,pch=4)
txt1 <- c('Relative probability',paste(round(1-p[rev(ip)],5),'sim'),'Obs','Severity','Peak')
legend(max(xlim),max(ylim),txt1,col=c(1,4,3,2,5,1,1),lty=c(0,0,0,0,0,0,0),pch=c(NA,1,1,1,1,45,4), yjust=1, xjust=1)

if (PRINTSAVE) dev.off()

# plots to understand what is going on
if(F) {
up.2by2()
il<-1:25  
plot(p2.n.day[il],                             xlab='Days',main='P(D=d), prob heatwave length=d days')
plot(1-cumsum(c(0,p2.n.day[il])),              xlab='Days',main='P(D>=d): 1-cumsum(c(0,p2.n.day))')
plot((1-p[20])/(1-cumsum(c(0,p2.n.day[il]))),  xlab='Days',main='P(S>=s|D>=d): (1-0.99)/(1-cumsum(c(0,p2.n.day)))',ylim=c(-.1,1.1))
abline(h=0)
plot(1-(1-p[20])/(1-cumsum(c(0,p2.n.day[il]))),xlab='Days',main='P(S<=s|D>=d): 1-(1-p[20])/(1-cumsum(c(0,p2.n.day)))',ylim=c(-.1,1.1))
abline(h=0)
#plot(,xlab='Days',main='')
#plot(,xlab='Days',main='')
#plot(,xlab='Days',main='')
}



