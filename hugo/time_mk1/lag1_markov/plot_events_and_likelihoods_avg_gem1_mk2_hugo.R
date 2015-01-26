# source("plot_events_and_likelihoods_avg_gem1_mk2_hugo.R")

### Heatwave metric: maximum temperature during heatwave


# here we take the output from obs-sim-contour-RUN-multi.R, plot the events and then add lines of equal probability

tit0    <- "Heatwave severity: Orleans"
ylab0   <- expression(paste("Mean daily severity, ",degree,"C"))
pathin  <- '/project/extremes1/hadsx/RSave.data'
strinA0 <- 'orleans_gem1_ant_temp_dat_sim_1e5_'
strinL0 <- 'orleans_gem1_all_temp_dat_sim_1e5_'
leg1 <- 'GEM1 All'
leg2 <- 'GEM1 Anthro'

nfiles  <- 13
ip <- c(20,21,22)
id <- c(17,25,25)


TESTING   <- F
PRINTSAVE <- T

if (PRINTSAVE) {
  postscript('drought_likelihood_avg_gem1_ant_vs_all.ps',horiz=F)
  up.2()
  }

# structure of thie simulated events
#> str(sim.events)
#List of 5
# $ : num [1:100000] 22.42 8.09 14.52 151.88 31.36 ...    (1) The severity values of each simulated event
# $ : num [1:35] 0.0213 0.083 0.138 0.1541 0.139 ...      (2) The duration distribution
# $ : int [1:100000] 7 3 4 21 6 5 2 6 1 3 ...             (3) The duration values of each simulated event
# $ : num [1:100000] 33.6 33.7 36.5 45.5 35.9 ...         (4) The peak values used to simulate from
# $ : Named num 27.2                                      (5) The modelling threshold (and %ile) on original margins

if(T) {  # load simulated evetns
### LOAD ALL
    sim.eventsL.3 <- NULL
    sim.eventsL.1 <- NULL
    for (f in list.files(path=pathin,pattern=strinL0, full.names=TRUE)[1:nfiles]) {
      cat('Loading ',f,cr)
      load(f)
      sim.eventsL.3 <- c(sim.eventsL.3,sim.events[[3]])
      #sim.eventsL.1 <- c(sim.eventsL.1,sim.events[[1]])
      sim.eventsL.1 <- c(sim.eventsL.1,sim.events[[1]]+(sim.events[[3]]*sim.events[[5]]))   # : anomaly*duration
    }
    ### convert to average severity per day
    udayL    <- sort(unique(sim.eventsL.3))
    for (d in udayL) {
      ix         <- which(sim.eventsL.3 == d)
      sim.eventsL.1[ix] <- sim.eventsL.1[ix]/d
    }

### LOAD ANT
    sim.eventsA.3 <- NULL
    sim.eventsA.1 <- NULL
    for (f in list.files(path=pathin,pattern=strinA0, full.names=TRUE)[1:nfiles]) {
      cat('Loading ',f,cr)
      load(f)
      sim.eventsA.3 <- c(sim.eventsA.3,sim.events[[3]])
      #sim.eventsA.1 <- c(sim.eventsA.1,sim.events[[1]])
      sim.eventsA.1 <- c(sim.eventsA.1,sim.events[[1]]+(sim.events[[3]]*sim.events[[5]]))   # : anomaly*duration
    }
    ### convert to average severity per day
    udayA    <- sort(unique(sim.eventsA.3))
    for (d in udayA) {
      ix         <- which(sim.eventsA.3 == d)
      sim.eventsA.1[ix] <- sim.eventsA.1[ix]/d
      ix         <- which(obs.events[[3]] == d)
      obs.events[[1]][ix] <- obs.events[[1]][ix]/d
    }

}

xlim <- extend.range(c(0,obs.events[[3]],sim.eventsA.3,sim.eventsL.3))
ylim <- extend.range(c(sim.eventsA.1,sim.eventsL.1))
#xlim <- c(0,30)
#ylim <- c(0,20)

# Have made a slight adjustment to the plotting command since the
# 'extend.range' function did not work for me
#plot(sim.eventsA.3[1:1e5]+0.6*runif(length(sim.eventsA.3[1:1e5]))-0.3,sim.eventsA.1[1:1e5],xlab="Duration (days)",ylab="Mean daily severity (arbitrary units)",main="Heatwaves: Central France",xlim=c(0,40),ylim=c(0,11.5),pch=46)
plot(sim.eventsA.3[1:1e5]+0.6*runif(length(sim.eventsA.3[1:1e5]))-0.3,sim.eventsA.1[1:1e5],xlab="Duration (days)",ylab=ylab0,main=tit0,xlim=xlim,ylim=ylim,pch=46)
#id <- which(sim.eventsA.3>10)
#points(sim.eventsA.3[id]+0.6*runif(length(id))-0.3,sim.eventsA.1[id],pch=46)
# obs stricutre a bit odd   # points(obs.events[[3]],obs.events[[4]],cex=1,col=1,pch=21,bg=5)

### calculate Ant quantiles
n0       <- length(sim.eventsA.3)
n.day    <- NULL
p.n.day  <- NULL
n2.day   <- NULL
p2.n.day <- NULL
p        <- c(0.01*(5*1:19),0.99,0.999,0.9999,0.99999)
x.save   <- list()
for (d in udayA) {
  ix         <- which(sim.eventsA.3 == d)
  x          <- sim.eventsA.1[ix]
  x.save[[d]]   <- x                           # Add line that saves the data for each duration d 
  n2.day[d]     <- length(ix)                  # number of events with heatwave length of =d days
  p2.n.day[d]   <- n2.day[d]/n0                # probability of having a heatwave of length = d days
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Additions by Hugo to create more realistic check marks
# DATE: 22/10/14
p3.x <- q3A.x <- array(NA,dim=c(25,length(p)))
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
  q3A.x[d,] <- quantile(x.save.no.list[(cumsum(c(0,n2.day))[d]+1):n0],probs = p3.x.temp)
  #readline("Stop")
}
#for(j in 1:length(ip)) points(udayA[1:(15+j*2)],q3A.x[1:(15+j*2),ip[j]],col=j+1,pch='-',cex=2)
for(j in 1:length(ip)) points(udayA[1:(id[j])],q3A.x[1:(id[j]),ip[j]],col=j+1,pch='-',cex=2)
#points(udayA[1:25],q3A.x[1:25,ip[4]],col=2,pch='-',cex=2)
#points(udayA[1:23],q3A.x[1:23,ip[3]],col=3,pch='-',cex=2)
#points(udayA[1:21],q3A.x[1:21,ip[2]],col=4,pch='-',cex=2)
#points(udayA[1:17],q3A.x[1:17,ip[1]],col=5,pch='-',cex=2)

#readline("Continuue?")

### calculate ALL quantiles
n0       <- length(sim.eventsL.3)
n.day    <- NULL
p.n.day  <- NULL
n2.day   <- NULL
p2.n.day <- NULL
p        <- c(0.01*(5*1:19),0.99,0.999,0.9999,0.99999)
x.save   <- list()
for (d in udayL) {
  ix         <- which(sim.eventsL.3 == d)
  x          <- sim.eventsL.1[ix]
  x.save[[d]]   <- x                           # Add line that saves the data for each duration d 
  n2.day[d]     <- length(ix)                  # number of events with heatwave length of =d days
  p2.n.day[d]   <- n2.day[d]/n0                # probability of having a heatwave of length = d days
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Additions by Hugo to create more realistic check marks
# DATE: 22/10/14
p3.x <- q3L.x <- array(NA,dim=c(25,length(p)))
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
  q3L.x[d,] <- quantile(x.save.no.list[(cumsum(c(0,n2.day))[d]+1):n0],probs = p3.x.temp)
  #readline("Stop")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# My new plotting commands, combine these with the 
# original plot command above
#for(j in 1:length(ip)) points(udayL[1:(15+j*2)],q3L.x[1:(15+j*2),ip[j]],col=j+1,pch='~',cex=1.3)
for(j in 1:length(ip)) points(udayL[1:(id[j])],q3L.x[1:(id[j]),ip[j]],col=j+1,pch='~',cex=1.3)
#points(udayL[1:25],q3L.x[1:25,ip[4]],col=2,pch='~',cex=1.3)
#points(udayL[1:23],q3L.x[1:23,ip[3]],col=3,pch='~',cex=1.3)
#points(udayL[1:21],q3L.x[1:21,ip[2]],col=4,pch='~',cex=1.3)
#points(udayL[1:17],q3L.x[1:17,ip[1]],col=5,pch='~',cex=1.3)

txt1 <- c('Relative probability',round(1-p[rev(ip)],5),leg1,leg2)
legend(max(xlim),max(ylim),txt1,col=c(1,4,3,2,1,1),lty=c(0,3,3,3,0,0),pch=c(NA,NA,NA,NA,'~','-'), yjust=1, xjust=1)

if (PRINTSAVE) dev.off()