# source("plot_events_and_likelihoods_max_cm3_mk2_hugo.R")

### Heatwave metric: maximum temperature during heatwave
### HadCM3 data


# here we take the output from obs-sim-contour-RUN-multi.R, plot the events and then add lines of equal probability

tit0    <- "Max Temperature during Heatwaves: Central France"
ylab0   <- "Temperature"
pathin  <- '/project/extremes1/hadsx/RSave.data/'
strinA0 <- 'orleans_cm3_ant_temp_dat_sim_1e5_'
strinN0 <- 'orleans_cm3_nat_temp_dat_sim_1e5_'
nfiles  <- 13
ip <- c(20,21,22)
id <- c(17,25,25)


TESTING   <- F
PRINTSAVE <- F

if (PRINTSAVE) {
  postscript('drought_likelihood_max_cm3_ant_vs_nat.ps',horiz=F)
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
### LOAD NAT
    sim.eventsN.3 <- NULL
    sim.eventsN.4 <- NULL
    for (f in list.files(path=pathin,pattern=strinN0, full.names=TRUE)[1:nfiles]) {
      cat('Loading ',f,cr)
      load(f)
      sim.eventsN.3 <- c(sim.eventsN.3,sim.events[[3]])
      sim.eventsN.4 <- c(sim.eventsN.4,sim.events[[4]])
    }

### LOAD ANT
    sim.eventsA.3 <- NULL
    sim.eventsA.4 <- NULL
    for (f in list.files(path=pathin,pattern=strinA0, full.names=TRUE)[1:nfiles]) {
      cat('Loading ',f,cr)
      load(f)
      sim.eventsA.3 <- c(sim.eventsA.3,sim.events[[3]])
      sim.eventsA.4 <- c(sim.eventsA.4,sim.events[[4]])
    }

}
#sim.eventsN.4 <- sim.eventsN.4-273.15
#sim.eventsA.4 <- sim.eventsA.4-273.15
udayA          <- sort(unique(sim.eventsA.3))
udayN          <- sort(unique(sim.eventsN.3))

xlim <- extend.range(c(0,obs.events[[3]],sim.eventsA.3,sim.eventsN.3))
ylim <- extend.range(c(floor(sim.events[[5]]),obs.events[[4]],sim.eventsA.4,sim.eventsN.4))
#xlim <- c(0,30)
#ylim <- c(0,20)

# Have made a slight adjustment to the plotting command since the
# 'extend.range' function did not work for me
#plot(sim.eventsA.3[1:1e5]+0.6*runif(length(sim.eventsA.3[1:1e5]))-0.3,sim.eventsA.4[1:1e5],xlab="Duration (days)",ylab="Mean daily severity (arbitrary units)",main="Heatwaves: Central France",xlim=c(0,40),ylim=c(0,11.5),pch=46)
plot(sim.eventsA.3[1:1e5]+0.6*runif(length(sim.eventsA.3[1:1e5]))-0.3,sim.eventsA.4[1:1e5],xlab="Duration (days)",ylab=ylab0,main=tit0,xlim=xlim,ylim=ylim,pch=46)
#id <- which(sim.eventsA.3>10)
#points(sim.eventsA.3[id]+0.6*runif(length(id))-0.3,sim.eventsA.4[id],pch=46)
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
  x          <- sim.eventsA.4[ix]
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

### calculate Nat quantiles
n0       <- length(sim.eventsN.3)
n.day    <- NULL
p.n.day  <- NULL
n2.day   <- NULL
p2.n.day <- NULL
p        <- c(0.01*(5*1:19),0.99,0.999,0.9999,0.99999)
x.save   <- list()
for (d in udayN) {
  ix         <- which(sim.eventsN.3 == d)
  x          <- sim.eventsN.4[ix]
  x.save[[d]]   <- x                           # Add line that saves the data for each duration d 
  n2.day[d]     <- length(ix)                  # number of events with heatwave length of =d days
  p2.n.day[d]   <- n2.day[d]/n0                # probability of having a heatwave of length = d days
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Additions by Hugo to create more realistic check marks
# DATE: 22/10/14
p3.x <- q3N.x <- array(NA,dim=c(25,length(p)))
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
  q3N.x[d,] <- quantile(x.save.no.list[(cumsum(c(0,n2.day))[d]+1):n0],probs = p3.x.temp)
  #readline("Stop")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# My new plotting commands, combine these with the 
# original plot command above
#for(j in 1:length(ip)) points(udayN[1:(15+j*2)],q3N.x[1:(15+j*2),ip[j]],col=j+1,pch='~',cex=1.3)
for(j in 1:length(ip)) points(udayN[1:(id[j])],q3N.x[1:(id[j]),ip[j]],col=j+1,pch='~',cex=1.3)
#points(udayN[1:25],q3N.x[1:25,ip[4]],col=2,pch='~',cex=1.3)
#points(udayN[1:23],q3N.x[1:23,ip[3]],col=3,pch='~',cex=1.3)
#points(udayN[1:21],q3N.x[1:21,ip[2]],col=4,pch='~',cex=1.3)
#points(udayN[1:17],q3N.x[1:17,ip[1]],col=5,pch='~',cex=1.3)

txt1 <- c('Relative probability',round(1-p[rev(ip)],5),'CM3 Nat','CM3 Anthro')
legend(max(xlim),max(ylim),txt1,col=c(1:4,1,1),lty=c(0,3,3,3,0,0),pch=c(NA,NA,NA,NA,'~','-'), yjust=1, xjust=1)

if (PRINTSAVE) dev.off()