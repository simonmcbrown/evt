# source("plot_events_and_likelihoods_max_gem2_mk2_hugo.R")

### Heatwave metric: max temp

# here we take the output from obs-sim-contour-RUN-multi.R, plot the events and then add lines of equal probability

tit0    <- "Max Temperature during Heatwaves: Orleans"
ylab0   <- "Teperature"
pathin  <- '/project/extremes1/hadsx/RSave.data'
strinH0 <- 'orleans_gem2_ghg_temp_dat_sim_1e5_'
strinC0 <- 'orleans_gem2_nat_temp_dat_sim_1e5_'
leg1 <- 'Gem2 GHG'
leg2 <- 'Gem2 NAT'

nfiles  <- 13
ip <- c(20,21,22)
id <- c(17,25,25)


TESTING   <- F
PRINTSAVE <- F

if (PRINTSAVE) {
  postscript('drought_likelihood_max_gem2_ghg_vs_nat.ps',horiz=F)
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
### LOAD COOL
    sim.eventsC.3 <- NULL
    sim.eventsC.4 <- NULL
    for (f in list.files(path=pathin,pattern=strinC0, full.names=TRUE)[1:nfiles]) {
      cat('Loading ',f,cr)
      load(f)
      sim.eventsC.3 <- c(sim.eventsC.3,sim.events[[3]])
      sim.eventsC.4 <- c(sim.eventsC.4,sim.events[[4]])
    }

### LOAD HOT
    sim.eventsH.3 <- NULL
    sim.eventsH.4 <- NULL
    for (f in list.files(path=pathin,pattern=strinH0, full.names=TRUE)[1:nfiles]) {
      cat('Loading ',f,cr)
      load(f)
      sim.eventsH.3 <- c(sim.eventsH.3,sim.events[[3]])
      sim.eventsH.4 <- c(sim.eventsH.4,sim.events[[4]])
    }

}

xlim <- extend.range(c(0,obs.events[[3]],sim.eventsH.3,sim.eventsC.3))
ylim <- extend.range(c(floor(sim.events[[5]]),obs.events[[4]],sim.eventsH.4,sim.eventsC.4))
#xlim <- c(0,30)
#ylim <- c(0,20)
udayH          <- sort(unique(sim.eventsH.3))
udayC          <- sort(unique(sim.eventsC.3))

# Have made a slight adjustment to the plotting command since the
# 'extend.range' function did not work for me
#plot(sim.eventsH.3[1:1e5]+0.6*runif(length(sim.eventsH.3[1:1e5]))-0.3,sim.eventsH.4[1:1e5],xlab="Duration (days)",ylab="Mean daily severity (arbitrary units)",main="Heatwaves: Central France",xlim=c(0,40),ylim=c(0,11.5),pch=46)
plot(sim.eventsH.3[1:1e5]+0.6*runif(length(sim.eventsH.3[1:1e5]))-0.3,sim.eventsH.4[1:1e5],xlab="Duration (days)",ylab=ylab0,main=tit0,xlim=xlim,ylim=ylim,pch=46)
#id <- which(sim.eventsH.3>10)
#points(sim.eventsH.3[id]+0.6*runif(length(id))-0.3,sim.eventsH.4[id],pch=46)
# obs stricutre a bit odd   # points(obs.events[[3]],obs.events[[4]],cex=1,col=1,pch=21,bg=5)

p        <- c(0.01*(5*1:19),0.99,0.999,0.9999,0.99999)

### calculate hot quantiles
Hn0       <- length(sim.eventsH.3)
Hn.day    <- NULL
Hp.n.day  <- NULL
Hn2.day   <- NULL
Hp2.n.day <- NULL
Hx.save   <- list()
for (d in udayH) {
  ix         <- which(sim.eventsH.3 == d)
  x          <- sim.eventsH.4[ix]
  Hx.save[[d]]   <- x                           # Add line that saves the data for each duration d 
  Hn2.day[d]     <- length(ix)                  # number of events with heatwave length of =d days
  Hp2.n.day[d]   <- Hn2.day[d]/Hn0                # probability of having a heatwave of length = d days
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Additions by Hugo to create more realistic check marks
# DATE: 22/10/14
Hp3.x <- q3H.x <- array(NA,dim=c(25,length(p)))
Hx.save.no.list <- unlist(Hx.save)
for (d in 1:25){   # Only do this for the first 25 duration values
  Hp3.x[d,] <- 1 -(1-p)/(1-cumsum(c(0,Hp2.n.day))[d])   # P(S<=s|D>=d) calculated as P(S<=s,D>=d)/P(D>=d)
      # P(S<=s|D>=d) calculated as P(S<=s,D>=d)/P(D>=d)
      # 1-p                                     is: P(Event>e)
      # (1-cumsum(c(0,p2.n.day))[d]             is: P(D>=d)
      # (1-p)/(1-cumsum(c(0,p2.n.day))[d])      is: P(S>=s|D>=d)
      # 1 - (1-p)/(1-cumsum(c(0,p2.n.day))[d])  is: P(S<=s|D>=d)  # need S<=s for the quantile function below
  Hp3.x.temp <- Hp3.x[d,]
  Hp3.x.temp[Hp3.x.temp<0] <- NA   # Will get negative values that are just ignored
  # Estimate the quantile using all the data for which D>=d and the probabilities calculated above
  q3H.x[d,] <- quantile(Hx.save.no.list[(cumsum(c(0,Hn2.day))[d]+1):Hn0],probs = Hp3.x.temp)
  #readline("Stop")
}
#for(j in 1:length(ip)) points(udayH[1:(15+j*2)],q3H.x[1:(15+j*2),ip[j]],col=j+1,pch='-',cex=2)
for(j in 1:length(ip)) points(udayH[1:(id[j])],q3H.x[1:(id[j]),ip[j]],col=j+1,pch='-',cex=2)   # hot
#points(udayH[1:25],q3H.x[1:25,ip[4]],col=2,pch='-',cex=2)
#points(udayH[1:23],q3H.x[1:23,ip[3]],col=3,pch='-',cex=2)
#points(udayH[1:21],q3H.x[1:21,ip[2]],col=4,pch='-',cex=2)
#points(udayH[1:17],q3H.x[1:17,ip[1]],col=5,pch='-',cex=2)

#readline("Continuue?")

### calculate cold quantiles
Cn0       <- length(sim.eventsC.3)
Cn.day    <- NULL
Cp.n.day  <- NULL
Cn2.day   <- NULL
Cp2.n.day <- NULL
Cx.save   <- list()
for (d in udayC) {
  ix         <- which(sim.eventsC.3 == d)
  x          <- sim.eventsC.4[ix]
  Cx.save[[d]]   <- x                           # Add line that saves the data for each duration d 
  Cn2.day[d]     <- length(ix)                  # number of events with heatwave length of =d days
  Cp2.n.day[d]   <- Cn2.day[d]/Cn0                # probability of having a heatwave of length = d days
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Additions by Hugo to create more realistic check marks
# DATE: 22/10/14
Cp3.x <- q3C.x <- array(NA,dim=c(25,length(p)))
Cx.save.no.list <- unlist(Cx.save)
for (d in 1:25){   # Only do this for the first 25 duration values
  Cp3.x[d,] <- 1 -(1-p)/(1-cumsum(c(0,Cp2.n.day))[d])   # P(S<=s|D>=d) calculated as P(S<=s,D>=d)/P(D>=d)
      # P(S<=s|D>=d) calculated as P(S<=s,D>=d)/P(D>=d)
      # 1-p                                     is: P(Event>e)
      # (1-cumsum(c(0,p2.n.day))[d]             is: P(D>=d)
      # (1-p)/(1-cumsum(c(0,p2.n.day))[d])      is: P(S>=s|D>=d)
      # 1 - (1-p)/(1-cumsum(c(0,p2.n.day))[d])  is: P(S<=s|D>=d)  # need S<=s for the quantile function below
  Cp3.x.temp <- Cp3.x[d,]
  Cp3.x.temp[Cp3.x.temp<0] <- NA   # Will get negative values that are just ignored
  # Estimate the quantile using all the data for which D>=d and the probabilities calculated above
  q3C.x[d,] <- quantile(Cx.save.no.list[(cumsum(c(0,Cn2.day))[d]+1):Cn0],probs = Cp3.x.temp)
  #readline("Stop")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# My new plotting commands, combine these with the 
# original plot command above
#for(j in 1:length(ip)) points(udayC[1:(15+j*2)],q3C.x[1:(15+j*2),ip[j]],col=j+1,pch='~',cex=1.3)
for(j in 1:length(ip)) points(udayC[1:(id[j])],q3C.x[1:(id[j]),ip[j]],col=j+1,pch='~',cex=1.3)  # cool/nat
#points(udayC[1:25],q3C.x[1:25,ip[4]],col=2,pch='~',cex=1.3)
#points(udayC[1:23],q3C.x[1:23,ip[3]],col=3,pch='~',cex=1.3)
#points(udayC[1:21],q3C.x[1:21,ip[2]],col=4,pch='~',cex=1.3)
#points(udayC[1:17],q3C.x[1:17,ip[1]],col=5,pch='~',cex=1.3)

txt1 <- c('Relative probability',round(1-p[rev(ip)],5),leg1,leg2)
legend(max(xlim),max(ylim),txt1,col=c(1,4,3,2,1,1),lty=c(0,3,3,3,0,0),pch=c(NA,NA,NA,NA,'-','~'), yjust=1, xjust=1)

if (PRINTSAVE) dev.off()