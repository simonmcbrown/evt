# source("hist_sim_hw_Tmax.R")

#tit0    <- "CM3 Max Temperature during Heatwaves: Central France"
#psout   <- 'hist_Tmax_cm3.ps'
#pathin  <- '/project/extremes1/hadsx/RSave.data/'
#strinH0 <- 'orleans_cm3_ant_temp_dat_sim_1e5_'
#strinC0 <- 'orleans_cm3_nat_temp_dat_sim_1e5_'
#leg0    <- c('ANT','NAT')

#tit0    <- "GEM1 Max Temperature during Heatwaves: Orleans"
#psout   <- 'hist_Tmax_orleans_gem1.ps'
#pathin  <- '/project/extremes1/hadsx/RSave.data/'
#strinH0 <- 'orleans_gem1_ant_temp_dat_sim_1e5_'
#strinC0 <- 'orleans_gem1_all_temp_dat_sim_1e5_'
#leg0    <- c('ANT','ALL')

#tit0    <- "GEM1 Max Temperature during Heatwaves: Toulouse"
#psout   <- 'hist_Tmax_toulouse_gem1.ps'
#pathin  <- '/project/extremes1/hadsx/RSave.data/'
#strinH0 <- 'toulouse_gem1_ant_temp_dat_sim_1e5_'
#strinC0 <- 'toulouse_gem1_all_temp_dat_sim_1e5_'
#leg0    <- c('ANT','ALL')

tit0    <- "GEM2 Max Temperature during Heatwaves: Orleans"
psout   <- 'hist_Tmax_orleans_gem2_ghg_nat.ps'
pathin  <- '/project/extremes1/hadsx/RSave.data/'
strinH0 <- 'orleans_gem2_ghg_temp_dat_sim_1e5_'
strinC0 <- 'orleans_gem2_nat_temp_dat_sim_1e5_'
leg0    <- c('GHG','NAT')



nfiles  <- 20

# structure of thie simulated events
#> str(sim.events)
#List of 5
# $ : num [1:100000] 22.42 8.09 14.52 151.88 31.36 ...    (1) The severity values of each simulated event
# $ : num [1:35] 0.0213 0.083 0.138 0.1541 0.139 ...      (2) The duration distribution
# $ : int [1:100000] 7 3 4 21 6 5 2 6 1 3 ...             (3) The duration values of each simulated event
# $ : num [1:100000] 33.6 33.7 36.5 45.5 35.9 ...         (4) The peak values used to simulate from
# $ : Named num 27.2                                      (5) The modelling threshold (and %ile) on original margins

if(T) {  # load simulated evetns
### LOAD COLD
    sim.eventsC.3 <- NULL
    sim.eventsC.4 <- NULL
    for (f in list.files(path=pathin,pattern=strinC0, full.names=TRUE)[1:nfiles]) {
      cat('Loading ',f,cr)
      load(f)
      sim.eventsC.3 <- c(sim.eventsC.3,sim.events[[3]])
      sim.eventsC.4 <- c(sim.eventsC.4,sim.events[[4]])
    }
    thC <- sim.events[[5]]

### LOAD HOT
    sim.eventsH.3 <- NULL
    sim.eventsH.4 <- NULL
    for (f in list.files(path=pathin,pattern=strinH0, full.names=TRUE)[1:nfiles]) {
      cat('Loading ',f,cr)
      load(f)
      sim.eventsH.3 <- c(sim.eventsH.3,sim.events[[3]])
      sim.eventsH.4 <- c(sim.eventsH.4,sim.events[[4]])
    }
    thH <- sim.events[[5]]

}
udayH          <- sort(unique(sim.eventsH.3))
udayC          <- sort(unique(sim.eventsC.3))

idH <- list()
for (d in udayH) idH[[d]] <- which(sim.eventsH.3 == d)
idC <- list()
for (d in udayC) idC[[d]] <- which(sim.eventsC.3 == d)

p  <- sort(1 - c(0.1,(1:10))/100)
qH <- qC <- list()
for (d in 1:25) {
    qH[[d]] <- quantile(sim.eventsH.4[idH[[d]]],p)
    qC[[d]] <- quantile(sim.eventsC.4[idC[[d]]],p)
}

if (F) {
plot(1/(1-p),qH[[25]],log='x')
points(1/(1-p),qC[[25]],col=2)

# sever d plots
d0 <- c(5,11,15,20)
par(mfrow=c(4,2))
par(mar=c(3,3,2,1))
par(mgp=c(2,1,0))

for (d in d0) {
# histogrammes
hH <- hist(sim.eventsH.4[idH[[d]]],100,plot=F)
x1 <- rep(hH$breaks,each=2)
x1 <- x1[2:(length(x1)-1)]
#y1 <- rep(hH$density,each=2)
y1 <- rep(hH$counts,each=2)
plot(x1,y1,type='l',main=paste('Duration =',d))

hC <- hist(sim.eventsC.4[idC[[d]]],100,plot=F)
x1 <- rep(hC$breaks,each=2)
x1 <- x1[2:(length(x1)-1)]
#y1 <- rep(hC$density,each=2)
y1 <- rep(hC$counts,each=2)
lines(x1,y1,col=2)

#ranked qq plots
lC <- length(sim.eventsC.4[idC[[d]]])
lH <- length(sim.eventsH.4[idH[[d]]])
if (lC>lH) n <- lH else n<-lC
 plot(sort(sim.eventsH.4[idH[[d]]],dec=T)[1:1000], sort(sim.eventsC.4[idC[[d]]],dec=T)[1:1000], xlab=leg0[1],ylab=leg0[2],xlim=range(sim.eventsH.4),ylim=range(sim.eventsC.4))
lines(sort(sim.eventsH.4[idH[[d]]],dec=T)[1000:n], sort(sim.eventsC.4[idC[[d]]],dec=T)[1000:n])
abline(0,1)
}

}

postscript(psout,horiz=F)
    #  all histos
    par(mfrow=c(5,4))
    par(mar=c(3,3,2,1))
    par(oma=c(0,0,1,0))
    par(mgp=c(2,1,0))
    for (d in 1:19) {
        # histogrammes
        #hH <- hist(sim.eventsH.4[idH[[d]]],100,plot=F)
        #hC <- hist(sim.eventsC.4[idC[[d]]],100,plot=F)
        hHC <- hist(c(sim.eventsH.4[idH[[d]]],sim.eventsC.4[idC[[d]]]),100,plot=F)
        hH  <- hist(sim.eventsH.4[idH[[d]]],breaks=hHC$breaks,plot=F)
        hC  <- hist(sim.eventsC.4[idC[[d]]],breaks=hHC$breaks,plot=F)

        x1 <- rep(hH$breaks,each=2)
        x1 <- x1[2:(length(x1)-1)]
        y1 <- rep(hH$density,each=2)
        plot(x1,y1,type='l',main=paste('Duration =',d),ylim=c(0,max(c(y1,hC$density))), xlab='Tmax', ylab='Density' )
        #y1 <- rep(hH$counts,each=2)
        #plot(x1,y1,type='l',main=paste('Duration =',d),ylim=c(0,max(c(y1,hC$counts))), xlab='Tmax', ylab='Count' )

        x1 <- rep(hC$breaks,each=2)
        x1 <- x1[2:(length(x1)-1)]
        y1 <- rep(hC$density,each=2)
        #y1 <- rep(hC$counts,each=2)
        lines(x1,y1,col=2)

        if (d==1) {
            #legend(max(x1),max(c(y1,hH$counts)),leg0,col=c(1,2), yjust=1, xjust=1, lty=1)
            legend(par()$xaxp[2],par()$yaxp[2],leg0,col=c(1,2), yjust=1, xjust=1, lty=1)
        }
    }
    mtext(tit0,line=-0.1,cex=0.9,outer=T)

    ### Hist of number of events by duration
    hdC<-hist(sim.eventsC.3,breaks=0:50,plot=F)
    hdH<-hist(sim.eventsH.3,breaks=0:50,plot=F)

    x1 <- rep(hdH$breaks,each=2)
    x1 <- x1[2:(length(x1)-1)]
    #y1 <- rep(hH$density,each=2)
    y1 <- rep(hdH$counts,each=2)
    plot(x1,y1,type='l',main=paste('Duration'),xlim=c(0,20), ylim=c(0,max(c(y1,hC$counts))), xlab='Duration', ylab='Count' )

    x1 <- rep(hdC$breaks,each=2)
    x1 <- x1[2:(length(x1)-1)]
    #y1 <- rep(hC$density,each=2)
    y1 <- rep(hdC$counts,each=2)
    lines(x1,y1,col=2)
#    legend(par()$xaxp[2],par()$yaxp[2],leg0,col=c(1,2), yjust=1, xjust=1, lty=1)

dev.off()
