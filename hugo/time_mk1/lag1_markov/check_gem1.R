# source("check_gem1.R")

# hack to make sense of ABW being hotter than ALL

seas_length <- 90

#st.ant   <- "~/extremes/tawn/hugo/heatwave/gem1/orleans/ant_1234_allyr_daily_tmax_1950_2005.csv"
#st.all   <- "~/extremes/tawn/hugo/heatwave/gem1/orleans/all_1234_allyr_daily_tmax_1950_2005.csv"
#psout    <- '~/extremes/tawn/hugo/time_mk1/check_gem1_orleans.ps'
#tit0     <- "GEM1 Daily Max Temperature: Orleans"

st.ant   <- "~/extremes/tawn/hugo/heatwave/gem1/toulouse/ant_1234_allyr_daily_tmax_1950_2005.csv"
st.all   <- "~/extremes/tawn/hugo/heatwave/gem1/toulouse/all_1234_allyr_daily_tmax_1950_2005.csv"
psout    <- '~/extremes/tawn/hugo/time_mk1/check_gem1_toulouse.ps'
tit0     <- "GEM1 Daily Max Temperature: Toulouse"

ant.data <- read.csv(st.ant,header=T)   # Read in data from text file
all.data <- read.csv(st.all,header=T)   # Read in data from text file

txl <- all.data$Tmax-273.15
txt <- ant.data$Tmax-273.15

idec <- array(0,dim=c(36,10))
for(d in 1:36) idec[d,] <- (d-1)*10+ 1:10
doyl <- all.data$Day + 30*(all.data$Month -1)
doyt <- ant.data$Day + 30*(ant.data$Month -1)

cll <- NULL
for(d in 1:36) cll[d] <- mean(txl[doyl == idec[d,]])
clt <- NULL
for(d in 1:36) clt[d] <- mean(txt[doyt == idec[d,]])

#plot(0,0,ylim=range(txl),xlim=c(161,250))
#for (y in unique(all.data$Year)) lines(rep(161:250,4),txl[which((all.data$Year==y)&(doyl>160)*(doyl<251))],col=y)
#readline("Continue?")

cll2 <- NULL
for(d in 1:360) cll2[d] <- mean(txl[doyl == d])
clt2 <- NULL
for(d in 1:360) clt2[d] <- mean(txt[doyt == d])

postscript(psout,horiz=T)
    up.2by2()
    par(oma=c(0,0,1,0))

    plot(cll2,main='Daily climatology',ylab='Tmax',xlab='Day of year')
    points(clt2,col=2,cex=.3)
    legend(0,25,c('GEM1 ALL','GEM1 ANT'),col=c(1,2),pch=1)

    plot(cll2-clt2,cex=.3,main='Daily climatology difference',ylab='Degrees C')
    a<- filter(cll2-clt2,rep(1,7)/7.0,circ=T)
    lines(a,col=3)
    abline(h=0)
    #readline("Continue?")

    y0 <- rep(unique(all.data$Year),4)
    x0 <- 180:230
    plot(x0,txl[x0],ylim=c(15,45),cex=.3, main='Summertime daily Tmax + medians',ylab='Degrees C',xlab='Day of year')
    for(y in 1:length(y0)) {
        points(x0,txl[x0+360*(y-1)],cex=.3)
        points(x0+.4,txt[x0+360*(y-1)],cex=.3,col=3)
    }

    for(j in 1:length(x0)) {
        ix <- seq(from=x0[j],length=length(y0),by=360)
        points(x0[j],median(txl[ix]),pch=3,col=2)
        points(x0[j]+.4,median(txt[ix]),pch=3,col=4)
    }

    # qq plot for top N%
    ijja <- NULL
    for(y in 1:length(y0)) ijja <- c(ijja,x0+360*(y-1))
    stxl <- sort(txl[ijja])
    stxt <- sort(txt[ijja])
    plot(stxl-stxt,typ='l',ylab='ALL - ANT', main='QQ Plot summertime daily Tmax differences')
    n <- length(stxt)
    points((n-20):n,stxl[(n-20):n]-stxt[(n-20):n],col=2,cex=.3)
    abline(h=0,col=4)

    mtext(tit0,line=-0.1,cex=0.9,outer=T)
dev.off()
