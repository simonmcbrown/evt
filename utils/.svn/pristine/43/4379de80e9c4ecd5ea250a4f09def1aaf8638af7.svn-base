# source("crash_glibc_force.R")

{
library(date)
source("/net/home/h03/hadsx/extremes/R/libs/pp_io.R")
source("/net/home/h03/hadsx/extremes/R/libs/simon_util.R")
source("/net/home/h03/hadsx/extremes/R/libs/simon_qump.R")
source("/net/home/h03/hadsx/extremes/R/libs/simon_plots.R")
source("/net/home/h03/hadsx/extremes/R/libs/simon_gev_reference.R")
source("/net/home/h03/hadsx/extremes/R/libs/gof.extremes.R")
library(ismev)

dir.in0  <- '/project/extremes1/hadsx/obs_data/prcp_1d_uk_25km/prediction_rqump/sss/pp'

dir.out0  <- sub('sss/pp','sss/plots',dir.in0)
patif0    <- 'rl_obs_50y_tg4_ifMMSS.gev.eee.dispersion.region03.pp' 
ir        <- 1
do.region  <- 3
psout0    <- 'histo_obs_50y_tg4_MMSS_ifMMSS.gev.dispersion.region03.ps'
rp        <- 50 	# return level in years
do.seas	  <- 1

st_mask0 <- '/project/extremes1/hadsx/obs_data/prcp_1d_uk_25km/prediction_rqump/sss/pp/pool_masks2_gev.pp'

READDATA     = T
imodel       = F
pred_gt <- 4.0	# value of global temperature covariate

if (ir == 1) irr <- c(1,2) else irr <- c(3,4)
st_regions <- c('<200m','>200m','High dispersion','Low dispersion')[irr]

ifieldsA <- c(1:3)
ifieldsB <- c(7:9)

# select regions
#ykeep   <- 28:52
#ynokeep <- 1:27
ykeep   <- 1:52
ynokeep <- 0

ylim1     <- c(-4,4)
nbins     <- 100


#do.expts <- 1:17
do.expts <- c(1,2,4,7,8,9,10,11,12,14,16)
nexpts   <- length(do.expts)
expts    <- c("afgcx", "afixa", "afixb", "afixc", "afixd", "afixf", "afixh", "afixi", "afixj", "afixk", "afixl", "afixm", "afixn", "afixo", "afixp", "afixq", "afixr")
exptsb   <- c("afgcx", "afixa", "(afixb)", "afixc", "(afixd)", "(afixf)", "afixh", "afixi", "afixj", "afixk", "afixl", "afixm", "(afixn)", "afixo", "(afixp)", "afixq", "(afixr)")

seas.all      <- c(12,1:11)
dim(seas.all) <- c(3,4)
#do.seas	      <- c(1,2,3,4)  # 1 == djf, 2 == mam etc.
#do.seas	      <- c(1,3) 

st_seas <- c( "DJF","MAM","JJA","SON","ANN")


######################################################
# loop through seasons 
for (iseas in do.seas) { 

	######################################################
	# read data 
	if(READDATA) {
		dir_in   <- sub('sss',st_seas[iseas],dir.in0)
		dir_out  <- sub('sss',st_seas[iseas],dir.out0)
		patif1   <- sub('sss',st_seas[iseas],patif0)

		#l.pp <- rep(list(vector(mode='list',length=11)),6)
		l.ppA <- vector(mode='list',length=nexpts)
		l.ppB <- vector(mode='list',length=nexpts)

		for (e in 1:nexpts) {
			stif               <- list.files(path=dir_in, pattern=sub('eee',expts[do.expts[e]],patif1),   full.names=TRUE)
			l.ppA[[e]]         <- read.pp(stif,id=ifieldsA)
			i                  <- which(l.ppA[[e]]$data == l.ppA[[e]]$bmdi[1])
			l.ppA[[e]]$data[i] <- NA
			l.ppB[[e]]         <- read.pp(stif,id=ifieldsB)
			i                  <- which(l.ppB[[e]]$data == l.ppB[[e]]$bmdi[1])
			l.ppB[[e]]$data[i] <- NA
		}
		
	}

	pp_mask = read.pp(sub('sss',st_seas[iseas],st_mask0))
	ir.1 <- which(pp_mask$data[,,1] == 1)
	ir.2 <- which(pp_mask$data[,,2] == 1)
	ir.3 <- which(pp_mask$data[,,3] == 1)
	ir.4 <- which(pp_mask$data[,,4] == 1)
	iregions <- list(ir.1,ir.2,ir.3,ir.4)

#readline("Stop")
	
	# plot histos
	psout <- paste(dir_out,psout0,sep='/')
	postscript(file=psout,horiz=T)
	up.2()
	par(mfrow=c(1,2))

		######################################################
		# calculate histograms
		

		d1A            <- sapply(l.ppA,"[[","data")
		dim(d1A)        <- c(39,52,length(ifieldsA),nexpts)
		d1A[,ynokeep,,] <- NA
		d1B            <- sapply(l.ppB,"[[","data")
		dim(d1B)        <- c(39,52,length(ifieldsB),nexpts)
		d1B[,ynokeep,,] <- NA
		
		min1    <-   floor(range(c(d1A[,,1:2,],d1B[,,1:2,]),na.rm=T)[1])
		max1    <- ceiling(range(c(d1A[,,1:2,],d1B[,,1:2,]),na.rm=T)[2])
		breaks1 <- seq(from=min1,to=max1,by=(max1-min1)/nbins)

		#min1    <-   floor(100*range(dall,na.rm=T)[1])/100
		#max1    <- ceiling(100*range(dall,na.rm=T)[2])/100
		min1    <-   floor(10*range(c(d1A[,,3,],d1B[,,3,]),na.rm=T)[1])/10
		if(min1 >= 1.0) min1<-0.9
		max1    <- ceiling(10*range(c(d1A[,,3,],d1B[,,3,]),na.rm=T)[2])/10
		#breaks2 <- seq(from=min1,to=max1,by=0.002)
		breaks2 <- seq(from=min1,to=max1,by=(max1-min1)/(nbins*2))

		for (r in do.region) {

		hist_1pA <- NULL
		hist_1fA <- NULL
		hist_sfA <- NULL
		hist_1pB <- NULL
		hist_1fB <- NULL
		hist_sfB <- NULL
		for (e in 1:nexpts) {
		#cat('Doing ',e,cr)
			data          <- d1A[,,1,e]
			hist_1pA[[e]] <- hist(data[unlist(iregions[r])],breaks=breaks1,plot=F)

			data          <- d1A[,,2,e]
			hist_1fA[[e]] <- hist(data[unlist(iregions[r])],breaks=breaks1,plot=F)

			data          <- d1A[,,3,e]
			hist_sfA[[e]] <- hist(data[unlist(iregions[r])],breaks=breaks2,plot=F)
			hist_sfA[[e]]$counts[1] <- 0

			data          <- d1B[,,1,e]
			hist_1pB[[e]] <- hist(data[unlist(iregions[r])],breaks=breaks1,plot=F)

			data          <- d1B[,,2,e]
			hist_1fB[[e]] <- hist(data[unlist(iregions[r])],breaks=breaks1,plot=F)

			data          <- d1B[,,3,e]
			hist_sfB[[e]] <- hist(data[unlist(iregions[r])],breaks=breaks2,plot=F)
			hist_sfB[[e]]$counts[1] <- 0

			#hist_sf[[e]] <- hist(data,breaks=breaks2,plot=T)
			#readline('continue?')
		}
		#readline('continue?')
		mxmn <- c(0.4,1.6)
		ymxmn <- c(1,100)
		k1 <- kernel("modified.daniell", 3)
		ymax <- NULL
		for (e in 1:nexpts) ymax <- max(ymax,kernapply(hist_sfA[[e]]$counts,k1))
		ymax=50
		plot(hist_sfA[[1]]$mids,kernapply(hist_sfA[[1]]$counts,k1,circ=T),type='n',
			ylim=c(1,ymax), xlim=mxmn, main='MMS',ylab='Frequency',xlab='Scale Factor',log='y')
			for (e in 1:nexpts) {
				lines(hist_sfA[[e]]$mids,kernapply(hist_sfA[[e]]$counts,k1,circ=T),type='l',col=e)
				#readline(paste(e,' continue?'))
			}
		#abline(v=1.0)

		
		for (e in 1:nexpts) ymax <- max(ymax,kernapply(hist_sfB[[e]]$counts,k1))
		ymax=50
		plot(hist_sfB[[1]]$mids,kernapply(hist_sfB[[1]]$counts,k1,circ=T),type='n',
			ylim=c(1,ymax), xlim=mxmn, main='MMSS',ylab='Frequency',xlab='Scale Factor',log='y')
			for (e in 1:nexpts) {
				lines(hist_sfB[[e]]$mids,kernapply(hist_sfB[[e]]$counts,k1,circ=T),type='l',col=e)
				#readline(paste(e,' continue?'))
			}
		#abline(v=1.0)

		mtext(paste('Region 3:',rp,'year return ,GEV,',st_seas[iseas]),line=-1.5,cex=1.2,outer=T,side=3,adj=NA)
		} # r

	dev.off()

} # end seas loop


}
