# file to collect useful qump specific R code

# source("/home/h03/hadsx/extremes/R/libs/simon_qump.R")

# get_qump17_names
# get_1row_trans17
# get_1row_slab17
# list_3d_2_array
# list_2d_2_array
# get_trans17_global_meam_responses
# get_slab_global_meam_responses
# 
# 
# 

#######################################################
#"name of function"<-
#function(list of parameter)
#{
#
#}
#######################################################
#######################################################
"get_qump17_names"<-
function(
	expt.file='/net/home/h03/hadsx/extremes/R/ukcip/general/rcm/expt_list_trans_1xco_2xco_rcm.txt'
)
{
# transient are expt.all[,1]
# slabx1 are expt.all[,2]
# slabx2 are expt.all[,3]
cat('Rubarb',cr)
c.types   <- c(rep("character",4))
expt.all  <- read.table(expt.file,skip=1,colClasses=c.types,col.names=c('GCM','CO2x1','CO2x2','RCM'),fill=TRUE)
expt.all
}
#######################################################
#######################################################
"get_1row_trans17"<-
function(
	ycoord,
	expt.list,
	data.dir = '/project/qump/hadcm3/xxx/daily',
	pat = '03236.08192',
	years = 1990:2099,
	idx = NA
	)
{

#sjb#library(date)

#source("/home/h03/hadsx/extremes/R/libs/simon_qump.R")

nexpt  <- length(expt.list)
l.t    <- vector(mode='list',length=nexpt)
date.t <- vector(mode='list',length=nexpt)

for (e in 1:nexpt) {
		path.to.search <- sub("xxx",expt.list[e],data.dir)
		st.all         <- dir(path=path.to.search, pattern=pat,full.names=TRUE)
		i.e            <- grep(expt.list[e],st.all)
		st.all         <- st.all[i.e]

		iy <- real(length(years))
		for (i in 1:length(iy)) iy[i] <-grep(as.character(years)[i],st.all)
		st.all         <- st.all[iy]

		nf             <- length(st.all)
		# here we read a row and then select the point later
		for (f in 1:nf) {
		
			pp1   <- read.row.pp(st.all[f],irow=ycoord,idxfields=idx)
			lbnpt <- pp1$lbnpt[1]
			lbrow <- pp1$lbrow[1]
			npy   <- pp1$nfields[1]
			
			if (f == 1) {
				d0.t <- array(0.0,dim=c(lbnpt,npy*nf))
				d0.t[1:lbnpt,1:npy] <- pp1$data
				yr.t <- pp1$lbyr
				mn.t <- pp1$lbmon
				dy.t <- pp1$lbdat
			} else {
				a                 <- (f-1)*npy+1
				b                 <- (f)*npy
				d0.t[1:lbnpt,a:b] <- pp1$data
				yr.t <- c(yr.t,pp1$lbyr)
				mn.t <- c(mn.t,pp1$lbmon)
				dy.t <- c(dy.t,pp1$lbdat)
			}
		}

		jdate.t <- mdy.date(mn.t, dy.t, yr.t)
		# deal with feb29 & feb30
		f29 <- which(mn.t==2 & dy.t==29)
		f29m1<-f29-1    
		jdate.t[f29]<-jdate.t[f29m1]+1

		f30 <- which(mn.t==2 & dy.t==30)
		f30m2<-f30-2    
		jdate.t[f30]<-jdate.t[f30m2]+2

		l.t[[e]]    <- d0.t
		date.t[[e]] <- jdate.t
}
	
	l1 <- list(data=l.t,dates=date.t,exptid=expt.list)
	l1
}

#######################################################
#######################################################
"get_1row_slab17"<-
function(
	ycoord,
	expt.list,
	data.dir = NA,
	months = 1:12,
	idx = NA
	)
{

#sjb#library(date)


nexpt  <- length(expt.list)
l.s    <- vector(mode='list',length=nexpt)
date.s <- vector(mode='list',length=nexpt)

for (e in 1:nexpt) {
		st.all         <- dir(path=data.dir, pattern=expt.list[e], full.names=TRUE)

		iy <- real(length(months))
		for (i in 1:length(iy)) iy[i] <-grep(as.character(months)[i],basename(st.all))[1]
		st.all  <- st.all[iy]

		nf             <- length(st.all)
		# here we read a row and then select the point later
		for (f in 1:nf) {
		
			pp1   <- read.row.pp(st.all[f],irow=ycoord,idxfields=idx)
			lbnpt <- pp1$lbnpt[1]
			lbrow <- pp1$lbrow[1]
			npy   <- pp1$nfields[1]
			
			if (f == 1) {
				d0.s <- array(0.0,dim=c(lbnpt,npy*nf))
				d0.s[1:lbnpt,1:npy] <- pp1$data
				yr.s <- pp1$lbyr
				mn.s <- pp1$lbmon
				dy.s <- pp1$lbdat
			} else {
				a                 <- (f-1)*npy+1
				b                 <- (f)*npy
				d0.s[1:lbnpt,a:b] <- pp1$data
				yr.s <- c(yr.s,pp1$lbyr)
				mn.s <- c(mn.s,pp1$lbmon)
				dy.s <- c(dy.s,pp1$lbdat)
			}
		}

		jdate.s <- mdy.date(mn.s, dy.s, yr.s)
		# deal with feb29 & feb30
		f29 <- which(mn.s==2 & dy.s==29)
		f29m1<-f29-1    
		jdate.s[f29]<-jdate.s[f29m1]+1

		f30 <- which(mn.s==2 & dy.s==30)
		f30m2<-f30-2    
		jdate.s[f30]<-jdate.s[f30m2]+2

		s.s     <- sort(as.real(jdate.s),index.return=T)[[2]]
		jdate.s <- jdate.s[s.s]
		d0.s    <- d0.s[,s.s]

		l.s[[e]]    <- d0.s
		date.s[[e]] <- jdate.s
}
		
	l1 <- list(data=l.s,dates=date.s,exptid=expt.list)
	l1
}

#######################################################
#######################################################
"get_1row_rcm17"<-
function(
	ycoord,
	expt.list,
	data.dir = '/project/extremes1/qump/rcm_europe/daily/',
	pat = '03236_temperature_at_1.5m_________________tmax',
	years = 1990:2099,
	idx = NA,
	multiyear = FALSE,
	quiet = TRUE
	)
{

#sjb#library(date)

source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_util.R",TRUE))

nexpt  <- length(expt.list)
l.r    <- vector(mode='list',length=nexpt)
date.r <- vector(mode='list',length=nexpt)
names(l.r) <- expt.list
names(date.r) <- expt.list

for (e in 1:nexpt) {
		path.to.search <- sub("eee",expt.list[e],data.dir)
		st.all         <- dir(path=path.to.search, pattern=pat,full.names=TRUE)
		i.e            <- grep(expt.list[e],st.all)
		st.all         <- st.all[i.e]
#browser()
		if(multiyear) {  # ie the file is multi-year

			cat("CAUTION - NOT SURE THIS FUNCTION IS WORKING FOR MULTI-YEAR FILES",cr)
			cat("Stopping in get_1row_rcm17()")
			return()
			
			st.all <- st.all
			nf     <- length(st.all)
			# here we read a row and then select the point later
			for (f in 1:nf) {

				pp1   <- read.row.pp(st.all[f],irow=ycoord,idxfields=idx, quiet=quiet)
				lbnpt <- pp1$lbnpt[1]
				lbrow <- pp1$lbrow[1]
				npy   <- pp1$nfields[1]

				if (f == 1) {
					d0.r <- array(0.0,dim=c(lbnpt,npy*nf))
					d0.r[1:lbnpt,1:npy] <- pp1$data
					yr.r <- pp1$lbyr
					mn.r <- pp1$lbmon
					dy.r <- pp1$lbdat
				} else {
					a                 <- (f-1)*npy+1
					b                 <- (f)*npy
					d0.r[1:lbnpt,a:b] <- pp1$data
					yr.r <- c(yr.r,pp1$lbyr)
					mn.r <- c(mn.r,pp1$lbmon)
					dy.r <- c(dy.r,pp1$lbdat)
				}
#browser()
			}

		} else { # files are one month for one year
			iy <- NULL
			for (i in 1:length(years)) {
				iy2 <-grep(as.character(years)[i],st.all)
				iy <- c(iy,iy2)
			}
			st.all         <- st.all[iy]
			nf             <- length(st.all)
			# here we read a whole row 
			for (f in 1:nf) {

				pp1   <- read.row.pp(st.all[f],irow=ycoord,idxfields=idx, quiet=quiet)
				lbnpt <- pp1$lbnpt[1]
				lbrow <- pp1$lbrow[1]
				npy   <- pp1$nfields[1]

				if (f == 1) {
					d0.r <- array(0.0,dim=c(lbnpt,npy*nf))
					d0.r[1:lbnpt,1:npy] <- pp1$data
					yr.r <- pp1$lbyr
					mn.r <- pp1$lbmon
					dy.r <- pp1$lbdat
				} else {
					a                 <- (f-1)*npy+1
					b                 <- (f)*npy
					d0.r[1:lbnpt,a:b] <- pp1$data
					yr.r <- c(yr.r,pp1$lbyr)
					mn.r <- c(mn.r,pp1$lbmon)
					dy.r <- c(dy.r,pp1$lbdat)
				}
				
			}
		} # end else
		
		if( (pp1$lbtim[1]%%100)%%10 == 2 ) jdate.r <- mdy.date.model(mn.r, dy.r, yr.r)
		else                               jdate.r <- mdy.date(mn.r, dy.r, yr.r)

		#jdate.r <- mdy.date.model(mn.r, dy.r, yr.r)
		## deal with feb29 & feb30
		#f29          <- which(mn.r==2 & dy.r==29)
		#f29m1        <- f29-1    
		#jdate.r[f29] <- jdate.r[f29m1]+1

		#f30          <- which(mn.r==2 & dy.r==30)
		#f30m2        <- f30-2    
		#jdate.r[f30] <- jdate.r[f30m2]+2
		
		isort <- sort(as.real(jdate.r),index.return=T)$ix

		d0.r           <- d0.r[,isort]
		jdate.r        <- jdate.r[isort]
		

		if( (pp1$lbtim[1]%%100)%%10 == 2 ) {
			class(jdate.r) <- "Modeldate"
			yr.r2          <- date.mdy.model(jdate.r)$myear
		} else 	yr.r2      <- date.mdy(jdate.r)$year
		
		iy          <- findall(years,yr.r2)
		l.r[[e]]    <- d0.r[,iy]
		
		jdate.r        <- jdate.r[iy]
		class(jdate.r) <- "Modeldate"
		date.r[[e]]    <- jdate.r

		#browser()
		
}

	l1 <- list(data=l.r,dates=date.r,exptid=expt.list)
	l1
}

#######################################################
#######################################################
"read_line_split_files_rcm17"<-
function(
	wave_ycoord,
	expt.list,
	data.dir = '/project/extremes1/qump/rcm_europe/daily/lines/temperature_at_1.5m_________________tmax/xxx',
	pat = '03236_8192_yxxx',
	years = 1950:2099,
	idx = NA,
	multiyear = TRUE
	)
{

cat("CAUTION - YCOORD IN WAVE UNITS PLEASE",cr)

#sjb#library(date)

source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_util.R",TRUE))
source(system("echo /net/home/h03/hadsx/extremes/R/libs/pp_io.R",TRUE))

nexpt  <- length(expt.list)
l.r    <- vector(mode='list',length=nexpt)
date.r <- vector(mode='list',length=nexpt)
names(l.r) <- expt.list
names(date.r) <- expt.list

for (e in 1:nexpt) {
		path.to.search <- sub("xxx",expt.list[e],data.dir)
		buf1           <- '000'
		sty            <- paste(wave_ycoord)
		buf2           <- paste(sprintf(paste("%.",nchar(buf1)-nchar(sty),"s",sep=""),buf1),sty,sep='')
		pat2           <- sub("xxx",buf2,pat)
		st.all         <- dir(path=path.to.search, pattern=pat2,full.names=TRUE)
		i.e            <- grep(expt.list[e],st.all)
		st.all         <- st.all[i.e]
		# temp patch
		st.all         <- st.all[1]
		
		if(multiyear) {  # ie the file is multi-year

			st.all <- st.all
			nf     <- length(st.all)
			# here we read a row and then select the point later
			for (f in 1:nf) {

				pp1   <- read.row.pp(st.all[f],irow=1,idxfields=idx)
				lbnpt <- pp1$lbnpt[1]
				lbrow <- pp1$lbrow[1]
				npy   <- pp1$nfields[1]

				if (f == 1) {
					d0.r <- array(0.0,dim=c(lbnpt,npy*nf))
					d0.r[1:lbnpt,1:npy] <- pp1$data
					yr.r <- pp1$lbyr
					mn.r <- pp1$lbmon
					dy.r <- pp1$lbdat
				} else {
					a                 <- (f-1)*npy+1
					b                 <- (f)*npy
					d0.r[1:lbnpt,a:b] <- pp1$data
					yr.r <- c(yr.r,pp1$lbyr)
					mn.r <- c(mn.r,pp1$lbmon)
					dy.r <- c(dy.r,pp1$lbdat)
				}
			}

		} else { # years are in different files
			cat('NOT SUPPORTED',cr)
		} # end else
		
		jdate.r <- mdy.date(mn.r, dy.r, yr.r)
		# deal with feb29 & feb30
		f29          <- which(mn.r==2 & dy.r==29)
		f29m1        <- f29-1    
		jdate.r[f29] <- jdate.r[f29m1]+1

		f30          <- which(mn.r==2 & dy.r==30)
		f30m2        <- f30-2    
		jdate.r[f30] <- jdate.r[f30m2]+2
		
		isort <- sort(as.real(jdate.r),index.return=T)$ix

		d0.r        <- d0.r[,isort]
		jdate.r     <- jdate.r[isort]
		
		yr.r2       <- date.mdy(jdate.r)$year
		iy          <- findall(years,yr.r2)
		l.r[[e]]    <- d0.r[,iy]
		date.r[[e]] <- jdate.r[iy]
}
		
	l1 <- list(data=l.r,dates=date.r,exptid=expt.list)
	l1
}

#######################################################
#######################################################
"list_3d_2_array"<-
function(list1)
{
	l1 <- length(list1)
	d1 <- dim(list1[[1]])[1]
	d2 <- dim(list1[[1]])[2]
	
	data1 <- array(NA,dim=c(l1,d1,d2))
	
	for (e in 1:l1) {
		data1[e,,] <- list1[[e]]
	}
	
	data1
	
}
#######################################################
#######################################################
"list_2d_2_array"<-
function(list1)
{
	l1 <- length(list1)
	d1 <- length(list1[[1]])[1]
	
	data1 <- array(NA,dim=c(l1,d1))
	
	for (e in 1:l1) {
		data1[e,] <- list1[[e]]
	}
	
	data1
	
}
#######################################################
#######################################################
"get_trans17_global_mean_responses"<-
function(
	expt.list, 
	trans_path ="/data/cr1/hadsx/PPTS/ts_xxx_03236_tran_global.txt",
	dates,	# julian dates
	startend,
	spar = 0.8,
	QUIET = T
	)
{

# 2012/03/29 mk1 could return dates for which there is no global temperature
	
#sjb#library(date)


if( (class(dates[[1]]) == "Modeldate") || (class(dates) == "Modeldate") ) {
	is.model.date=TRUE
	#cat('modaldate class detected',cr)
} else if (is.date(dates[[1]])) {
	is.model.date=FALSE
	#cat('date class detected',cr)
} else {
	cat('Needs date structure in get_trans17_global_mean_responses',cr)
	return(NA)
}

#	cat('get_trans17_global_mean_responses: Non-model dates assumed','\n')

if(is.model.date) {

	if(!QUIET) cat('Model dates detected in get_trans17_global_mean_responses',cr)

	if(missing(startend)) {
		s.date <- mdy.date.model(1,1,1950)
		e.date <- mdy.date.model(12,31,2099)
	} else {
		if ( (class(startend) != "Modeldate") ) {
			cat('get_trans17_global_mean_responses: date incompatability',cr)
			return(NA)
		}
		s.date <- startend[1]
		e.date <- startend[2]
	}

	# get transient global temperatures
	nexpt <- length(expt.list)
	gt.t  <- vector(mode='list',length=nexpt)
	y.t   <- vector(mode='list',length=nexpt)
	
	for (e in 1:nexpt) {
		st.global.temp <- sub("xxx",expt.list[e],trans_path)
		gt.all  <- read.table(st.global.temp,col.names=c('t','year','month','day'),sep=',')
		gt.date <- mdy.date.model(gt.all$month, gt.all$day, gt.all$year)
		ygt     <- gt.all$year
		id      <- which(gt.date>=s.date & gt.date<=e.date)	#2012.03.29 changed to >=, <=
		gty     <- smooth.spline(gt.all$t[id],spar=spar)$y
		ygt     <- ygt[id]
		
		# interpolate to the frewuency of dates
		y0      <- date.mdy.model(dates[[e]])$myear
		gt0     <- gty[findInterval(y0,ygt)]
		y0      <- y0[findInterval(y0,ygt)]	#2012.03.29 to esnure gt0 & y0 has same number of elements
		
		gt.t[[e]] <- gt0
		y.t[[e]]  <- y0

	}
} else {

	if(missing(startend)) {
		s.date <- mdy.date(1,1,1950)
		e.date <- mdy.date(12,31,2099)
	} else {
		s.date <- startend[1]
		e.date <- startend[2]
	}

	# get transient global temperatures
	nexpt <- length(expt.list)
	gt.t  <- vector(mode='list',length=nexpt)
	y.t   <- vector(mode='list',length=nexpt)
	
	for (e in 1:nexpt) {
		st.global.temp <- sub("xxx",expt.list[e],trans_path)
		gt.all  <- read.table(st.global.temp,col.names=c('t','year','month','day'),sep=',')
		gt.date <- mdy.date(gt.all$month, gt.all$day, gt.all$year)
		ygt     <- gt.all$year
		id      <- which(gt.date>=s.date & gt.date<=e.date)	#2012.03.29 changed to >=, <=
		gty     <- smooth.spline(gt.all$t[id],spar=spar)$y
		ygt     <- ygt[id]
		
		# interpolate to the frewuency of dates
		y0      <- date.mdy(dates[[e]])$year
		gt0     <- gty[findInterval(y0,ygt)]
		y0      <- y0[findInterval(y0,ygt)]	#2012.03.29 to esnure gt0 & y0 has same number of elements

		gt.t[[e]] <- gt0
		y.t[[e]]  <- y0

	}
}
	list(expt.id=expt.list, global.temp=gt.t,date=y.t)

}
#######################################################
#######################################################
"get_trans17_global_mean_responses.mk1"<-
function(
	expt.list, 
	trans_path ="/data/cr1/hadsx/PPTS/ts_xxx_03236_tran_global.txt",
	dates,	# julian dates
	startend,
	spar = 0.8
	)
{
	
#sjb#library(date)


if( (class(dates[[1]]) == "Modeldate") || (class(dates) == "Modeldate") ) {
	is.model.date=TRUE
	#cat('modaldate class detected',cr)
} else if (is.date(dates[[1]])) {
	is.model.date=FALSE
	#cat('date class detected',cr)
} else {
	cat('Needs date structure in get_trans17_global_mean_responses',cr)
	return(NA)
}

#	cat('get_trans17_global_mean_responses: Non-model dates assumed','\n')

if(is.model.date) {

	cat('Model dates detected in get_trans17_global_mean_responses',cr)

	if(missing(startend)) {
		s.date <- mdy.date.model(1,1,1950)
		e.date <- mdy.date.model(12,31,2099)
	} else {
		if ( (class(startend) != "Modeldate") ) {
			cat('get_trans17_global_mean_responses: date incompatability',cr)
			return(NA)
		}
		s.date <- startend[1]
		e.date <- startend[2]
	}

	# get transient global temperatures
	nexpt <- length(expt.list)
	gt.t  <- vector(mode='list',length=nexpt)
	y.t   <- vector(mode='list',length=nexpt)
	
	for (e in 1:nexpt) {
		st.global.temp <- sub("xxx",expt.list[e],trans_path)
		gt.all  <- read.table(st.global.temp,col.names=c('t','year','month','day'),sep=',')
		gt.date <- mdy.date.model(gt.all$month, gt.all$day, gt.all$year)
		ygt     <- gt.all$year
		id      <- which(gt.date>s.date & gt.date<e.date)
		gty     <- smooth.spline(gt.all$t[id],spar=spar)$y
		ygt     <- ygt[id]
		
		# interpolate to the frewuency of dates
		y0      <- date.mdy.model(dates[[e]])$myear
		gt0     <- gty[findInterval(y0,ygt)]
		
		gt.t[[e]] <- gt0
		y.t[[e]]  <- y0

	}
} else {

	if(missing(startend)) {
		s.date <- mdy.date(1,1,1950)
		e.date <- mdy.date(12,31,2099)
	} else {
		s.date <- startend[1]
		e.date <- startend[2]
	}

	# get transient global temperatures
	nexpt <- length(expt.list)
	gt.t  <- vector(mode='list',length=nexpt)
	y.t   <- vector(mode='list',length=nexpt)
	
	for (e in 1:nexpt) {
		st.global.temp <- sub("xxx",expt.list[e],trans_path)
		gt.all  <- read.table(st.global.temp,col.names=c('t','year','month','day'),sep=',')
		gt.date <- mdy.date(gt.all$month, gt.all$day, gt.all$year)
		ygt     <- gt.all$year
		id      <- which(gt.date>s.date & gt.date<e.date)
		gty     <- smooth.spline(gt.all$t[id],spar=spar)$y
		ygt     <- ygt[id]
		
		# interpolate to the frewuency of dates
		y0      <- date.mdy(dates[[e]])$year
		gt0     <- gty[findInterval(y0,ygt)]
		
		gt.t[[e]] <- gt0
		y.t[[e]]  <- y0

	}
}
	list(expt.id=expt.list, global.temp=gt.t,date=y.t)

}
#######################################################
#######################################################
"get_slab_global_mean_responses"<-
function(
	expt.list, 
	slab_path  ='/net/home/h03/hadsx/extremes/R/ukcip/t2m/referance_data/global_mean_temperature.txt'
	)
{
	# read slab global mean temperatures
	c.types        <- c(rep("character",17))
	d1.names       <- read.table(slab_path,skip=0,colClasses=c.types,nrows=1,fill=TRUE,sep=',')
	c.types        <- c(rep("character",2),rep("real",15))
	slab.responses <- read.table(slab_path,skip=1,colClasses=c.types,fill=TRUE,sep=',',col.names=d1.names)
	expt.x1.list   <- slab.responses$co1_expt
	expt.x2.list   <- slab.responses$co2_expt

	i.1x    <- match(expt.list,expt.x1.list)
	expt.1x <- expt.x1.list[i.1x]
	expt.2x <- expt.x2.list[i.1x]

	gt.s      <- slab.responses[i.1x,]

	gt.s
	
}
#######################################################
#######################################################
"zero_histogram_equalise"<-
function(d0, show.plot=FALSE)
{
	flag <- 0
	d1   <- d0
	d1.n <- trunc(min(d1,na.rm=TRUE))
	d1.x <- trunc(max(d1,na.rm=TRUE))+1
	if(d1.n<272 & d1.x>274) {	# data spans zero
		d1.h  <- hist(d1,breaks=(d1.n:d1.x),plot=FALSE)		
		s1    <- sort(d1.h$counts,decreasing = TRUE,index.return=TRUE)
		c.272 <- d1.h$counts[which(d1.h$breaks==272)]
		c.273 <- d1.h$counts[which(d1.h$breaks==273)]
		c.274 <- d1.h$counts[which(d1.h$breaks==274)]

		#if ( !( (c.273>c.272 & c.273<c.274) || (c.273<c.272 & c.273>c.274)) ) {
		if ( (c.273>c.272 & c.273>c.274) ) {
			c.273.old <- c.273
			c.273     <- trunc((c.272+c.274)/2.0)
			i <- (1:length(d1))[(d1>=273.0)&(d1<274.0)]
			if (all(is.finite(c(i, c.273, s1$x[1],c.273.old)))) {
				if((c.273.old-c.273) >0) {
					s     <- sample(i,(c.273.old-c.273),replace=FALSE)
					d1[s] <- NA
					flag <- 1
				}
			} else {
				d1[which(d1>272.9 & d1<273.5)]         <- NA
				cat("PROBLEM IN HISTOGRAM EQUALISATION ",cr)
			}
		#d1b.h  <- hist(d1[[e]],breaks=(d1.n:d1.x),plot=TRUE)
		#abline(v=273.16,col=2)
		#c.272 <- d1b.h$counts[which(d1.h$breaks==272)]
		#c.273 <- d1b.h$counts[which(d1.h$breaks==273)]
		#c.274 <- d1b.h$counts[which(d1.h$breaks==274)]
		#cat("counts 3 ",c.272,c.273,c.274,cr)
		#readline("Continue? post histo")				
		}
	}

	if(show.plot) {
		x11()
		par(mfcol=c(2,1))
		d1.h <- hist(d0,breaks=(d1.n:d1.x),plot=TRUE)
		abline(v=273.16,col=2)
		d1.h <- hist(d1,breaks=(d1.n:d1.x),plot=TRUE)
		abline(v=273.16,col=2)
	}

	list(data=d1,equalised.flag=flag)
}
#######################################################
#######################################################
"threshold_spline_fast"<-
function(d0, t0, show.plot=FALSE, quantile=0.75, max.to.keep=0.06, bias.step=NA, smooth=1.2, max.iter=500, is.model.date=TRUE)
{
#sjb#library(date)


	if( (class(t0) == "Modeldate") ) {
		is.model.date=TRUE
		#cat('modaldate class detected',cr)
	} else if (is.date(t0)) {
		is.model.date=FALSE
		#cat('date class detected',cr)
	} else {
		cat('Needs date structure in threshold_spline',cr)
		return(NA)
	}
	
	if(length(max.to.keep)>1) {
		cat('ERROR threshold_spline: length(max.to.keep)>1',cr)
		return(NA)
	}
	
	if(is.na(bias.step)) {
		bias.step <- (max(d0,na.rm=T) - quantile(d0,probs=0.8,na.rm=TRUE))/max.iter
	}
	
	# first fint the upper quantile of data for each year then smooth with spline
	if(is.model.date) y1   <- date.mdy.model(t0)$myear
	else              y1   <- date.mdy(t0)$year
	
	y.d2 <- unique(y1)
	d2.q <- real(length(y.d2))
	for (f2 in 1:length(y.d2)) {
		i  <- which(y1 == y.d2[f2])
		s1 <- sort(d0[i])
		if (length(s1) == 1) 	it <- 1
		else					it <- trunc(length(s1) * quantile)
		if (it>0) d2.q[f2] <- s1[it] else d2.q[f2] <- NA
		}
	ix  <- is.finite(d2.q)
	#SMOOTH THE INITIAL QUANTILES AND THEN INTERPOLATE
	sd2.q<-smooth.spline(d2.q[ix],spar=smooth)$y
	th  <- sd2.q[findInterval(y1,y.d2[ix])]

	# threshold set so that faction of data = max.to.keep
	#pre 091207 th.bias   <- max(d0,na.rm=TRUE)-max(th,na.rm=TRUE)
	th.bias   <- quantile(d0,probs=(1-0.3*max.to.keep),na.rm=TRUE)-max(th,na.rm=TRUE)
	th        <- th+th.bias
	nex       <- length(which(d0>th))
	i.count   <- 1
	while ( ((nex/length(d0)) < max.to.keep) && (i.count < max.iter) ) {
		th      <- th - bias.step
		nex     <- length(which(d0>th))
		i.count <- i.count + 1
	}

	if(show.plot) {
		x11()
		par(mfcol=c(1,1))
		plot(t0,d0,cex=0.3)
		yu <- mdy.date(month=6,day=1,year=y.d2)
		points(yu,d2.q,col=3)
		lines(t0,th,col=2)
	}
	
	if(i.count == max.iter) {
		th <- NULL
		cat('ERROR:THRESHOLD_SPLINE HAS HIT THE BUFFERS',cr)
	}
	
	th
}
#######################################################
#######################################################
"threshold_spline"<-
function(d0, t0, show.plot=FALSE, quantile=0.75, max.to.keep=0.06, bias.step=NA, smooth=1.2, max.iter=500, is.model.date=TRUE)
{
#sjb#library(date)


	if( (class(t0) == "Modeldate") ) {
		is.model.date=TRUE
		#cat('modaldate class detected',cr)
	} else if (is.date(t0)) {
		is.model.date=FALSE
		#cat('date class detected',cr)
	} else {
		cat('Needs date structure in threshold_spline',cr)
		return(NA)
	}
	
	if(length(max.to.keep)>1) {
		cat('ERROR threshold_spline: length(max.to.keep)>1',cr)
		return(NA)
	}
	
	if(is.na(bias.step)) {
		bias.step <- (max(d0,na.rm=T) - quantile(d0,probs=0.8,na.rm=TRUE))/max.iter
	}
	
	# first fint the upper quantile of data for each year then smooth with spline
	if(is.model.date) y1   <- date.mdy.model(t0)$myear
	else              y1   <- date.mdy(t0)$year
	
	y.d2 <- unique(y1)
	d2.q <- real(length(y.d2))
	for (f2 in 1:length(y.d2)) {
		i  <- which(y1 == y.d2[f2])
		s1 <- sort(d0[i])
		if (length(s1) == 1) 	it <- 1
		else					it <- trunc(length(s1) * quantile)
		if (it>0) d2.q[f2] <- s1[it] else d2.q[f2] <- NA
		}
	ix  <- is.finite(d2.q)
	th  <- d2.q[ix][findInterval(y1,y.d2[ix])]
	if (length(th) > 1000000) {
		cat('threshold_spline: smooth.spline cannot cope with vectors longer than 1e6: ',length(th),' \n')
		return(NA)
	}
	th  <- smooth.spline(th,spar=smooth)$y

	# threshold set so that faction of data = max.to.keep
	#pre 091207 th.bias   <- max(d0,na.rm=TRUE)-max(th,na.rm=TRUE)
	th.bias   <- quantile(d0,probs=(1-0.3*max.to.keep),na.rm=TRUE)-max(th,na.rm=TRUE)
	th        <- th+th.bias
	nex       <- length(which(d0>th))
	i.count   <- 1
	while ( ((nex/length(d0)) < max.to.keep) && (i.count < max.iter) ) {
		th      <- th - bias.step
		nex     <- length(which(d0>th))
		i.count <- i.count + 1
	}

	if(show.plot) {
		x11()
		par(mfcol=c(1,1))
		plot(t0,d0,cex=0.3)
		lines(t0,th,col=2)
	}
	
	if(i.count == max.iter) {
		th <- NULL
		cat('ERROR:THRESHOLD_SPLINE HAS HIT THE BUFFERS',cr)
	}
	
	th
}
#######################################################
#######################################################
"threshold_spline.old"<-
function(d0, t0, show.plot=FALSE, quantile=0.75, max.to.keep=0.06, bias.step=NA, smooth=1.2, max.iter=500, is.model.date=TRUE)
{
#sjb#library(date)


	if(!is.date(t0)) {
		cat('Needs date structure in threshold_spline',cr)
		return(NA)
	}
	
	if(length(max.to.keep)>1) {
		cat('ERROR threshold_spline: length(max.to.keep)>1',cr)
		return(NA)
	}
	
	if(is.na(bias.step)) {
		bias.step <- (max(d0,na.rm=T) - quantile(d0,probs=0.8,na.rm=TRUE))/max.iter
	}
	
	# first fint the upper quantile of data for each year then smooth with spline
	if(is.model.date) y1   <- date.mdy.model(t0)$year
	else              y1   <- date.mdy(t0)$year
	
	y.d2 <- unique(y1)
	d2.q <- real(length(y.d2))
	for (f2 in 1:length(y.d2)) {
		i  <- which(y1 == y.d2[f2])
		s1 <- sort(d0[i])
		if (length(s1) == 1) 	it <- 1
		else					it <- trunc(length(s1) * quantile)
		if (it>0) d2.q[f2] <- s1[it] else d2.q[f2] <- NA
		}
	ix  <- is.finite(d2.q)
	th  <- d2.q[ix][findInterval(y1,y.d2[ix])]
	if (length(th) < 1000000) {
		cat('threshold_spline: smooth.spline cannot cope with vectors longer than 1e6 \n')
		return(NA)
	}
	th  <- smooth.spline(th,spar=smooth)$y

	# threshold set so that faction of data = max.to.keep
	#pre 091207 th.bias   <- max(d0,na.rm=TRUE)-max(th,na.rm=TRUE)
	th.bias   <- quantile(d0,probs=(1-0.3*max.to.keep),na.rm=TRUE)-max(th,na.rm=TRUE)
	th        <- th+th.bias
	nex       <- length(which(d0>th))
	i.count   <- 1
	while ( ((nex/length(d0)) < max.to.keep) && (i.count < max.iter) ) {
		th      <- th - bias.step
		nex     <- length(which(d0>th))
		i.count <- i.count + 1
	}

	if(show.plot) {
		x11()
		par(mfcol=c(1,1))
		plot(t0,d0,cex=0.3)
		lines(t0,th,col=2)
	}
	
	if(i.count == max.iter) {
		th <- NULL
		cat('ERROR:THRESHOLD_SPLINE HAS HIT THE BUFFERS',cr)
	}
	
	th
}
#######################################################
#######################################################
"threshold_spline_nodate"<-
function(d0, show.plot=FALSE, q=0.75, max.to.keep=0.06, bias.step=NA, nyr=NULL, smooth=1.2, max.iter=500)
{
#sjb#library(date)


	if(length(max.to.keep)>1) {
		cat('ERROR threshold_spline: length(max.to.keep)>1',cr)
		return(NA)
	}
	
	if(is.na(bias.step)) {
		bias.step <- (max(d0,na.rm=T) - quantile(d0,probs=0.8,na.rm=TRUE))/max.iter
	}
	
	# first find the upper quantile of data for each year then smooth with spline
	npy  <- ceiling(length(d0)/nyr)
	y1   <- (1:length(d0))/npy
	ny   <- trunc(length(d0)/npy)
	
	q2 <- NULL
	y2 <- NULL
	for (y in 1:ny) {
		q2[y] <- quantile(d0[(1+(y-1)*npy):(y*npy)],q)
		y2[y] <- mean(y1[(1+(y-1)*npy):(y*npy)])
	}
	#y.d2 <- unique(y1)
	#d2.q <- real(length(y.d2))
	#for (f2 in 1:length(y.d2)) {
	#	i  <- which(y1 == y.d2[f2])
	#	s1 <- sort(d0[i])
	#	if (length(s1) == 1) 	it <- 1
	#	else					it <- trunc(length(s1) * q)
	#	if (it>0) d2.q[f2] <- s1[it] else d2.q[f2] <- NA
	#	}
	ix  <- is.finite(q2)
	iy  <- findInterval(y1,y2[ix])
	iy[which(iy==0)] <- 1
	th  <- q2[ix][iy]
	th  <- smooth.spline(th,spar=smooth)$y

	# threshold set so that faction of data = max.to.keep
	#pre 091207 th.bias   <- max(d0,na.rm=TRUE)-max(th,na.rm=TRUE)
	th.bias   <- quantile(d0,probs=(1-0.3*max.to.keep),na.rm=TRUE)-max(th,na.rm=TRUE)
	th        <- th+th.bias
	nex       <- length(which(d0>th))
	i.count   <- 1
	while ( ((nex/length(d0)) < max.to.keep) && (i.count < max.iter) ) {
		th      <- th - bias.step
		nex     <- length(which(d0>th))
		i.count <- i.count + 1
	}
#browser()
	if(show.plot) {
		x11()
		par(mfcol=c(1,1))
		plot(y1,d0,cex=0.3)
		lines(y1,th,col=2)
	}
	
	if(i.count == max.iter) {
		th <- NULL
		cat('ERROR:THRESHOLD_SPLINE HAS HIT THE BUFFERS',cr)
	}
	
	th
}
#######################################################
#######################################################
"threshold_trend"<-
function(d0, t0, show.plot=FALSE, quantile=0.8, max.to.keep=0.06, bias.step=0.1, max.iter=200)
{
#sjb#library(date)


	rt0 <- as.real(t0)
	### calculate time dependant threshold using thinned data
	df0    <- data.frame(y1=d0,x1=rt0)
	linreg <- lm(y1 ~ x1,data=df0)


	# threshold set so that faction of data = max.to.keep
	th.bias <- max(d0)-(linreg$coef[1] + linreg$coef[2]*rt0[1])
	th      <- th.bias + linreg$coef[1] + linreg$coef[2]*rt0

	nex       <- length(which(d0>th))
	i.count   <- 1
	while ( ((nex/length(d0)) < max.to.keep) && (i.count < max.iter) ) {
		th      <- th - bias.step
		nex     <- length(which(d0>th))
		i.count <- i.count + 1
	}

	if(show.plot) {
		x11()
		par(mfcol=c(1,1))
		plot(t0,d0)
		lines(t0,th,col=2)
	}

	th
}
#######################################################
#######################################################
"threshold_global_mean"<-
function(d0, gt, show.plot=FALSE, max.to.keep=0.06, bias.step=0.1, max.iter=200)
{


	# threshold set so that faction of data = max.to.keep
	th.bias <- max(d0)-min(gt)
	th      <- th.bias + gt

	nex       <- length(which(d0>th))
	i.count   <- 1
	while ( ((nex/length(d0)) < max.to.keep) && (i.count < max.iter) ) {
		th      <- th - bias.step
		nex     <- length(which(d0>th))
		i.count <- i.count + 1
	}

	if(show.plot) {
		x11()
		par(mfcol=c(1,1))
		plot(t0,d0)
		lines(t0,th,col=2)
	}

	th
}
#######################################################
#######################################################
"qump_qq_plot"<-
function(l.pp, imodel, nrow=6, ncol=3, newplot=TRUE, ylim=NA, cex=0.25, ann=F, main=T, title=NULL, warning=T)
{
	source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_pp_reference.R",TRUE))
	
	if(newplot) x11( width = 14, height = 10)
	par(mfcol=c(nrow,ncol))
	par(mar=c(2,1,2,1),oma=c(1,1,2,1), ann=ann)
	
	ne <- length(l.pp)
	for(e in 1:ne) {
		z      <- l.pp[[e]][[imodel]]
		good.z <- (z$conv==0) && (all(!is.na(z$se))) && (all(z$se > 1e-2)) 
		
		if(!is.na(match(imodel,c('loc.x12','sca.x12','shp.x12','sta.x12')))) tit1 = paste( l.pp[[e]][[2]], l.pp[[e]][[3]],sep=',')
		else if(!is.na(match(imodel,c('loc.t','sca.t','shp.t','sta.t'))))  tit1 = l.pp[[e]][[1]]
		else if(!is.na(match(imodel,c('loc.r','sca.r','shp.r','sta.r'))))  tit1 = l.pp[[e]][[1]]

		if(good.z ) qq_plot.sjb5(z,main=tit1, warning=warning)
		else        plot(0,0,main=tit1,type='n')
		
	}
	tit0 = paste("QQ plot: ",title,sep='')
	if(main) mtext(tit0,outer=T,col=1,line=-0.5)
}
#######################################################
#######################################################
"qump_qq_diff_plot"<-
function(l.pp, imodel, nrow=6, ncol=3, newplot=TRUE, commonylim=T, ylim=NULL, cex=0.25, ann=F, main=T, title=NULL, warning=T)
{
	source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_pp_reference.R",TRUE))
	
	if(newplot) x11( width = 14, height = 10)
	par(mfcol=c(nrow,ncol))
	par(mar=c(2,1,2,1),oma=c(1,1,2,1), ann=ann)
	
	ne <- length(l.pp)
	if(commonylim) {
		qdx=0
		for(e in 1:ne) {
			z      <- l.pp[[e]][[imodel]]
			if (is.list(z)) good.z <- (z$conv==0) && (all(!is.na(z$se))) && (all(z$se > 1e-2)) 
			else good.z=FALSE
			if(good.z ) qdx <- max(c(qdx,abs(get.pp.quantiles.sjb5(z, get.ranked.probabilities.sjb5(z, warning=F), warning=F)-z$xindata)))
		}
		ylim=c(-qdx,qdx)
	}
	
	for(e in 1:ne) {
		z      <- l.pp[[e]][[imodel]]
		if (is.list(z)) good.z <- (z$conv==0) && (all(!is.na(z$se))) && (all(z$se > 1e-2)) 
		else good.z=FALSE
		
		if(!is.na(match(imodel,c('loc.x12','sca.x12','shp.x12','sta.x12')))) tit1 = paste( l.pp[[e]][[2]], l.pp[[e]][[3]],sep=',')
		else if(!is.na(match(imodel,c('loc.t','sca.t','shp.t','sta.t'))))  tit1 = l.pp[[e]][[1]]
		else if(!is.na(match(imodel,c('loc.r','sca.r','shp.r','sta.r'))))  tit1 = l.pp[[e]][[1]]

		if(good.z ) qq_diff_plot.sjb5(z,main=tit1, ylim=ylim, warning=warning)
		else        plot(0,0,main=tit1,type='n')
	}
	tit0 = paste("QQ type Residuals (actual residuals) wrt Rank: ",title,sep='')
	if(main) mtext(tit0,outer=T,col=1,line=-0.5)
}
#######################################################
#######################################################
"qump_qq_diff_time_plot"<-
function(l.pp, imodel, nrow=6, ncol=3, newplot=TRUE, commonylim=T, ylim=NULL, cex=0.25, ann=F, main=T, title=NULL, warning=T)
{
	source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_pp_reference.R",TRUE))
	
	if(newplot) x11( width = 14, height = 10)
	par(mfcol=c(nrow,ncol))
	par(mar=c(2,1,2,1),oma=c(1,1,2,1), ann=ann)
	
	ne <- length(l.pp)
	if(commonylim) {
		qdx=0
		ne <- length(l.pp)
		for(e in 1:ne) {
			z      <- l.pp[[e]][[imodel]]
			good.z <- (z$conv==0) && (all(!is.na(z$se))) && (all(z$se > 1e-2)) 
			if(good.z ) qdx <- max(c(qdx,abs(get.pp.quantiles.sjb5(z, get.ranked.probabilities.sjb5(z, warning=F), warning=F)-z$xindata)))
		}
		ylim=c(-qdx,qdx)
	}
	
	for(e in 1:ne) {
		z <- l.pp[[e]][[imodel]]
		
		if(!is.na(match(imodel,c('loc.x12','sca.x12','shp.x12','sta.x12')))) tit1 = paste( l.pp[[e]][[2]], l.pp[[e]][[3]],sep=',')
		else if(!is.na(match(imodel,c('loc.t','sca.t','shp.t','sta.t'))))  tit1 = l.pp[[e]][[1]]
		else if(!is.na(match(imodel,c('loc.r','sca.r','shp.r','sta.r'))))  tit1 = l.pp[[e]][[1]]

		if(good.z ) qq_diff_time_plot.sjb5(z,main=tit1, ylim=ylim, warning=warning)
		else        plot(0,0,main=tit1,type='n')
		
	}
	tit0 = paste("QQ type Residuals (actual residuals) wrt Time: ",title,sep='')
	if(main) mtext(tit0,outer=T,col=1,line=-0.5)
}
#######################################################
#######################################################
"qump_return_level_plot"<-
function(l.pp, imodel, nrow=6, ncol=3, newplot=TRUE, ylim=NA, cex=0.25, ann=F, main=T, title=NULL, warning=T)
{
	source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_pp_checked.R",TRUE))
	
	if(newplot) x11( width = 14, height = 10)
	par(mfcol=c(nrow,ncol))
	par(mar=c(2,1,2,1),oma=c(1,1,2,1), ann=ann)
	
	ne <- length(l.pp)
	for(e in 1:ne) {
		z      <- l.pp[[e]][[imodel]]
		good.z <- (z$conv==0) && (all(!is.na(z$se))) && (all(z$se > 1e-2)) 
		
		if(!is.na(match(imodel,c('loc.x12','sca.x12','shp.x12','sta.x12')))) tit1 = paste( l.pp[[e]][[2]], l.pp[[e]][[3]],sep=',')
		else if(!is.na(match(imodel,c('loc.t','sca.t','shp.t','sta.t'))))  tit1 = l.pp[[e]][[1]]
		else if(!is.na(match(imodel,c('loc.r','sca.r','shp.r','sta.r'))))  tit1 = l.pp[[e]][[1]]
		
		if(good.z ) {
			if(e == 1) return_level_plot.sjb5(z,main=tit1,plot.exp=F, warning=warning, do.leg=T)
			else       return_level_plot.sjb5(z,main=tit1,plot.exp=F, warning=warning, do.leg=F)
		} else  plot(0,0,main=tit1,type='n')
	}
	tit0 = paste("Return Levels: ",title,sep='')
	if(main) mtext(tit0,outer=T,col=1,line=-0.5)
}
#######################################################
#######################################################
"qump_relative_return_level_plot"<-
function(l.pp, imodel, nrow=6, ncol=3, newplot=TRUE, ylim=NA, cex=0.25, ann=F, main=T, title=NULL, warning=T)
{
	source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_pp_reference.R",TRUE))
	
	if(newplot) x11( width = 14, height = 10)
	par(mfcol=c(nrow,ncol))
	par(mar=c(2,1,2,1),oma=c(1,1,2,1), ann=ann)
	
	ne <- length(l.pp)
	for(e in 1:ne) {
		z      <- l.pp[[e]][[imodel]]
		good.z <- (z$conv==0) && (all(!is.na(z$se))) && (all(z$se > 1e-2)) 
		
		if(!is.na(match(imodel,c('loc.x12','sca.x12','shp.x12','sta.x12')))) tit1 = paste( l.pp[[e]][[2]], l.pp[[e]][[3]],sep=',')
		else if(!is.na(match(imodel,c('loc.t','sca.t','shp.t','sta.t'))))  tit1 = l.pp[[e]][[1]]
		else if(!is.na(match(imodel,c('loc.r','sca.r','shp.r','sta.r'))))  tit1 = l.pp[[e]][[1]]
		
		if(good.z ) {
			if(e == 1) relative_return_level_plot.sjb5(z,main=tit1,plot.exp=F, warning=warning, do.leg=T)
			else       relative_return_level_plot.sjb5(z,main=tit1,plot.exp=F, warning=warning, do.leg=F)
		} else        plot(0,0,main=tit1,type='n')
	}
	tit0 = paste("Return Levels relative to 2yearRL wrt Rank: ",title,sep='')
	if(main) mtext(tit0,outer=T,col=1,line=-0.5)
}
#######################################################
#######################################################
"qump_return_period_diff_plot"<-
function(l.pp, imodel, nrow=6, ncol=3, newplot=TRUE, ylim=NA, cex=0.25, ann=F, main=T, title=NULL, warning=T)
{
	source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_pp_reference.R",TRUE))
	
	if(newplot) x11( width = 14, height = 10)
	par(mfcol=c(nrow,ncol))
	par(mar=c(2,1,2,1),oma=c(1,1,2,1), ann=ann)
	
	ne <- length(l.pp)
	for(e in 1:ne) {
		z      <- l.pp[[e]][[imodel]]
		good.z <- (z$conv==0) && (all(!is.na(z$se))) && (all(z$se > 1e-2)) 
		
		if(!is.na(match(imodel,c('loc.x12','sca.x12','shp.x12','sta.x12')))) tit1 = paste( l.pp[[e]][[2]], l.pp[[e]][[3]],sep=',')
		else if(!is.na(match(imodel,c('loc.t','sca.t','shp.t','sta.t'))))  tit1 = l.pp[[e]][[1]]
		else if(!is.na(match(imodel,c('loc.r','sca.r','shp.r','sta.r'))))  tit1 = l.pp[[e]][[1]]
		
		if(good.z ) return_period_diff_plot.sjb5(z,main=tit1, warning=warning)
		else        plot(0,0,main=tit1,type='n')
		
	}
	tit0 = paste("Model-Ranked Probability Residuals (%) wrt Rank: ",title,sep='')
	if(main) mtext(tit0,outer=T,col=1,line=-0.5)
}
#######################################################
#######################################################
"plot_data_w_threshold"<-
function(d0, t0, th, i.t, xcoord, expt, nrow=6, ncol=3, newplot=TRUE, ylim=NA, cex=0.25, ann=F, main=T)
{
	#source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_pp_checked.R",TRUE))
	
	#iti<-list_2d_2_array(i.t) due to data dropouts cannot use this need to keep as list
	
	if(newplot) x11()
	par(mfcol=c(nrow,ncol))
	par(mar=c(2,1,2,1),oma=c(1,1,2,1), ann=ann)
	
	if(missing(expt)) ne <- length(d0)
	else              ne <- length(expt)
	
	for(e in 1:ne) {

		if(is.na(ylim)) {
			ylim <- range(d0[[e]][xcoord,i.t[[e]]],na.rm=TRUE)
			ylim.was.not.set <- TRUE
		}

		 plot(t0[[e]][i.t[[e]]],d0[[e]][xcoord,i.t[[e]]],ylim=ylim,pch=33)
		lines(t0[[e]][i.t[[e]]],th[[e]],col=2)
		if(is.character(expt[e])) title(expt[e])
		if(ylim.was.not.set) ylim <- NA
	}
	
	if(main) mtext("Data and Threshold",outer=T,col=1,line=-0.5)
	
}

#######################################################
"qump_lrt"<-
function(l.pp,mod1,mod2)
{
	source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_pp_reference.R",TRUE))
	ne   = length(l.pp)
	mods = names(e.pp[[1]])

	for(e in 1:ne) {
		z1  = l.pp[[e]][[mod1]]
		z2  = l.pp[[e]][[mod2]]
		lrt = 2*(z1$nllh-z2$nllh)
		tname = paste(mod2,mod1,sep='')
		l1 = list(lrt)
		names(l1) = tname
		if (is.null(l.pp[[e]][['lrt']])) l.pp[[e]]$lrt=l1
		else {
			l0 = l.pp[[e]][['lrt']]
			l0 = c(l0,l1)
			l.pp[[e]]$lrt = l0
		}
		
	}
	
	l.pp
}
#######################################################
#######################################################
#""<-
#function(l.pp, imodel)
#{
#	source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_pp_reference.R",TRUE))
#	ne <- length(l.pp)
#	for(e in 1:ne) {
#		z <- l.pp[[e]][[imodel]]
#		
#	}
#}
#######################################################



###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
#""<-
#function()
#{
#	
#}
#######################################################
#######################################################
# version b below here
#######################################################
"read_line_split_files_rcm17b"<-
function(
	wave_ycoord,
	expt.list,
	data.dir = '/project/extremes1/qump/rcm_europe/daily/lines/temperature_at_1.5m_________________tmax/xxx',
	pat = '03236_8192_yxxx',
	years = 1950:2099,
	idx = NA,
	multiyear = TRUE
	)
{

cat("CAUTION - YCOORD IN WAVE UNITS PLEASE",cr)

#sjb#library(date)

source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_util.R",TRUE))
source(system("echo /net/home/h03/hadsx/extremes/R/libs/pp_io.R",TRUE))

nexpt      <- length(expt.list)
l.r        <- rep(list(vector(mode='list',length=2)),nexpt)
names(l.r) <- expt.list
for (i in 1:nexpt) {
	names(l.r[[i]]) <- c('data','date')
}

date.r <- vector(mode='list',length=nexpt)
names(l.r) <- expt.list
names(date.r) <- expt.list

for (e in 1:nexpt) {
		path.to.search <- sub("xxx",expt.list[e],data.dir)
		buf1           <- '000'
		sty            <- paste(wave_ycoord)
		buf2           <- paste(sprintf(paste("%.",nchar(buf1)-nchar(sty),"s",sep=""),buf1),sty,sep='')
		pat2           <- sub("xxx",buf2,pat)
		st.all         <- dir(path=path.to.search, pattern=pat2,full.names=TRUE)
		i.e            <- grep(expt.list[e],st.all)
		st.all         <- st.all[i.e]
		# temp patch
		st.all         <- st.all[1]
		
		if(multiyear) {  # ie the file is multi-year

			st.all <- st.all
			nf     <- length(st.all)
			# here we read a row and then select the point later
			for (f in 1:nf) {

				pp1   <- read.row.pp(st.all[f],irow=1,idxfields=idx)
				lbnpt <- pp1$lbnpt[1]
				lbrow <- pp1$lbrow[1]
				npy   <- pp1$nfields[1]

				if (f == 1) {
					d0.r <- array(0.0,dim=c(lbnpt,npy*nf))
					d0.r[1:lbnpt,1:npy] <- pp1$data
					yr.r <- pp1$lbyr
					mn.r <- pp1$lbmon
					dy.r <- pp1$lbdat
				} else {
					a                 <- (f-1)*npy+1
					b                 <- (f)*npy
					d0.r[1:lbnpt,a:b] <- pp1$data
					yr.r <- c(yr.r,pp1$lbyr)
					mn.r <- c(mn.r,pp1$lbmon)
					dy.r <- c(dy.r,pp1$lbdat)
				}
			}

		} else { # years are in different files
			cat('NOT SUPPORTED',cr)
		} # end else
		
		# 04/11/10 Strictly speaking this is the bad way to do it and shoudl use Modeldate class instead and mdy.date.model but I have used this for so long and it works I shall leave it.
		jdate.r <- mdy.date(mn.r, dy.r, yr.r)
		# deal with feb29 & feb30
		f29          <- which(mn.r==2 & dy.r==29)
		f29m1        <- f29-1    
		jdate.r[f29] <- jdate.r[f29m1]+1

		f30          <- which(mn.r==2 & dy.r==30)
		f30m2        <- f30-2    
		jdate.r[f30] <- jdate.r[f30m2]+2
		
		isort <- sort(as.real(jdate.r),index.return=T)$ix

		d0.r        <- d0.r[,isort]
		jdate.r     <- jdate.r[isort]
		
		yr.r2       <- date.mdy(jdate.r)$year
		iy          <- findall(years,yr.r2)
		
		l.r[[e]][['data']] <- d0.r[,iy]
		l.r[[e]][['date']] <- jdate.r[iy]
}
		
	l.r
}

#######################################################
#######################################################
"plot_data_w_thresholdb"<-
function(l1, th, i.t, xcoord, expt, nrow=6, ncol=3, newplot=TRUE, ylim=NA, cex=0.25, ann=F, main=T)
{
	#source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_pp_checked.R",TRUE))
	
	#iti<-list_2d_2_array(i.t) due to data dropouts cannot use this need to keep as list
	
	if(newplot) x11()
	par(mfcol=c(nrow,ncol))
	par(mar=c(2,1,2,1),oma=c(1,1,2,1), ann=ann)
	
	if(missing(expt)) ne <- length(l1)
	else              ne <- length(expt)
	
	for(e in 1:ne) {

		if(is.na(ylim[1])) {
			ylim <- range(l1[[e]][['data']][xcoord,i.t[[e]]],na.rm=TRUE)
			ylim.was.not.set <- TRUE
		} else ylim.was.not.set <- FALSE

		 plot(l1[[e]][['date']][i.t[[e]]], l1[[e]][['data']][xcoord,i.t[[e]]], ylim=ylim,pch=33)
		lines(l1[[e]][['date']][i.t[[e]]], th[[e]],col=2)
		if(is.character(expt[e])) title(expt[e])
		if(ylim.was.not.set) ylim <- NA
	}
	
	if(main) mtext("Data and Threshold",outer=T,col=1,line=-0.5)
	
}
#######################################################
#######################################################
"plot_exceedanceb"<-
function(l1, th, i.t, xcoord, expt, nrow=6, ncol=3, newplot=TRUE, ylim=NA, cex=0.25, ann=F, main=T)
{
	#source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_pp_checked.R",TRUE))
	
	#iti<-list_2d_2_array(i.t) due to data dropouts cannot use this need to keep as list
	
	if(newplot) x11()
	par(mfcol=c(nrow,ncol))
	par(mar=c(2,1,2,1),oma=c(1,1,2,1), ann=ann)
	
	if(missing(expt)) ne <- length(l1)
	else              ne <- length(expt)
	
	for(e in 1:ne) {

		d1 <- l1[[e]][['data']][xcoord,i.t[[e]]] 
		i1 <- d1>th[[e]]
		d1 <- l1[[e]][['data']][xcoord,i.t[[e]]] - th[[e]]

		if(is.na(ylim)) {
			ylim <- range(d1[i1],na.rm=TRUE)
			ylim.was.not.set <- TRUE
		} else ylim.was.not.set <- FALSE
		
		 plot(l1[[e]][['date']][i.t[[e]]][i1], d1[i1], ylim=ylim,cex=0.3)
		if(is.character(expt[e])) title(expt[e])
		if(ylim.was.not.set) ylim <- NA
	}
	
	if(main) mtext("Data and Threshold",outer=T,col=1,line=-0.5)
	
}
#######################################################
#######################################################
"plot_globalt_threshold"<-
function(l1, gt, th, i.t, xcoord, expt, nrow=6, ncol=3, newplot=TRUE, diff=F, ylim=NA, cex=0.25, ann=F, main=T)
{
	#source(system("echo /net/home/h03/hadsx/extremes/R/libs/simon_pp_checked.R",TRUE))
	
	#iti<-list_2d_2_array(i.t) due to data dropouts cannot use this need to keep as list
	
	if(newplot) x11()
	par(mfcol=c(nrow,ncol))
	par(mar=c(2,1,2,1),oma=c(1,1,2,1), ann=ann)
	
	if(missing(expt)) ne <- length(l1)
	else              ne <- length(expt)
	
	for(e in 1:ne) {

		d1 <- gt[[e]][i.t[[e]]]
		d2 <- th[[e]] - th[[e]][1]
		
		if(diff) {
		
			d1 <- d1-d2
			if(is.na(ylim)) {
				ylim <- range(d1,na.rm=TRUE)
				ylim.was.not.set <- TRUE
			} else ylim.was.not.set <- FALSE

			  plot(l1[[e]][['date']][i.t[[e]]], d1, type='l',ylim=ylim,cex=0.3)
			if(is.character(expt[e])) title(expt[e])
			if(ylim.was.not.set) ylim <- NA

		} else {

			if(is.na(ylim)) {
				ylim <- range(c(d1,d2),na.rm=TRUE)
				ylim.was.not.set <- TRUE
			} else ylim.was.not.set <- FALSE

			  plot(l1[[e]][['date']][i.t[[e]]], d1, type='l',ylim=ylim,cex=0.3)
			 lines(l1[[e]][['date']][i.t[[e]]], d2, col=3)
			if(is.character(expt[e])) title(expt[e])
			if(ylim.was.not.set) ylim <- NA

		}
	}
	
	if(main) mtext("Data and Threshold",outer=T,col=1,line=-0.5)
	
}
#######################################################
#######################################################
"zero_histogram_equaliseb"<-
function(li, show.plot=FALSE)
{
	flag <- 0
	d1   <- d0
	d1.n <- trunc(min(d1,na.rm=TRUE))
	d1.x <- trunc(max(d1,na.rm=TRUE))+1
	if(d1.n<272 & d1.x>274) {	# data spans zero
		d1.h  <- hist(d1,breaks=(d1.n:d1.x),plot=FALSE)		
		s1    <- sort(d1.h$counts,decreasing = TRUE,index.return=TRUE)
		c.272 <- d1.h$counts[which(d1.h$breaks==272)]
		c.273 <- d1.h$counts[which(d1.h$breaks==273)]
		c.274 <- d1.h$counts[which(d1.h$breaks==274)]

		if ( !( (c.273>c.272 & c.273<c.274) || (c.273<c.272 & c.273>c.274)) ) {
			c.273.old <- c.273
			c.273     <- trunc((c.272+c.274)/2.0)
			i <- (1:length(d1))[(d1>=273.0)&(d1<274.0)]
			if (all(is.finite(c(i, c.273, s1$x[1],c.273.old)))) {
				if((c.273.old-c.273) >0) {
					s     <- sample(i,(c.273.old-c.273),replace=FALSE)
					d1[s] <- NA
					flag <- 1
				}
			} else {
				d1[which(d1>272.9 & d1<273.5)]         <- NA
				cat("PROBLEM IN HISTOGRAM EQUALISATION ",cr)
			}
		#d1b.h  <- hist(d1[[e]],breaks=(d1.n:d1.x),plot=TRUE)
		#abline(v=273.16,col=2)
		#c.272 <- d1b.h$counts[which(d1.h$breaks==272)]
		#c.273 <- d1b.h$counts[which(d1.h$breaks==273)]
		#c.274 <- d1b.h$counts[which(d1.h$breaks==274)]
		#cat("counts 3 ",c.272,c.273,c.274,cr)
		#readline("Continue? post histo")				
		}
	}

	if(show.plot) {
		x11()
		par(mfcol=c(2,1))
		d1.h <- hist(d0,breaks=(d1.n:d1.x),plot=TRUE)
		abline(v=273.16,col=2)
		d1.h <- hist(d1,breaks=(d1.n:d1.x),plot=TRUE)
		abline(v=273.16,col=2)
	}

	list(data=d1,equalised.flag=flag)
}
#######################################################
#######################################################
"pp_prediction1_return_level"<-
function(pp1,rp,covar=0.0,imodel=F)
{

if(dim(pp1$data)[3] != 34) {
	cat('Input pp structure has wrong number of fiedls.  Exiting.',cr)
	exit(-1)
}
ppout <- clone.pp(pp1,1)

#######################################################
		i.nllh 		= 	1
		i.conv 		= 	2
		i.gof.stat1 = 	3
		i.gof.stat2 = 	4
		i.gof.stat3 = 	5
		i.gof.count1 = 	6
		i.gof.count2 = 	7
		i.gof.count3 = 	8
		i.nboot 	= 	9
		i.npy 		= 	10
		i.loc 		= 	11
		i.loc2		= 	12
		i.loct 		= 	13
		i.sca 		= 	14
		i.sca2		= 	15
		i.scat 		= 	16
		i.shp 		= 	17
		i.shp2		= 	18
		i.shpt 		= 	19
		i.se.loc 	= 	20
		i.se.loc2	= 	21
		i.se.loct 	= 	22
		i.se.sca 	= 	23
		i.se.sca2	= 	24
		i.se.scat 	= 	25
		i.se.shp 	= 	26
		i.se.shp2	= 	27
		i.se.shpt	= 	28


		for(y in 1:pp1$lbrow[1]) {
			for(x in 1:pp1$lbnpt[1]) {
				if( pp1$data[x,y,i.loc] != pp1$bmdi[1]) {
				
				if( pp1$data[x,y,i.loc]  != pp1$bmdi[1]) m0 <- pp1$data[x,y,i.loc]  else  m0 <- 0
				if( pp1$data[x,y,i.loc2] != pp1$bmdi[1]) m2 <- pp1$data[x,y,i.loc2] else  m2 <- 0
				if( pp1$data[x,y,i.loct] != pp1$bmdi[1]) mt <- pp1$data[x,y,i.loct] else  mt <- 0
				if( pp1$data[x,y,i.sca]  != pp1$bmdi[1]) s0 <- pp1$data[x,y,i.sca]  else  s0 <- 0
				if( pp1$data[x,y,i.sca2] != pp1$bmdi[1]) s2 <- pp1$data[x,y,i.sca2] else  s2 <- 0
				if( pp1$data[x,y,i.scat] != pp1$bmdi[1]) st <- pp1$data[x,y,i.scat] else  st <- 0
				if( pp1$data[x,y,i.shp]  != pp1$bmdi[1]) g0 <- pp1$data[x,y,i.shp]  else  g0 <- 0
				npy  <- pp1$data[x,y,i.npy]
				#prp  <- 1-(1.0/(rp))
				prp1 <- 1-(1.0/(rp*npy))
				if(!imodel) {
					loc  = m0 + covar*mt
					sca  = exp(s0 + covar*st)
					shp  = g0
				} else {
					loc  = m0 + covar*mt + m2
					sca  = exp(s0 + covar*st + s2)
					shp  = g0
				}
				# convert to npy=1 ##################################
				delta = (1.0/npy)/(1.0/1.0)
				
				scab  = sca*delta^shp
				locb  = loc + (scab*(1-delta^(-1*shp)))/shp
				
				# calculate the return levels ################################
				ppout$data[x,y,1] = locb  +  scab*( (  1/(-1*log(prp1))   )^shp - 1 )/shp
				} else {
				ppout$data[x,y,1] = pp1$bmdi[1]
				}
			} # x
		} # y
ppout
}
#######################################################
