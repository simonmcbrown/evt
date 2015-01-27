# info
# index.for.max.in.block
# max.in.block
# nao.daily.by.season
# max.f
# min.f
# mean.f
# sd.f
# vflip  # flips an "image" in the vertical - for plotting pp fields
# find.threshold
# cov2corr
# nothing
#"findall"<-function(find.these,in.this)
#"findall.nearest"<-function(find.these,in.this)
# iquantile(x,prob=0.5)
# indexforcommon(x,y)

#source("/net/home/h03/hadsx/extremes/R/libs/simon_util.R")

library(methods)

#### temprorary
#real<-function(length=NULL)
#{
#double(length)
#}

###############################################################
## standard multi-panel plots
up.1<- function()	      
{
par(mfcol=c(1,1))
}
up.2<- function()	      
{
par(mfcol=c(2,1))
}
up.3<- function()	      
{
par(mfcol=c(3,1))
}
up.2by2<- function()	      
{
par(mfcol=c(2,2))
par(mar=c(3,3,2,1))
par(mgp=c(2,1,0))
}
up.2by3<- function()	      
{
par(mfrow=c(2,3))
par(mar=c(3,3,2,1))
par(mgp=c(2,1,0))
}
up.3by2<- function()	      
{
par(mfrow=c(3,2))
par(mar=c(3,3,2,1))
par(mgp=c(2,1,0))
}
up.3by3<- function()	      
{
par(mfcol=c(3,3))
par(mar=c(3,3,2,1))
par(mgp=c(2,1,0))
}
up.3by4<- function()	      
{
par(mfrow=c(3,4))
par(mar=c(3,3,2,1))
par(mgp=c(2,1,0))
}
up.4by3<- function()	      
{
par(mfrow=c(4,3))
par(mar=c(3,3,2,1))
par(mgp=c(2,1,0))
}
up.6by3<- function()	      
{
par(mfcol=c(6,3))
par(mar=c(3,3,2,1))
par(mgp=c(2,1,0))
}
up.3by6<- function()	      
{
par(mfrow=c(3,6))
par(mar=c(3,3,2,1))
par(mgp=c(2,1,0))
}

a4p2x2 <- function(scale=8)
{
x11(width=scale,height=scale*sqrt(2))
up.2by2()
}

a4p1x2 <- function(scale=8)
{
x11(width=scale,height=scale*sqrt(2))
up.2()
}

a4p2x3 <- function(scale=8)
{
x11(width=scale,height=scale*sqrt(2))
up.3by2()
}

extend.range<- function(x)	      
{
# does celing and floor version of range()
lo <- floor(min(x,na.rm=T))
hi <- ceiling(max(x,na.rm=T))
a  <- c(lo,hi)
a
}

finite<-function(x)
{
  y <- x[is.finite(x)]
  y
}

#######################################################
"get_seasonal_maxima_index"<-function(x1,jdates1,months,getmax=T,VERBOSE=TRUE)
{
if(VERBOSE) cat('get_seasonal_maxima: months must be in order eg 11,12,1,2',cr)

if( (class(jdates1) == "Modeldate")) {
	mdy1  <- date.mdy.model(jdates1)
	iyall <- c(unique(mdy1$year),max(unique(mdy1$year))+1)
	iseasmax <- NULL
	for (y in iyall) {
		if(length(findall(c(12,1),months)) >1) { #season spanns December and January
			iy <- which( (jdates1 >= mdy.date.model(months[1],1,y-1)) & (jdates1 < mdy.date.model(months[length(months)]+1,1,y)))
			if(getmax) iseasmax <- c(iseasmax,iy[which.max(x1[iy])])
			else       iseasmax <- c(iseasmax,iy[which.min(x1[iy])])
		} else {
			iy <- which( (jdates1 >= mdy.date.model(months[1],1,y)) & (jdates1 < mdy.date.model(months[length(months)]+1,1,y)))
			if(getmax) iseasmax <- c(iseasmax,iy[which.max(x1[iy])])
			else       iseasmax <- c(iseasmax,iy[which.min(x1[iy])])
		}
	}

} else { # non-model dates

	mdy1  <- date.mdy(jdates1)
	iyall <- c(unique(mdy1$year),max(unique(mdy1$year))+1)
	iseasmax <- NULL
	for (y in iyall) {
		if(length(findall(c(12,1),months)) >1) { #season spanns December and January
			iy <- which( (jdates1 >= mdy.date(months[1],1,y-1)) & (jdates1 < mdy.date(months[length(months)]+1,1,y)))
			if(getmax) iseasmax <- c(iseasmax,iy[which.max(x1[iy])])
			else       iseasmax <- c(iseasmax,iy[which.min(x1[iy])])
		} else {
			iy <- which( (jdates1 >= mdy.date(months[1],1,y)) & (jdates1 < mdy.date(months[length(months)]+1,1,y)))
			if(getmax) iseasmax <- c(iseasmax,iy[which.max(x1[iy])])
			else       iseasmax <- c(iseasmax,iy[which.min(x1[iy])])
		}
	}

}
iseasmax
}

#######################################################
"index.for.max.in.block"<-function(x1,block.size)
{
	if (block.size>1) {
		n  <- length(x1)
		n2 <- trunc(n/block.size)
		rem <- n %% block.size
		#if ( (n2+1)*block.size > length(x1) ) n2 <- n2-1
		if (rem > 0) {

			i1 <- double(n2+1)
			for (j in 0:(n2-1)) {
				i1.t     <- which.max(x1[(j*block.size+1):(j*block.size+block.size)]) 
				if (length(i1.t) < 1) i1.t <- 1
				i1[j+1]  <- (j*block.size)+i1.t
			}
			i1.t     <- which.max(x1[(n2*block.size+1):length(x1)]) 
			if (length(i1.t) < 1) i1.t <- 1
			i1[n2+1] <- (n2*block.size)+i1.t

		} else {
			i1 <- double(n2)
			for (j in 0:(n2-1)) {
				i1.t     <- which.max(x1[(j*block.size+1):(j*block.size+block.size)]) 
				if (length(i1.t) < 1) i1.t <- 1
				i1[j+1]  <- (j*block.size)+i1.t
			}
		}

	} else {
	i1 <- 1:length(x1)
	}

i1
}


#######################################################
"max.in.block"<-function(x1,block.size)
{

	ix1 <- index.for.max.in.block(x1,block.size)
	e1  <- x1[ix1]

e1
}
#######################################################
"decluster.sjb"<-function(x1,block.size,sepperation=1)
{
# this basically does max in block but then ensures that there are no two values within sepperation of each other.
	n    <- length(x1)
	e1   <- x1
	m1   <- min(x1,na.rm=TRUE)
	e1[] <- m1
	ix1  <- index.for.max.in.block(x1,block.size)

	e1[ix1] <- x1[ix1]
	
	for (i in 1:(n-sepperation)) {
		c1 <- length(which(e1[i:(i+sepperation)] > m1))
		if (c1 > 1) {
			i2 <- which.max(e1[i:(i+sepperation)])
			x2 <- e1[i:(i+sepperation)][i2]
			e1[i:(i+sepperation)] <- m1
			e1[i:(i+sepperation)][i2] <- x2
		}
	}

e1
}
#######################################################
"find.maxima.1d"<-function(a1,block.size,sepperation=1,smooth=0.5,verbose=FALSE)
{
	# although more fancy than decluster.sjb it is much slower and is not obviously producing better results
	
	n          = length(a1)
	mx1        = double(n)
	
	# fit spline, differentiate, find zero crossings +ve to -ve
	#for (i in 1:(n-block.size)) {
	for (i in seq(from=1,to=(n-block.size),by=(sepperation-1)) ) {
		y     = a1[i:(i+block.size-1)]
		sp1   = smooth.spline(1:length(y),y,spar=smooth)
		sp1.p = predict(sp1,1:length(y))
		
		
		diff1 = c(-1,diff(sp1.p$y))
		
		sp2   = smooth.spline(1:length(y),diff1,spar=smooth)
		sp2.p = predict(sp2,0.1*(1:(10*length(y))))
		
		r = double(length(sp2.p))
		
		for(j in 2:length(sp2.p$y)) r[j] = ( (sp2.p$y[j-1]>0) & (sp2.p$y[j]<0) )
		i.sp2.max = which(r == 1)
		i.x.max   = round(sp2.p$x[i.sp2.max])
		
		if(length(i.x.max) > 1) {
			#cat('Two maxs',i,cr)
			i2 = which.max(y[i.x.max])
			i.x.max = i.x.max[i2]
		}
		
		if(is.finite(i.x.max[1])) {
			mx1[i] = i+i.x.max-1
			#cat('Max at ',mx1[i],cr)
		}
		i.x.max = NA
	}
	if(verbose) cat('Zero crossings found',cr)
	
	# find unique maxima
	mx2 = unique(mx1)
	mx2 = mx2[2:length(mx2)]	# 2 gets rid of the 0s

	# search the input data for the real max near the spline data
	d3   = double(length(a1))
	d3[] = NA
	for (i in 1:length(mx2)) {
		if( ((mx2[i]-block.size/2) > 0) && ((mx2[i]+block.size/2) < n) ) {
			i2 <- which.max(a1[(mx2[i]-block.size/2):(mx2[i]+block.size/2)])
			if(is.finite(i2[1])) d3[(mx2[i]-block.size/2)+i2-1] = a1[(mx2[i]-block.size/2)+i2-1]
		}
	}
	if(verbose) cat('Maxima at zero crossings found',cr)

	# ensure that the maxima are not too close
	# minor flaw in this is that can get successive removal of maxima when removing
	# one in the middle would have sufficed.
	for (i in 1:(n-sepperation)) {
		#c1 <- length(which(d3[i:(i+sepperation)] > min(d3,na.rm=T)))
		c1 <- length(which(!is.na(d3[i:(i+sepperation)])))
		if (c1 > 1) {
			i2 <- which.max(d3[i:(i+sepperation)])
			if(is.finite(i2[1])) {
				x2 <- d3[i:(i+sepperation)][i2]
				d3[i:(i+sepperation)]     <- NA
				d3[i:(i+sepperation)][i2] <- x2
			}
		}
	
	}
	if(verbose) cat('Maxima are not too close',cr)

# final check. Ensure points either side are smaller than the max

	imx = which(!is.na(d3))
	for (i in 1:length(imx)) {if(which.max(a1[(imx[i]-1):(imx[i]+1)]) != 2) imx[i] = NA	}
	imx = imx[!is.na(imx)]

	d3 = double(length(a1))
	d3[] = NA
	d3[imx] = a1[imx]
	
d3

}
#######################################################
"lsp"<-function(x)
{
ls(env=parent.env(environment()),pat=as.character(x))
}
#######################################################
"parent"<-function(x)
{
#parent.env(parent)
parent.env(environment())
}
#######################################################
"str1"<-function(x)
{
str(x,max=1)
}
#######################################################
"str2"<-function(x)
{
str(x,max=2)
}
#######################################################
"max.f"<-function(x)
{
max(x,na.rm=TRUE)
}
#######################################################
"min.f"<-function(x)
{
min(x,na.rm=TRUE)
}
#######################################################
"mean.f"<-function(x)
{
mean(x,na.rm=TRUE)
}
#######################################################
"sd.f"<-function(x)
{
sd(x,na.rm=TRUE)
}
#######################################################
"nao.daily.by.season"<-function(nao.seas,jdate,years,iseas)
{
	#### 02/04/07 attempt at constructing seasonal NAO at daily resolution
	jdate.r <- as.double(jdate)
	nao     <- double(length(jdate))
	i7      <- findInterval(years,nao.seas$year)
	if (iseas <= 4) {
		nao[]   <- nao.seas[[(iseas+1)]][i7]
	} else {
		nao[]   <- nao.seas[[6]][i7]
	}

	if (iseas==1) {
	years2 <- unique(years)
	for (y in years2[1]:years2[length(years2)-1]) {
		y1 <- as.double(mdy.date(12,1,(y)))
		y2 <- as.double(mdy.date(12,31,(y)))
		i3 <- which((jdate.r>=y1) & (jdate.r<=y2))
		i4 <- which(jdate==mdy.date(1,1,(y+1)))
		nao[i3] <- nao[i4]
		}
	}

	nao.sm  <- nao
	nao.sm  <- nao.sm-mean(nao.sm,na.rm=TRUE)
	nao.sm  <- nao.sm/sd(nao.sm,na.rm=TRUE) # standardise so that sd = 1

	nao.sm
}
#######################################################
"zero.histogram.equalise"<-function(d1,do.plot=FALSE,kelvin=TRUE)
{			# remove temps near zero deg C (and mdi)
	d1.n <- trunc(min(d1,na.rm=TRUE))-1
	d1.x <- trunc(max(d1,na.rm=TRUE))+1
	
	if (kelvin) {
		t272 <- 272
		t273 <- 273
		t274 <- 274
	} else {
		t272 <- -1
		t273 <- 0
		t274 <- 1
	}

	if(d1.n<t272 & d1.x>t274) {
		d1.h <- hist(d1,breaks=(d1.n:d1.x),plot=FALSE)
		s1 <- sort(d1.h$counts,decreasing = TRUE,index.return=TRUE)

		c.272 <- d1.h$counts[which(d1.h$breaks==t272)]
		c.273 <- d1.h$counts[which(d1.h$breaks==t273)]
		c.274 <- d1.h$counts[which(d1.h$breaks==t274)]

		if ( !( (c.273>c.272 & c.273<c.274) || (c.273<c.272 & c.273>c.274)) ) {
			c.273 <- trunc((c.272+c.274)/2.0)
			i     <- (1:length(d1))[(d1>=t273)&(d1<t274)]
			d1b   <- d1
			if (!any(c(i, c.273, s1$x[1]))) {
				s <- sample(i,(s1$x[1]-c.273),replace=FALSE)
				d1b[s] <- NA
			} else {
				d1b[which( d1>(t273-0.25) & d1<(t273+0.25) )]         <- NA
				#cat("no deal ",cr)
			}
			if (do.plot) {
				par(mfcol=c(2,1))				
				d1.h <- hist(d1,breaks=(d1.n:d1.x),plot=TRUE)
				d1b.h  <- hist(d1b,breaks=(d1.n:d1.x),plot=TRUE)
				readline("zero.histogram.equalise finnished. Continue? ")
			}
		} else {
			if (do.plot) {
				d1.h <- hist(d1,breaks=(d1.n:d1.x),plot=TRUE)
				readline("zero.histogram.equalise nothing to equalise 1. Continue? ")
			}
			d1b <- d1			
		}
	} else {
		if (do.plot) {
			d1.h <- hist(d1,breaks=(d1.n:d1.x),plot=TRUE)
			readline("zero.histogram.equalise nothing to equalise 2. Continue? ")
		}
		d1b <- d1
	}
	d1b
}
#######################################################
"find.threshold"<-function(x, threshold.fraction, UPPER=TRUE, STATIONARY=TRUE, bias.step=0.1, i.max = 200)
{
if(UPPER) {
	if(STATIONARY) {
		th        <- max(x,na.rm=T)-bias.step
		nex       <- length(which(x>th))
		i.count   <- 1
		while ( ((nex/length(x)) < threshold.fraction) && (i.count < i.max) ) {
			th      <- th - bias.step
			nex     <- length(which(x>th))
			i.count <- i.count + 1
		}

	} else {
		th     <- array(data=0.0,dim=c(length(x)))
		ix.max <- which.max(x)
		x.time <- (1:length(x))/length(x)
		df     <- data.frame(y1=x,x1=x.time)
		linreg <- lm(y1 ~ x1,data=df)

		# threshold set so that faction of data = threshold.fraction
		th.bias <- x[ix.max]-(linreg$coef[1] + linreg$coef[2]*x.time)[ix.max]
		th      <- th.bias + linreg$coef[1] + linreg$coef[2]*x.time
		nex     <- length(which(x>th))
		i.count <- 1
		while ( ((nex/length(x)) < threshold.fraction) && (i.count < i.max) ) {
			th      <- th - bias.step
			nex     <- length(which(x>th))
			i.count <- i.count + 1
		}

	}

} else {	# ie lower tail threshold
	if(STATIONARY) {
		th        <- min(x,na.rm=T)+bias.step
		nex       <- length(which(x<th))
		i.count   <- 1
		while ( ((nex/length(x)) < threshold.fraction) && (i.count < i.max) ) {
			th      <- th + bias.step
			nex     <- length(which(x<th))
			i.count <- i.count + 1
		}

	} else {
		th     <- array(data=0.0,dim=c(length(x)))
		ix.min <- which.min(x)
		x.time <- (1:length(x))/length(x)
		df     <- data.frame(y1=x,x1=x.time)
		linreg <- lm(y1 ~ x1,data=df)

		# threshold set so that faction of data = threshold.fraction
		th.bias <- x[ix.min]+(linreg$coef[1] + linreg$coef[2]*x.time)[ix.min]
		th      <- th.bias + linreg$coef[1] + linreg$coef[2]*x.time
		nex     <- length(which(x<th))
		i.count <- 1
		while ( ((nex/length(x)) < threshold.fraction) && (i.count < i.max) ) {
			th      <- th + bias.step
			nex     <- length(which(x<th))
			i.count <- i.count + 1
		}

	}

}


th
}
#######################################################
"cov2corr"<-function(x)
{
d<-diag(x)
x/sqrt(d%o%d)
}
#######################################################
"vflip"<-function(x)
{
dims  <- dim(x)
if (length(dims) != 2) return(NA)
temp1 <- x
for (i in 1:dims[2]) x[,i] <- temp1[,(dims[2]+1)-i]
return(x)
}
#######################################################
#######################################################
"get_index_for_months_in_season"<-
function(
	time1, 
	season
	)
{
	if( !(class(time1) == "date")) {
		cat("get_index_for_months_in_season: input date not class date",cr)
		id <- NA
	} else {
		mnth <- date.mdy(time1)$month
		iall <- NULL
		for (m in 1:length(season)) {
			i1   <- which(mnth == season[m])
			iall <- c(iall,i1)
		}
		s1    <- time1[iall]
		siall <- sort(as.double(s1),index.return=TRUE)$ix
		id    <- iall[siall]
		
	}
	
	id
}
#######################################################
#######################################################
"mdy.date.model"<-function(month1,day1,year1)
{
	# I do not understand classes yet
	#setClass("MODate.j", representation("Date", day0="list", julian="numeric", calander360='logical'),
	#	prototype=list(day0=list(year=1960,month=1,day=1), julian=0,calander360=TRUE))
	#
	#setClass("MODate.ymd", representation("Date", day0="list",year="numeric",month="numeric",day="numeric"),
	#	prototype=list(day0=list(year=1960,month=1,day=1), year=1960,month=1,day=1))

	
	ref1 <- 1960*360 + (1-1)*30 + (1-1)
	model.date <- year1*360 + (month1-1)*30 + (day1-1)
	model.date <- model.date - ref1

	class(model.date) <- "Modeldate"
	return(model.date)
}

#######################################################
#######################################################
"date.mdy.model"<-function(jdate.model,force=FALSE)
{
	# input a mdy.date.model() derived julian date 
	if( (!(class(jdate.model) == "Modeldate")) && (!force) ) {
		cat("date.mdy.model: input date not class Modeldate",cr)
		l.model <- NA
	} else {
		ref1    = 1960*360 + (1-1)*30 + (1-1)
		
		y.model = trunc((jdate.model +ref1)/360)
		
		m.model = trunc((jdate.model +ref1 -360*y.model)/30) +1
		
		d.model = jdate.model +ref1 -360*y.model - 30*(m.model-1) +1
		
		# 4/11/10 changed lables to myear, mmonth, mday to ensure model julian dates are not mixed with real julian dates
		# list order now matches date.mdy is m then d then y
		l.model = list(mmonth=m.model, mday=d.model, myear=y.model)
		
		class(l.model) <- "Modeldate"
	}
	return(l.model)

}

#######################################################
#######################################################
"get_index_for_months_in_season_model"<-
function(
	time1, 
	season
	)
{	
	if( !(class(time1) == "Modeldate")) {
		cat("get_index_for_months_in_season_model: input date not class Modeldate",cr)
		id <- NA
	} else {
		mnth <- date.mdy.model(time1)$mmonth
		iall <- NULL
		for (m in 1:length(season)) {
			i1   <- which(mnth == season[m])
			iall <- c(iall,i1)
		}
		s1    <- time1[iall]
		siall <- sort(as.double(s1),index.return=TRUE)$ix
		id    <- iall[siall]
	}
	
	id
}

#######################################################
nothing <- function(x) {
	x
}

#######################################################
tidy <- function() {
	graphics.off()
}

#######################################################
"info"<-function(a)
{
# recursivly gives attributes of a


 
t <- mode(a)
n <- length(a)

if (t!="list")
	{
	print("This is a ")
	print(mode(a))
	print(length(a))
	}
else
	{
	print(c("This is a ",mode(a)," of ",length(a)),quote=FALSE)
	print(c("Names: ",names(a)))
	l <- names(a)
	
	for (i in 1:n)
		{
#		print(l[i])
		b <- a[[i]]
		t2 <- mode(b)
		n2 <- length(b)
		if (t2!="list")
			{
			print(c(l[i],mode(b),length(b)),quote=FALSE)
			}
		else
			{
			print(c(l[i],mode(b),length(b)),quote=FALSE)
			print(names(b))
			l2 <- names(b)
			for (i2 in 1:n2)
				{
#				print(l2[i])
				c <- b[[i2]]
				t3 <- mode(c)
				n3 <- length(c)
				if (t3!="list")
					{
					print(c(l2[i],mode(c),length(c)),quote=FALSE)
					}
				else
					{
					print("list larger than 3 levels deep")
					print(c(l2[i],mode(c),length(c)),quote=FALSE)
					}
				}
			}

		}
	}

}
#######################################################
"findall"<-function(find.these,in.this)
{
	n1 <- length(find.these)
	l.all <- vector(mode='list',length=n1)
	names(l.all) <- rep('index',n1)
	for (i in 1:n1) {
		i1 <- which(find.these[i]==in.this)
		l.all[[i]] <- i1
	}
	 i.all<-unlist(sapply(l.all, "["))

	 i.all
}
#######################################################
"indexforcommon"<-function(array1,array2)
{
  n1    <- length(array1)
  l1.all <- vector(mode='list',length=n1)
  l2.all <- vector(mode='list',length=n1)
  for(i in 1:n1) {
    ix <- which(array2 == array1[i])
    #browser()
    if (length(ix) > 0) {
      if (ix[1] > 0) {
        l1.all[[i]] <- i
        l2.all[[i]] <- ix
      } else {
        l1.all[[i]] <- NA
        l2.all[[i]] <- NA
      }
    }
  }
  i1.all <- unlist(sapply(l1.all, "["))
  i2.all <- unlist(sapply(l2.all, "["))
  
  cbind(i1.all,i2.all)
}
#######################################################
"notfound"<-function(dontfind.these,in.this)
{

	"%out%" <- function(x, table) match(x, table, nomatch = 0) == 0

	n          <- length(in.this)
	i.notfound <- (1:n)[in.this %out% dontfind.these]
	i.notfound
}

#######################################################
"findall.nearest"<-function(find.these,in.this)
{
	if(min(in.this) < 0) cat("WARNING: DOES NOT WORK FOR NEGATIVE NUMBERS!",cr)
	s.in.this     = sort(in.this)
	n1           <- length(find.these)
	l.all        <- vector(mode='list',length=n1)
	names(l.all) <- rep('index',n1)
	for (i in 1:n1) {
		jset = which(find.these[i]<=s.in.this)[1]
		if (!is.finite(jset)) jset = length(s.in.this)
		jget = which(find.these[i]>=s.in.this)
		jget = jget[length(jget)]
		if (!is.finite(jget)) jget = 1

		d.jget = find.these[i]-s.in.this[jget]
		d.jset = s.in.this[jset]-find.these[i]

		#cat(i,find.these[i],s.in.this[jget],s.in.this[jset],cr)
		if (d.jget < d.jset) {
			#l.all[[i]] <- s.in.this[jget]
			l.all[[i]] <-jget
		} else {
			#l.all[[i]] <- s.in.this[jset]
			l.all[[i]] <- jset
		}

	}
	 i.all<-unlist(sapply(l.all, "["))

	 i.all
}
#######################################################
str1 <- function(x) {
	str(x,max=1)
}

str2 <- function(x) {
	str(x,max=2)
}

ll.sjb <- function(x, n=20) {
	if(missing(x)) l1<-ls(.GlobalEnv)
	ll<-NULL
	for (i in 1:length(l1)) ll[i]<-object.size(get(l1[i]))
	ix <- sort(ll,index=T)$ix
	len <- length(ix)
	ix <- ix[(len-n):len]
	for (i in ix) cat(ll[i],'\t\t',l1[i],cr)
}

iquantile <- function(x,prob=0.5) {
	ix0  <- 1:length(x)
	xf   <- x[is.finite(x)]
	ixf0 <- ix0[is.finite(x)]
	ixf1 <- sort(xf,ind=T)$ix
	iq1 <- findInterval(quantile(xf,prob=prob),xf[ixf1])
	ixf0[ixf1][iq1]
	
}
#######################################################
get_links <- function(z) {
	links <- substr(z$link,3,(nchar(z$link)-1))
	mulink  <- sprintf('%s',strsplit(links,', ')[[1]][1])
	siglink <- sprintf('%s',strsplit(links,', ')[[1]][2])
	shlink  <- sprintf('%s',strsplit(links,', ')[[1]][3])
	mul  <- z$model[[1]]
	sigl <- z$model[[2]]
	shl  <- z$model[[3]]
	npmu <- length(mul) + 1
	npsc <- length(sigl) + 1
	npsh <- length(shl) + 1

c(rep(mulink,npmu),rep(siglink,npsc),rep(shlink,npsh))

# would then use return value by
# untransformed_mle <- get(link[3])(mle[3])

}
#######################################################
######################################################################

get_ev_model <- function(z)
{
	links  <- get_links(z)
	model  <- z$model

	if (is.null(model[[1]])) {
		mulink <- "identity"
		n.mu   <- 1
		mul    <- NULL
	} else {
		n.mu   <- 1 + length(model[[1]])
		mulink <- links[1:n.mu][1]
		mul    <- model[[1]]
	}

	if (is.null(model[[2]])) {
		siglink <- "identity"
		n.sig   <- 1
		sigl    <- NULL
	} else {
		n.sig   <- 1 + length(model[[2]])
		siglink <- links[(n.mu+1):(n.mu+n.sig)][1]
		sigl    <- model[[2]]
	}

	if (is.null(model[[3]])) {
		shlink <- "identity"
		n.shp   <- 1
		shl    <- NULL
	} else {
		n.shp   <- 1 + length(model[[3]])
		shlink <- links[(n.mu+n.sig+1):(n.mu+n.sig+n.shp)][1]
		shl    <- model[[3]]
	}
	
	m <- list(mul=mul, mulink=mulink, sigl=sigl, siglink=siglink, shl=shl, shlink=shlink)
	
	m
}

######################################################################
ex.comp.1d <- function(z,comp)
{
  dims <- dim(z)
  ndim <- length(dims)

  if (ndim == 1) {
    d <- sapply(z,'[[',comp)
    
  } else if (ndim == 2) {
    d <- array(NA,dim=c(length(z[[1,1]][[comp]]), dims[1], dims[2]) )
    for(j in 1:dims[2]) for(i in 1:dims[1]) d[,i,j] <- z[[i,j]][[comp]]
    
  } else if (ndim == 3) {
    d <- array(NA,dim=c(length(z[[1,1]][[comp]], dims[1], dims[2], dims[3])))
    for(k in 1:dims[3]) for(j in 1:dims[2]) for(i in 1:dims[1]) d[,i,j,k] <- z[[i,j,k]][[comp]]

  } else if (ndim >= 4) {
    cat("ex.comp.1d: dimension of list must be <=3",cr)
  } 
  d
}

######################################################################
### add here some useful map and spherical trig fns
# Convert a latitude and longitude from one pole to another
pp.pole.to.pole<-function(lat,long,
                          from.pole.lat,from.pole.long,
                          to.pole.lat,to.pole.long) {
  if(is.na(lat) || is.na(long) ||
     is.na(from.pole.lat) || is.na(from.pole.long) ||
     is.na(to.pole.lat) || is.na(to.pole.long)) return(c(NA,NA))
  
  if(from.pole.lat==to.pole.lat &&
     from.pole.long==to.pole.long) return(c(lat,long))

  if(from.pole.lat==90 && from.pole.long==0) {
    return(pp.ll.to.rg(lat,long,to.pole.lat,to.pole.long))
  }

  standard.ll<-pp.rg.to.ll(lat,long,from.pole.lat,from.pole.long)
  if(to.pole.lat==90 && to.pole.long==0) return(standard.ll)

  return(pp.ll.to.rg(standard.ll[1],standard.ll[2],
                          to.pole.lat,to.pole.long))
}  

######################################################################
# Convert a latitude and longitude to the equivalents
#  with a rotated pole. Lat and long in degrees.
# (Formulae from UMDP S1)
pp.ll.to.rg<-function(lat,long,pole.lat,pole.long) {
   while(pole.long>180) pole.long<-pole.long-360
   l0<-pole.long+180
   dtr<-pi/180
   sin.pole.lat<-sin(pole.lat*dtr)
   cos.pole.lat<-cos(pole.lat*dtr)
   if(pole.lat<0) {
      sin.pole.lat<- -sin.pole.lat
      cos.pole.lat<- -cos.pole.lat
   }
   long<-long-l0
   while(long>  180) long<-long-360
   while(long< -180) long<-long+360
   
   lat.rotated<-asin(max(-1,min(1,-cos.pole.lat*
                                   cos(long*dtr)*
                                   cos(lat*dtr)+
                                   sin.pole.lat*
                                   sin(lat*dtr))))
                         
   long.rotated<-0
   if(cos(lat.rotated)>1.0e-6) {
       long.rotated<-acos(max(-1,min(1,(cos.pole.lat*
                                         sin(lat*dtr)+
                                         sin.pole.lat*
                                         cos(long*dtr)*
                                         cos(lat*dtr))/
                                         cos(lat.rotated))))
   }
   long.rotated<-long.rotated*sign(long)
   lat.rotated<-lat.rotated/dtr
   long.rotated<-long.rotated/dtr
   while(long.rotated>  180) long.rotated<-long.rotated-360
   while(long.rotated< -180) long.rotated<-long.rotated+360
   return(c(lat.rotated,long.rotated))
 }

######################################################################
# Convert a latitude and longitude from a rotated pole to
#  regular lat and long
pp.rg.to.ll<-function(lat,long,pole.lat,pole.long) {
   l0<-pole.long+180
   dtr<-pi/180
   sin.pole.lat<-sin(pole.lat*dtr)
   cos.pole.lat<-cos(pole.lat*dtr)
   if(pole.lat<0) {
      sin.pole.lat<- -sin.pole.lat
      cos.pole.lat<- -cos.pole.lat
   }
   while(long>  180) long<-long-360
   while(long< -180) long<-long+360
   
   lat.rotated<-asin(max(-1,min(1,cos.pole.lat*
                                  cos(long*dtr)*
                                  cos(lat*dtr)+
                                  sin.pole.lat*
                                  sin(lat*dtr))))

   long.rotated<-0
   if(cos(lat.rotated)>1.0e-6) {
       long.rotated<-acos(max(-1,min(1,(-cos.pole.lat*
                                         sin(lat*dtr)+
                                         sin.pole.lat*
                                         cos(long*dtr)*
                                         cos(lat*dtr))/
                                         cos(lat.rotated))))
   }
   long.rotated<-long.rotated*sign(long)
   long.rotated<-long.rotated+l0*dtr
   lat.rotated<-lat.rotated/dtr
   long.rotated<-long.rotated/dtr
   while(long.rotated>  180) long.rotated<-long.rotated-360
   while(long.rotated< -180) long.rotated<-long.rotated+360
   return(c(lat.rotated,long.rotated))
 }

######################################################################
# funtions needed from newer R versions
######################################################################
globalVariables <- function (names, package, add = TRUE) 
{
    .listFile <- ".__global__"
    .simplePackageName <- function(env) {
        if (exists(".packageName", envir = env, inherits = FALSE)) 
            get(".packageName", envir = env)
        else "(unknown package)"
    }
    if (missing(package)) {
        env <- topenv(parent.frame())
        package <- .simplePackageName(env)
    }
    else if (is.environment(package)) {
        env <- package
        package <- .simplePackageName(env)
    }
    else env <- asNamespace(package)
    if (exists(.listFile, envir = env, inherits = FALSE)) 
        current <- get(.listFile, envir = env)
    else current <- character()
    if (!missing(names)) {
        if (environmentIsLocked(env)) 
            stop(gettextf("The namespace for package \"%s\" is locked; no changes in the global variables list may be made.", 
                package))
        if (add) 
            current <- unique(c(current, names))
        else current <- names
        assign(.listFile, current, envir = env)
    }
    current
}

### paste0
#paste0 = function(..., collapse = NULL)
paste0 <- function(...) paste(...,sep="")

### angle360 - eg for winds return the wind direction from (sin_theta, cos_theta) pair
angle360 <- function(sin_theta, cos_theta)
{
  if(sin_theta >0) out <- 180*acos(cos_theta)/pi
  else             out <- 360 -180*acos(cos_theta)/pi

  out
}



