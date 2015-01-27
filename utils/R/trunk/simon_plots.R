# source("/home/h03/hadsx/extremes/R/libs/simon_plots.R")

### general plotting 

### 2013/04 added plotting routines from "Marc in the box" http://menugget.blogspot.co.uk/


## plots 2d contour plot image and lines
#  kde2dplot.2d

## block plot of a pp field
#  plot.pp.block
#"sjb_x_error_bars"<-

#######################################################
#"name of function"<-
#function(list of parameter)
#{
#
#}
#######################################################
###############################################################

library(fields)


###############################################################
## block plot of a pp field
plot.pp.block<- function(
    pp1,          # multi ppfield
    id=1,         # field to plot, default =1
    ncol=50,            # the number of colors to use - redundant?
    zlim=NULL,      # limits in z coordinates 
    nlevels=20,         # see option nlevels in contour 
    main=NULL,      # title for plot
    flip=TRUE,      # verticaly flip 
    col=NULL,
    breaks=NULL,
    lab.breaks=NULL,
    graphics.reset=FALSE,
    offset=0
    )
          
{

d1 <- pp1$data[,,id]
d1 <- drop(d1)

if(offset != 0) {
  d0 <- d1
  d1[ 1                     : (dim(d1)[1]-offset), ] <- d0[ (offset+1) : (dim(d1)[1]), ]
  d1[ (dim(d1)[1]-offset+1) : (dim(d1)[1]),        ] <- d0[ 1          : offset,       ]
}

npt <- pp1$lbnpt[id]
nrow <- pp1$lbrow[id]

i1 <- which(d1 == pp1$bmdi[id])
d1[i1] <- NA

if (is.null(col))  col <- tim.colors(nlevels)

if (!is.null(col)) nlevels <- length(col)

if (is.null(zlim)) {
  inmdi <- which(d1 != pp1$bmdi[id])
  absmax <- max(abs(d1[inmdi]),na.rm=TRUE)
  zlim  <- c(-1.0*absmax,absmax)
}

if(flip) {
  temp1 <- d1
  for (i in 1:nrow) d1[,i] <- temp1[,(nrow+1)-i]
}

if (!is.null(breaks)) {
  image.plot(d1, zlim=zlim, nlevel = nlevels, main=main, col=col, graphics.reset=graphics.reset, breaks=breaks, lab.breaks=lab.breaks)
} else {
  image.plot(d1, zlim=zlim, nlevel = nlevels, main=main, col=col, graphics.reset=graphics.reset)
}

usr <- par("usr")
clip(usr[1], -2, usr[3], usr[4])
}

#######################################################
### plots 2d contour plot image and lines
kde2dplot.2d <- function(
		d,				  	# a 2d density computed by kde2D
		ncol=50,          	# the number of colors to use 
		zlim=c(0,max(z)), 	# limits in z coordinates 
		nlevels=20,       	# see option nlevels in contour 
		theta=30,         	# see option theta in persp
		phi=30,           	# see option phi in persp
		xlab='x',           		# 
		ylab ='y',           		# 
		main=''           		# 
		)
		      
{
z   <- d$z
nrz <- nrow(z) 
ncz <- ncol(z) 

couleurs  <- tail(topo.colors(trunc(1.4 * ncol)),ncol) 
fcol      <- couleurs[trunc(z/zlim[2]*(ncol-1))+1] 
dim(fcol) <- c(nrz,ncz) 
fcol      <- fcol[-nrz,-ncz]

#par(mfrow=c(1,2),mar=c(0.5,0.5,0.5,0.5)) 
#persp(d,col=fcol,zlim=zlim,theta=theta,phi=phi,zlab="density") 

par(mar=c(4,4,4,2)) 
image(d,col=couleurs,main=main,xlab=xlab,ylab=ylab) 
contour(d,add=T,nlevels=nlevels) 
box() 
}


#######################################################
"sjb_x_error_bars"<-
function(x_vals,y_vals,...)
{
for(i in 1:length(x_vals)) {
	x <- rep(x_vals[i],2)
	lines(x,y_vals[i,],...)
}
}
#######################################################
"sjb_y_error_bars"<-
function(y_vals,x_vals,...)
{
for(i in 1:length(y_vals)) {
	y <- rep(y_vals[i],2)
	lines(x_vals[i,],y,...)
}
}
#######################################################
"get_pp_coords"<-
function(
	pp1,			  	# multi ppfield
	n=1,		
	...)
{
cat("NOT SURE THIS WORKS - get_pp_coords",cr)
	xdim <- pp1$lbnpt[1]
	ydim <- pp1$lbrow[1]
	
	xpixelsize <- 1.0/xdim
	ypixelsize <- 1.0/ydim
	
	xmagic_number = 0 # determined by the typical bias on the cursor
	ymagic_number = ypixelsize/10.0 # determined by the typical bias on the cursor

	xscreen <- -(xpixelsize)/2+(0:xdim)*((1.0+xpixelsize)/xdim) + xmagic_number
	yscreen <- -(ypixelsize)/2+(0:ydim)*((1.0+ypixelsize)/ydim) + ymagic_number
	
	ip <- locator(n, type = "p", pch = "+")
	
	xcoords <- ip$x				
	ycoords <- ip$y 
	
	ix <- findInterval(xcoords,xscreen)
	iy <- findInterval(ycoords,yscreen)

	#for(i in 1:length(xcoords)) cat(xcoords[i],ycoords[i],"\n")

	a <- c(ix,pp1$lbrow[1]-iy+1)
	dim(a) <- c(n,2)
	a

	
}

#######################################################
"get_pp_values"<-
function(
	pp1,			  	# multi ppfield
	flipped=T,
	mark=T,
	digits=3
)
{
  if(flipped) cat("Assuming plotted data is flipped",cr)
   
  res  <- data.frame()
  xy   <- locator(1)
  
  dimx <- dim(pp1$data)[1]
  dimy <- dim(pp1$data)[2]
  
  xslope <- dimx -1
  xint   <- 1
  yslope <- dimy -1
  yint   <- 1
  
  while(!is.null(xy)){

    x2 <- round( xy$x * xslope + xint)
    y2 <- round( xy$y * yslope + yint)
    
    if(flipped) y2 <- (dimy:1)[y2]

    d  <- pp1$data[x2,y2,]

    cat("[",x2,",",y2,"] = ")
    cat(d[1],"\n",sep=' ')

    if(mark){
      points(xy$x,xy$y,pch=19,cex=.5,col="blue")
      text(xy$x,xy$y,format(d[1],digits=digits),adj=-.2,col="blue")
    }

    res <- rbind(res,data.frame(x=x2,y=y2,z=d))
    xy <- locator(1)
  }
  res
}

#######################################################
"get_coords2"<-
function(
	im,			  	# 2d array image
	n=1,		
	...)
{
cat("NOT SURE THIS WORKS. DO NOT USE - get_coords2",cr)
return(NA)
	xdim <- dim(im)[1]
	ydim <- dim(im)[2]
	
	xpixelsize <- 1.0/xdim
	ypixelsize <- 1.0/ydim
	
	xmagic_number = 0 # determined by the typical bias on the cursor
	ymagic_number = ypixelsize/10.0 # determined by the typical bias on the cursor

	xscreen <- -(xpixelsize)/2+(0:xdim)*((1.0+xpixelsize)/xdim) + xmagic_number
	yscreen <- -(ypixelsize)/2+(0:ydim)*((1.0+ypixelsize)/ydim) + ymagic_number
	
	ip <- locator(n, type = "p", pch = "+")
	
	xcoords <- ip$x				
	ycoords <- ip$y 
	
	ix <- findInterval(xcoords,xscreen)
	iy <- findInterval(ycoords,yscreen)

	for(i in 1:length(xcoords)) cat(xcoords[i],ycoords[i],"\n")

	c(ix,iy)
	
}
#######################################################
"hist2"<-
function(x, width, xlim=xlim, freq = NULL, 
     angle = NULL, col = NULL, xname='x', border=NULL,
     main = paste("Histogram of" , xname),
     xlab = xname, density=NULL, add=FALSE,
     plot = TRUE,...)
{
	if(missing(width)) width=1
	scale1 = log10(width)
	if(scale1<0) sf=round(abs(scale1))
	else         sf=1
	
	r1 = range(x,na.rm=TRUE)
	neg1 = r1<0
	if(missing(xlim)) {
		xlim=1:2
		xlim[1] = signif((r1[1]-width),digits=sf)
		xlim[2] = signif((r1[2]+width),digits=sf)
	}
	breaks = seq(from=xlim[1], to=xlim[2], by=width)
	rbreaks= range(breaks)
	if(rbreaks[1]>r1[1]) {
		i1 = which(x<rbreaks[1])
		x[i1] = rbreaks[1]
	}
	if(rbreaks[2]<r1[2]) {
		i1 = which(x>rbreaks[2])
		x[i1] = rbreaks[2]
	}
	
	h <- hist(x,breaks = breaks,xlim=xlim, freq = freq, 
     angle = angle, col = col, border=border,
     main = main, density=density,
     xlab = xlab, add=add,
     plot = plot,...)
	 
	 h
	
}
#######################################################
"addtable2plotsjb"<-
function (x, y = NULL, table, lwd = par("lwd"), bty = "n", bg = par("bg"),
    cex = 1, xjust = 0, yjust = 1, box.col = par("fg"), text.col = par("fg"),
    display.colnames = TRUE, display.rownames = FALSE, hlines = FALSE,
    vlines = FALSE, title = NULL, xpd=TRUE, signiff=3)
{
    if (dev.cur() == 1)
        stop("Cannot add table unless a graphics device is open")
    if (is.null(y)) {
        if (is.null(x$y))
            stop("both x and y coordinates must be given")
        y <- x$y
        x <- x$x
    }
	# sjb
	table<- signif(table,signiff)
	
    tabdim <- dim(table)
    column.names <- colnames(table)
    if (is.null(column.names) && display.colnames)
        column.names <- 1:tabdim[1]
    row.names <- rownames(table)
    if (is.null(row.names) && display.rownames)
        row.names <- 1:tabdim[1]
    mwidth <- strwidth("M", cex = cex)
    if (par("xlog"))
        x <- log10(x)
    if (display.colnames) {
        cellwidth <- max(strwidth(c(column.names, row.names,
            as.vector(unlist(table))), cex = cex)) + mwidth
        nvcells <- tabdim[1] + 1
    }
    else {
        nvcells <- tabdim[1]
        cellwidth <- max(strwidth(c(row.names, as.vector(unlist(table))),
            cex = cex)) + mwidth
    }
    if (display.rownames)
        nhcells <- tabdim[2] + 1
    else nhcells <- tabdim[2]
    if (par("ylog"))
        y <- log10(y)
    cellheight <- max(strheight(c(column.names, row.names, as.vector(unlist(table))),
        cex = cex)) * 1.5
    xleft <- x - xjust * nhcells * cellwidth
    ytop <- y + yjust * nvcells * cellheight
    oldpar <- par(ylog = FALSE, ylog = FALSE, xpd = TRUE)
	
	#sjb bits
	par("xpd"=xpd)

    if (bty == "o")
        rect(xleft, ytop - nvcells * cellheight, xleft + nhcells *
            cellwidth, ytop, lwd = lwd, col = bg, border = box.col)
    for (row in 1:tabdim[1]) {
        if (row <= nvcells - 1 && hlines)
            segments(xleft, ytop - row * cellheight, xleft +
                nhcells * cellwidth, ytop - row * cellheight,
                lwd = lwd, col = box.col)
        if (display.rownames)
            text(xleft + 0.5 * cellwidth, ytop - (row + display.colnames -
                0.5) * cellheight, row.names[row], cex = cex,
                col = text.col)
        for (col in 1:tabdim[2]) {
            text(xleft + (col + display.rownames - 0.5) * cellwidth,
                ytop - (row + display.colnames - 0.5) * cellheight,
                table[row, col], cex = cex, col = text.col)
            if (vlines)
                segments(xleft + (col + display.rownames - 1) *
                  cellwidth, ytop - (row + display.colnames) *
                  cellheight, xleft + (col + display.rownames -
                  1) * cellwidth, ytop - row * cellheight)
        }
    }
    if (display.colnames)
        for (col in 1:tabdim[2]) {
            text(xleft + (col + display.rownames - 0.5) * cellwidth,
                ytop - 0.5 * cellheight, column.names[col], cex = cex,
                col = text.col)
            if (!hlines)
                segments(xleft, ytop - cellheight, xleft + nhcells *
                  cellwidth, ytop - cellheight, lwd = lwd, col = box.col)
        }
    if (!is.null(title)) {
        text(xleft + (nhcells * cellwidth)/2, ytop + cellheight/2,
            title, cex = cex, col = text.col)
        if (bty == "n")
            segments(xleft, ytop, xleft + nhcells * cellwidth,
                ytop, lwd = lwd, col = box.col)
    }
    par(oldpar)
}

#######################################################
### sourced from http://menugget.blogspot.co.uk/
#######################################################
### val2col
#this function converts a vector of values("z") to a vector of color
#levels. One must define the number of colors. The limits of the color
#scale("zlim") or the break points for the color changes("breaks") can 
#also be defined. when breaks and zlim are defined, breaks overrides zlim.

val2col<-function(z, zlim, col = heat.colors(12), breaks){
 if(!missing(breaks)){
  if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
 }
 if(missing(breaks) & !missing(zlim)){
  zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
  zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
 }
 if(missing(breaks) & missing(zlim)){
  zlim <- range(z, na.rm=TRUE)
  zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
  zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
 }
 CUT <- cut(z, breaks=breaks)
 colorlevels <- col[match(CUT, levels(CUT))] # assign colors to heights for each point
 return(colorlevels)
}

### image.scale
#This function creates a color scale for use with e.g. the image()
#function. Input parameters should be consistent with those
#used in the corresponding image plot. The "horiz" argument
#defines whether the scale is horizonal(=TRUE) or vertical(=FALSE).
#Depending on the orientation, x- or y-limits may be defined that
#are different from the z-limits and will reduce the range of
#colors displayed.
 
image.scale <- function(z, zlim, col = heat.colors(12),
breaks, horiz=TRUE, ylim=NULL, xlim=NULL, ...){
 if(!missing(breaks)){
  if(length(breaks) != (length(col)+1)){stop("must have one more break than colour")}
 }
 if(missing(breaks) & !missing(zlim)){
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1)) 
 }
 if(missing(breaks) & missing(zlim)){
  zlim <- range(z, na.rm=TRUE)
  zlim[2] <- zlim[2]+c(zlim[2]-zlim[1])*(1E-3)#adds a bit to the range in both directions
  zlim[1] <- zlim[1]-c(zlim[2]-zlim[1])*(1E-3)
  breaks <- seq(zlim[1], zlim[2], length.out=(length(col)+1))
 }
 poly <- vector(mode="list", length(col))
 for(i in seq(poly)){
  poly[[i]] <- c(breaks[i], breaks[i+1], breaks[i+1], breaks[i])
 }
 xaxt <- ifelse(horiz, "s", "n")
 yaxt <- ifelse(horiz, "n", "s")
 if(horiz){YLIM<-c(0,1); XLIM<-range(breaks)}
 if(!horiz){YLIM<-range(breaks); XLIM<-c(0,1)}
 if(missing(xlim)) xlim=XLIM
 if(missing(ylim)) ylim=YLIM
 plot(1,1,t="n",ylim=ylim, xlim=xlim, xaxt=xaxt, yaxt=yaxt, xaxs="i", yaxs="i", ...)  
 for(i in seq(poly)){
  if(horiz){
   polygon(poly[[i]], c(0,0,1,1), col=col[i], border=NA)
  }
  if(!horiz){
   polygon(c(0,0,1,1), poly[[i]], col=col[i], border=NA)
  }
 }
}

#######################################################
#identify.image <- function(xyz, mark=T, digits=3){
get.plot.data <- function(xyz, mark=T, digits=3){

  res  <- data.frame()
  xy   <- locator(1)
  
  if(!is.list(xyz)) {
    dimx <- dim(xyz)[1]
    dimy <- dim(xyz)[2]
  } else {
    dimx <- dim(xyz$z)[1]
    dimy <- dim(xyz$z)[2]
  }
  
  xslope <- dimx -1
  xint   <- 1
  yslope <- dimy -1
  yint   <- 1
  
  
  while(!is.null(xy)){

    x2 <- round( xy$x * xslope + xint)
    y2 <- round( xy$y * yslope + yint)

    if(!is.list(xyz)) {
      if(length(dim(xyz)) == 2 ) d  <- xyz[x2,y2] else  d  <- xyz[x2,y2,]
    } else d  <- xyz$z[x2,y2,]

    cat("[",x2,",",y2,"] = ")
    cat(d,"\n",sep=' ')

    if(mark){
      points(xy$x,xy$y,pch=19,cex=.5,col="blue")
      text(xy$x,xy$y,format(d[1],digits=digits),adj=-.2,col="blue")
    }

    res <- rbind(res,data.frame(x=x2,y=y2,z=d))
    xy <- locator(1)
  }
  
  res
}

#######################################################
#identify.image <- function(xyz, mark=T, digits=3){
get_coords <- function(xyz, mark=T, digits=3){

cat("NOT SURE THIS WORKS DO NOT USE - get_coords",cr)

  res  <- data.frame()
  xy   <- locator(1)
   
  while(!is.null(xy)){

    if(!is.list(xyz)) {
      d  <- xyz
      x2 <- dim(d)[1]* ((xy$x+0.05)/1.1) +1.0
      y2 <- dim(d)[2]* ((xy$y+0.05)/1.1) +1.0
      newx <- 1:(dim(d)[1]+1)
      newy <- 1:(dim(d)[2]+1)
    } else {
      d  <- xyz$z
      x2 <- xy$x+0.5
      y2 <- xy$y+0.5
      newx <- 1:(max(xyz$x)+1)
      newy <- 1:(max(xyz$y)+1)
    }
  
    xbin <- as.numeric(cut(x2,newx))
    ybin <- as.numeric(cut(y2,newy))

    #cat(xy$x,xy$y,' ')
    cat("[",xbin,",",ybin,"] = ")
    cat(d[xbin,ybin],"\n",sep='')
     
    if(mark){
      points(xy$x,xy$y,pch=19,cex=.5,col="blue")
      text(xy$x,xy$y,format(d[xbin,ybin],digits=digits),adj=-.2,col="blue")
    }
     
    res <- rbind(res,data.frame(i=xbin,j=ybin,x=x2,y=y2,z=d[xbin,ybin]))
    xy <- locator(1)
  }
  res
}

#######################################################
interactive_timeseries <- function(xyz, xd=NULL, ylim=NULL, col=1, mark=TRUE, add=FALSE, ...){

  firsttime <- T
  main.plt  <- dev.list()
  main.plt  <- main.plt[length(main.plt)]
  
  xy   <- locator(1)
   
  while(!is.null(xy)){

    if(!is.list(xyz)) {
      d  <- xyz[,,1]
      x2 <- dim(d)[1]* ((xy$x+0.05)/1.1) +1.0
      y2 <- dim(d)[2]* ((xy$y+0.05)/1.1) +1.0
      newx <- 1:(dim(d)[1]+1)
      newy <- 1:(dim(d)[2]+1)
    } else {
      d  <- xyz$z[,,1]
      x2 <- xy$x+0.5
      y2 <- xy$y+0.5
      newx <- 1:(max(xyz$x)+1)
      newy <- 1:(max(xyz$y)+1)
    }
  
    xbin <- as.numeric(cut(x2,newx))
    ybin <- as.numeric(cut(y2,newy))

    cat("[",xbin,",",ybin,"] = ")
    cat(d[xbin,ybin],"\n",sep='')
     
    if(mark) points(xy$x,xy$y,pch=19,cex=.5,col="blue")
    
    if(is.null(xd)) xd <- 1:length(xyz[xbin,ybin,])

    if (firsttime){
      x11()
      up.1()
      plot(xd, xyz[xbin,ybin,], type='l', col=col, ylim=ylim)
      firsttime <- FALSE
      sub.plt  <- dev.list()
      sub.plt  <- sub.plt[length(sub.plt)]
    } else if (!add & !firsttime) {
      x11()
      up.1()
      plot(xd, xyz[xbin,ybin,], type='l', col=col)
    } else {
      dev.set(sub.plt) # back to the sub plot to plot timeseries on
      col <- col+1
      lines(xd, xyz[xbin,ybin,], col=col)
    }
    
    dev.set(main.plt) # back to the original plot to get coords from
    
    xy <- locator(1)
  }

}
#######################################################
interactive_hist <- function(xyz, xd=NULL, ylim=NULL, col=1, nbin=100, mark=TRUE, add=FALSE, ...){

  firsttime <- T
  main.plt  <- dev.list()
  main.plt  <- main.plt[length(main.plt)]
  
  xy   <- locator(1)
   
  while(!is.null(xy)){

    if(!is.list(xyz)) {
      d  <- xyz[,,1]
      x2 <- dim(d)[1]* ((xy$x+0.05)/1.1) +1.0
      y2 <- dim(d)[2]* ((xy$y+0.05)/1.1) +1.0
      newx <- 1:(dim(d)[1]+1)
      newy <- 1:(dim(d)[2]+1)
    } else {
      d  <- xyz$z[,,1]
      x2 <- xy$x+0.5
      y2 <- xy$y+0.5
      newx <- 1:(max(xyz$x)+1)
      newy <- 1:(max(xyz$y)+1)
    }
  
    xbin <- as.numeric(cut(x2,newx))
    ybin <- as.numeric(cut(y2,newy))

    cat("[",xbin,",",ybin,"] = ")
    cat(d[xbin,ybin],"\n",sep='')
     
    if(mark) points(xy$x,xy$y,pch=19,cex=.5,col="blue")
    
    if(is.null(xd)) xd <- 1:length(xyz[xbin,ybin,])

    if (firsttime){
      x11()
      up.1()
      h1<-hist(xyz[xbin,ybin,], nbin, main=paste('[',xbin,',', ybin,']',sep=''))
      #plot(h1$mids,h1$counts,log='xy',xlab='ppt mm/day', ylab='frequency',main='Monthly rainfall, 240 years, Sahara x16 y89')
      firsttime <- FALSE
      sub.plt  <- dev.list()
      sub.plt  <- sub.plt[length(sub.plt)]
    } else if (!add & !firsttime) {
      x11()
      up.1()
      h1<-hist(xyz[xbin,ybin,], nbin, main=paste('[',xbin,',', ybin,']',sep=''))
    } else {
      dev.set(sub.plt) # back to the sub plot to plot timeseries on
      col <- col+1
      #lines(xd, xyz[xbin,ybin,], col=col)
      h1<-hist(xyz[xbin,ybin,], nbin, add=T, col=col)
    }
    
    dev.set(main.plt) # back to the original plot to get coords from
    
    xy <- locator(1)
  }

}

#######################################################
addtable2plotB <-
function (x, y = NULL, table, lwd = par("lwd"), bty = "n", bg = par("bg"), 
    cex = 1, xjust = 0, yjust = 1, box.col = par("fg"), text.col = par("fg"), 
    display.colnames = TRUE, display.rownames = FALSE, hlines = FALSE, 
    vlines = FALSE, title = NULL) 
{
    if (dev.cur() == 1) 
        stop("Cannot add table unless a graphics device is open")
    if (is.null(y)) {
        if (is.null(x$y)) 
            stop("both x and y coordinates must be given")
        y <- x$y
        x <- x$x
    }
    tabdim <- dim(table)
    column.names <- colnames(table)
    if (is.null(column.names) && display.colnames) 
        column.names <- 1:tabdim[1]
    row.names <- rownames(table)
    if (is.null(row.names) && display.rownames) 
        row.names <- 1:tabdim[1]
    mwidth <- strwidth("M", cex = cex)
    if (par("xlog")) 
        x <- log10(x)
    if (display.colnames) {
        #cellwidth <- max(strwidth(c(column.names, row.names, as.vector(unlist(table))), cex = cex)) + mwidth
        #cellwidth <- strwidth(c(column.names, row.names, as.vector(unlist(table))), cex = cex) + mwidth
        cellwidth <- c(max(strwidth(c(column.names, row.names)), cex = cex),strwidth(c(as.vector(unlist(table))), cex = cex)) + mwidth
        nvcells <- tabdim[1] + 1
    }
    else {
        nvcells <- tabdim[1]
        #cellwidth <- max(strwidth(c(row.names, as.vector(unlist(table))), cex = cex)) + mwidth
        cellwidth <- strwidth(c(row.names, as.vector(unlist(table))), cex = cex) + mwidth
        cellwidth <- c(max(strwidth(c(row.names), cex = cex)),strwidth(c(as.vector(unlist(table))), cex = cex)) + mwidth
    }
    if (display.rownames) 
        nhcells <- tabdim[2] + 1
    else nhcells <- tabdim[2]
    if (par("ylog")) 
        y <- log10(y)
    cellheight <- max(strheight(c(column.names, row.names, as.vector(unlist(table))), 
        cex = cex)) * 1.5
    #xleft <- x - xjust * nhcells * mean(cellwidth)
    xleft <- x - xjust * sum(cellwidth[1:(1+tabdim[2])])
    ytop <- y + yjust * nvcells * cellheight
    oldpar <- par(ylog = FALSE, ylog = FALSE, xpd = TRUE)
    if (bty == "o") 
        rect(xleft, ytop - nvcells * cellheight, 
             #xleft + nhcells * mean(cellwidth), ytop, 
             xleft + sum(cellwidth[1:(1+tabdim[2])]), ytop, 
             lwd = lwd, col = bg, border = box.col)
    for (row in 1:tabdim[1]) {
        if (row <= nvcells - 1 && hlines) 
            segments(xleft, ytop - row * cellheight, 
                     #xleft + nhcells * cellwidth[1], ytop - row * cellheight, 
                     xleft + sum(cellwidth[1:(1+tabdim[2])]), ytop - row * cellheight, 
                     lwd = lwd, col = box.col)
        if (display.rownames) 
            text(xleft + 0.5 * cellwidth[1], ytop - (row + display.colnames - 
                0.5) * cellheight, row.names[row], cex = cex, 
                col = text.col)
        for (col in 1:tabdim[2]) {
            #text(xleft + (col + display.rownames - 0.5) * cellwidth[col+1], 
            #text(xleft + sum(cellwidth[1:(col)]) - 0.5*cellwidth[col+1], 
            cat(cellwidth,cr)
            text(xleft + sum(cellwidth[1:(col+1)])  - 0.5*cellwidth[col+1], 
                ytop - (row + display.colnames - 0.5) * cellheight, 
                table[row, col], cex = cex, col = text.col)
            if (vlines) 
                segments(xleft + (col + display.rownames - 1) * 
                  cellwidth[col+1], ytop - (row + display.colnames) * 
                  cellheight, xleft + (col + display.rownames - 
                  1) * cellwidth[col+1], ytop - row * cellheight)
        }
    }
    if (display.colnames) 
        for (col in 1:tabdim[2]) {
            text(xleft + (col + display.rownames - 0.5) * cellwidth[col+1], 
                ytop - 0.5 * cellheight, column.names[col], cex = cex, 
                col = text.col)
            if (!hlines) 
                segments(xleft, ytop - cellheight, xleft + nhcells * 
                  cellwidth[col+1], ytop - cellheight, lwd = lwd, col = box.col)
        }
    if (!is.null(title)) {
        text(xleft + (nhcells * cellwidth[1])/2, ytop + cellheight/2, 
            title, cex = cex, col = text.col)
        if (bty == "n") 
            segments(xleft, ytop, xleft + nhcells * cellwidth[1], 
                ytop, lwd = lwd, col = box.col)
    }
    par(oldpar)
}
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
