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
