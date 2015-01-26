
ContourFromSevDur <- function(obs.obj,sim.obj,dur.bwd,grid.res,probs=c(0.99)){
  # Function that creates a contour plot with
  # contours for the probability of a point
  # falling outside the severity/duration area
  #
  # Args:
  #   obs.obj: An object fitted using 'DurSevFromObs.R'
  #   sim.obj: An object fitted using 'DurSevFromSim.R'
  #   dur.bwd: Bandwidth to smooth the duration (in days) to get more realistic contour lines
  #   gris.res: Resolution of the grid upon which to estimate the 2D KDE
  #   probs: Probability values at which to draw contours
  #
  # Returns:
  #   A plot of the contours superimposed onto the simulated values and 
  #   then onto the observations
  #
  # Function START
  
  # Need to choose bandwidth over duration so as to remove unrealism over discretization
  # Can increase the resolution of the grid over which the 2D KDE is made (via 'n' variable)
  cont.2d <- kde2d(x=sim.obj[[3]],y=sim.obj[[1]],h=c(dur.bwd,1.5*bandwidth.nrd(sim.obj[[1]])),n=grid.res) 
  ###cont.2d <- kde2d(x=sim.obj[[3]]+runif(length(sim.obj[[3]]))-0.5,y=sim.obj[[1]],h=c(dur.bwd,bandwidth.nrd(sim.obj[[1]])),n=grid.res) 
  dx <- diff(cont.2d$x[1:2])   # Work out the difference between x axis values
  dy <- diff(cont.2d$y[1:2])   # Work out the difference between y axis values
  sz <- sort(cont.2d$z)   # Sort all values into order
  c1 <- cumsum(sz)*dx*dy   # Work out the cumulative sum of density values multiplied by difference between x and y axis vals
  
  # Interpolate the function to calculate the value associated to the desired probability contour
  # which can be used as the level at which to plot the contour
  level <- sapply(probs,function(x){
    approx(c1,sz,xout=1-x)$y
  })
  
  # Plot the underlying data along with associated contour values
  #par(mfrow=c(1,1),mar=c(5,5,2,2))
  xlim <- extend.range(c(0,obs.events[[3]],sim.events[[3]]))
  ylim <- extend.range(c(0,obs.events[[1]],sim.events[[1]]))

  plot(sim.obj[[3]],sim.obj[[1]],xlab="Duration (days)",ylab="Severity (arbitrary units)",main="Heatwaves: Central France",xlim=xlim,ylim=ylim,cex=.3,pch=3)
  points(obs.obj[[3]],obs.obj[[1]],cex=1,col=1,pch=21,bg='red')
  contour(cont.2d,levels=level,labels=probs,add=TRUE,col=4)
  #plot(obs.obj[[3]],obs.obj[[1]],xlab="Duration",ylab="Severity",main="Observed",xlim=xlim,ylim=ylim)
  #contour(cont.2d,levels=level,labels=probs,add=TRUE,col=4)
  legend(min(xlim),max(ylim),c('Observations','Simulated'),col=c(1,1),pch=c(21,3),pt.bg=c('red','transparent'))
  
}