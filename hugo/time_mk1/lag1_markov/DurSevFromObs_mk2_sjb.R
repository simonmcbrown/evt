
DurSevFromObs_mk2_sjb <- function(dat,thresh,max.lb,max.ub,pts.ba,seas.length=92){
  # Function that outputs the duration and severity
  # characteristics of observed events
  #
  # Args:
  #   dat: Original data without missing values removed on the original margins
  #   thresh: Modelling threshold on the original scale
  #   max.lb, max.ub: Lower and upper bounds on the maximum value
  #   pts.ba: The length of the period leading up to and following the maximum value
  #
  # Returns:
  #   A list of variables 'out.put' that include:
  #   (1) The severity values of each observed event
  #   (2) The duration distribution
  #   (3) The duration values of each observed event
  #   (4) The peak values for each observed cluster
  #   (5) The modelling threshold on original margins
  #
  # Function START

  data.to.decluster         <- dat
  logical.data.to.decluster <- data.to.decluster > thresh
  ny                        <- length(dat)/seas.length
  declustered.data          <- decluster.runs(z=logical.data.to.decluster,r=1,blocks=rep(1:ny,each=seas.length))
  
  # Create a data frame with the original value, cluster number and overall location (where it falls in the vector)
  data.with.cluster <- cbind(data.to.decluster[declustered.data$s],as.factor(declustered.data$cluster),declustered.data$s)
  
  # Work out which of the cluster values are the maximum and where these occur in the original data
  max.clust <- place.max.clust <- numeric(max(data.with.cluster[,2]))
  for (i in 1:max(data.with.cluster[,2])){
    max.clust[i] <- max(data.with.cluster[data.with.cluster[,2]==i,1])
    place.max.clust[i] <- (data.with.cluster[data.with.cluster[,2]==i,3][(data.with.cluster[data.with.cluster[,2]==i,1]==max.clust[i])])[1]
  }
  
  # Pick out the maxima that fall within the range 'max.lb' to 'max.ub'
  where.max.in.range <- (max.clust > max.lb) & (max.clust <= max.ub)
  place.where.max <- place.max.clust[where.max.in.range]   # Where do these values occur in the original data?
  
  ###sjb data.to.decluster[place.where.max]
  ###sjb code to remove enents that will produce out-of-bounds index
  place.where.max <- place.where.max[place.where.max>pts.ba & place.where.max<=(length(data.to.decluster)-pts.ba)]
  
  max.clust.event <- max.clust[where.max.in.range]   # The observed maxima for which lay in the range (max.lb,max.ub)
  data.to.decluster[place.where.max]
  
  # Take the 5 points before and 5 points after the cluster maximum in the full
  # temperature series which will be plotted
  obs.event <- matrix(NA,nrow=length(place.where.max),ncol=(2*pts.ba+1))
  for (i in 1:length(place.where.max)){
    obs.event[i,] <- data.to.decluster[(place.where.max[i]-pts.ba):(place.where.max[i]+pts.ba)]
  }
  
  log.above.thresh <- obs.event > thresh
  # Apply the function above to give the positions of the values that occur
  # either side of the maximum without going below the threshold
  mid.run.vals <- apply(X=log.above.thresh,MARGIN=1,FUN=MidValsFromEvent,md=(pts.ba+1))
  
  # Sum up the severities for the consecutive events
  sev.sum.obs <- numeric(dim(obs.event)[1])
  for ( i in 1:dim(obs.event)[1] ){
    sev.sum.obs[i] <- sum(obs.event[i,mid.run.vals[[i]]]-thresh)
  }
  
  # Calculate the duration distribution as the number of consecutive exceedances
  dur.dist.obs <- tabulate(unlist(lapply(X=mid.run.vals,FUN=length)))/sum(tabulate(unlist(lapply(X=mid.run.vals,FUN=length))))
  dur.vals.obs <- unlist(lapply(X=mid.run.vals,FUN=length))
  
  out.put <- list()
  out.put[[1]] <- sev.sum.obs
  out.put[[2]] <- dur.dist.obs
  out.put[[3]] <- dur.vals.obs
  out.put[[4]] <- max.clust.event
  out.put[[5]] <- thresh
  
  return( out.put )
  
}