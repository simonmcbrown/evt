
DurSevFromSim <- function(data,marg.obj,dep.obj,dep.obj.bwd,thresh.u,crit.lev.u,max.lb,max.ub,nsim){
  # A function that outputs the duration and severity
  # distributions from a simulated data set along with
  # useful plots for validation
  #
  # Args:
  #   data: Data on original margins (only used to work out quantiles below the modelling threshold)
  #   marg.obj: An object that contains a fit of 'PotNllhGpd' with all marginal parameters
  #   dep.obj: An object that contains a fit from 'BveHTDepPen.R' with all dependence parameters
  #   dep.obj.bwd: An object that contains a fit from 'BveHTDepPen.R' with all dependence parameters for the backward chain
  #   thresh.u: Modelling threshold on uniform margin
  #   crit.lev.u: Critical level on uniform margin
  #   max.lb, max.ub: Lower and upper bounds on the maximum value
  #   nsim: Number of replicate events to be simulated
  #
  # Returns:
  #   A list of variables 'out.put' that include:
  #   (1) The severity values of each simulated event
  #   (2) The duration distribution
  #   (3) The duration values of each simulated event
  #   (4) The peak values used to simulate from
  #   (5) The modelling threshold on original margins
  #
  # Function START
  
  thresh <- quantile(x=data,probs=thresh.u)
  
  #random.starts <- runif(n=nsim,min=max.lb,max=max.ub)   # Choose the vector of random starting positions
  random.starts <- rgpd(n=nsim,loc=max.lb,scale=marg.obj$par[2]+marg.obj$par[3]*(max.lb-thresh),shape=marg.obj$par[3])
  unif.random.starts <- EcdfWithGpd(data=random.starts,p=marg.obj$par[2:4],u=thresh)   # Need the starting positions on uniform margins for the function
  sim.events2 <- FwdBwdSimWithIntensityDiffPV(fwdpar=dep.obj$par,fwdGz=dep.obj$origz,
                 bwdpar=dep.obj.bwd$par,bwdGz=dep.obj.bwd$origz,n=40,thresh=thresh.u,
                 peakVal=unif.random.starts,nrep=nsim)[[3]]
  
  sim.events2.u <- matrix(plaplace(sim.events2),nrow=dim(sim.events2)[1],ncol=dim(sim.events2)[2])
  sim.events2.orig <- apply(X=sim.events2.u,MARGIN=2,FUN=unifFromGpdVal,xd=data,u=thresh.u,ud=thresh,p=marg.obj$par[2:4])
  
  mid.run.vals.sim <- apply(sim.events2.orig>thresh,MARGIN=2,FUN=midValsFromEvent,md=40)
  
  sev.sum.sim <- numeric(dim(sim.events2.orig)[2])
  for ( i in 1:dim(sim.events2.orig)[2] ){
    sev.sum.sim[i] <- sum(sim.events2.orig[mid.run.vals.sim[[i]],i]-thresh)
  }
  
  dur.dist.sim <- tabulate(unlist(lapply(X=mid.run.vals.sim,FUN=length)))/sum(tabulate(unlist(lapply(X=mid.run.vals.sim,FUN=length))))
  dur.vals <- unlist(lapply(X=mid.run.vals.sim,FUN=length))
  
  out.put <- list()
  out.put[[1]] <- sev.sum.sim
  out.put[[2]] <- dur.dist.sim
  out.put[[3]] <- dur.vals
  out.put[[4]] <- random.starts
  out.put[[5]] <- thresh
  
  return( out.put )
  
}