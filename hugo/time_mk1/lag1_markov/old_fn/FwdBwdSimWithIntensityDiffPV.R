
FwdBwdSimWithIntensityDiffPV <- function(fwdpar,fwdGz,bwdpar,bwdGz,n,thresh,peakVal,nrep){
  # Compute probabilities P(N=i|M=Mcl) and P(NC=i|M=Mcl) using 
  # peak value estimation technique
  #
  # NB: This slightly altered code takes in the variable 'peakVal' as a vector
  # of different starting values that allows the maximum of each event to vary
  # in a particular range.
  # 
  # Args:
  #   fwdpar: H+T dependence parameters from a fit of X_{t+1}|X_{t}>thresh
  #   fwdGz: Empirical estimate of the distribution G for the above forward fit
  #   bwdpar: H+T dependence parameters from a fit of X_{t}|X_{t+1}>thresh
  #   bwdGz: Empirical estimate of the distribution G for the above backward fit
  #   n: The length of each chain to be simulated, final output clusters will have length 2n-1
  #   thresh: Modelling threshold on uniform scale
  #   peakVal: Maximum value of the replicate clusters to be simulated
  #   nrep: The number of replicate clusters simulated to generate P(N=i|M=Mcl) and P(NC=i|M=Mcl)
  #
  # Returns:
  #   Probabilities P(N=i|M=Mcl) and P(NC=i|M=Mcl) computed from the replicate clusters
  #
  # Function START
  
  #browser()
  
  thresh.l <- qlaplace(thresh)  # transform the threshold from the uniform scale onto the laplace scale
  
  Xtfwd <- Xtbwd <- array(NA,dim=c(n,nrep))
  
  peakVal.l <- qlaplace(peakVal)   # Convert the cluster maximum to the Laplace margins that we wish to work on
  
  Xtfwd[1,] <- peakVal.l
  for (i in 1:(n-1)){
    Xtfwd[i+1,] <- FwdStepHT(Xtfwd[i,],dep=fwdpar,z.samp=fwdGz,nrep=nrep)
  }
  
  while(any(apply(X=Xtfwd,MARGIN=2,FUN=max)>peakVal.l)){
    fail.nos <- which(apply(X=Xtfwd,MARGIN=2,FUN=max)>peakVal.l)
    Xtfwd[,fail.nos] <- 0
    Xtfwd[1,fail.nos] <- peakVal.l[fail.nos]
    for (i in 1:(n-1)){
      Xtfwd[i+1,fail.nos] <- FwdStepHT(Xtfwd[i,fail.nos],dep=fwdpar,z.samp=fwdGz,nrep=length(Xtfwd[i,fail.nos]))
    }
  }
  
  Xtbwd[1,] <- peakVal.l
  for (i in 1:(n-1)){
    Xtbwd[i+1,] <- FwdStepHT(Xtbwd[i,],dep=bwdpar,z.samp=bwdGz,nrep=nrep)
  }
  
  while(any(apply(X=Xtbwd,MARGIN=2,FUN=max)>peakVal.l)){
    fail.nos <- which(apply(X=Xtbwd,MARGIN=2,FUN=max)>peakVal.l)
    Xtbwd[,fail.nos] <- 0
    Xtbwd[1,fail.nos] <- peakVal.l[fail.nos]
    for (i in 1:(n-1)){
      Xtbwd[i+1,fail.nos] <- FwdStepHT(Xtbwd[i,fail.nos],dep=bwdpar,z.samp=bwdGz,nrep=length(Xtbwd[i,fail.nos]))
    }
  }
  
  Xtsim <- rbind(apply(X=Xtbwd,MARGIN=2,FUN=rev),Xtfwd[-1,])
  
 # phi <- apply(X=Xtsim>thresh.l,MARGIN=2,FUN=sum)
 # #sjb20140806 consec.vals <- apply(X=Xtsim>thresh.l,MARGIN=2,FUN=decluster.runs,r=1)   # Only want the cluster sizes coming out here!
 # browser()   ### stop here and see if next line works
 # consec.vals <- apply(X=Xtsim,MARGIN=2,FUN=decluster.runs,r=1,threshold=thresh.l)   # update to new decluster Only want the cluster sizes coming out here!
 # chi <- unlist(lapply(X=lapply(X=consec.vals,FUN=SizeFromList),FUN=max))
 # 
 # pNcM <- c(tabulate(phi),rep(0,length.out=2*n-length(tabulate(phi))))/nrep
 # pNCcM <- c(tabulate(chi),rep(0,length.out=2*n-length(tabulate(chi))))/nrep
 # 
 # out.put <- list()
 # out.put[[1]] <- pNcM
 # out.put[[2]] <- pNCcM
 # out.put[[3]] <- Xtsim
  
  out.put <- list()
  out.put[[1]] <- NULL
  out.put[[2]] <- NULL
  out.put[[3]] <- Xtsim
  
  return( out.put )   # Remember to return the output of the code!
  
}
