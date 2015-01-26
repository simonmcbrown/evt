
####################################################################
# A function file to collect together lots of small functions that #
# are required to run other codes                                  #
####################################################################

FwdStepSM <- function(x,dep,nrep){
  # Function that steps a MC chain forwards with transition probability
  # given in eqn (20) of my paper
  #
  # Args:
  #   x: The value of the chain at the current time
  #   dep: The logistic dependence parameter
  #   nrep: The number of replications which controls how many new uniform values to simulate
  #
  # Returns:
  #   out.put: The value of the chain at the next timestep
  #
  # Function START
  
  out.put <- x - (dep)*log(runif(nrep)^(1/(dep-1))-1)
  return( out.put )
}

FwdStepHT <- function(x,dep,z.samp,nrep){
  # Function that steps a MC forwards as in algorithm 3 of my paper
  # (uses Heffernan and Tawn methodology)
  #
  # Args:
  #   x: The value of the chain at the current time
  #   dep: The Heffernan and Tawn dependence parameters (alpha,beta)
  #   z.samp: Non-parametric estimate of the distribution G
  #   nrep: The number of replications which controls how many new uniform values to simulate
  #
  # Returns:
  #   out.put: The value of the chain at the next timestep
  #
  # Function START
  
  out.put <- numeric(nrep)
  out.put[x>0] <- dep[1] * pmax(x[x>0],0) + (pmax(x[x>0],0)^(dep[2])) * qlaplace(sample(x=z.samp,size=sum(x>0),replace=TRUE))
  out.put[x<=0] <- dep[1] * pmax(x[x<=0],0)
  
  return( out.put )
}

FwdStepNP <- function(x,dep,z.samp,nrep){
  # Function that steps a MC forwards as in algorithm 2 of my paper
  # (uses non-parametric methodology)
  #
  # Args:
  #   x: The value of the chain at the current time
  #   dep: The Heffernan and Tawn dependence parameters (alpha=1,beta=0)
  #   z.samp: Non-parametric estimate of the distribution G obtained as differences of original chain at consecutive time steps
  #   nrep: The number of replications which controls how many new uniform values to simulate
  #
  # Returns:
  #   out.put: The value of the chain at the next timestep
  #
  # Function START
  
  if (dep[1]!=1 || dep[2]!=0){
    stop("Need dependence parameters alpha=1 and beta=0 for asymptotically dependent method")
  }
  
  out.put <- numeric(nrep)
  out.put[x>0] <- dep[1] * pmax(x[x>0],0) + (pmax(x[x>0],0)^(dep[2])) * sample(x=z.samp,size=sum(x>0),replace=TRUE)
  out.put[x<=0] <- dep[1] * pmax(x[x<=0],0)
  
  return( out.put )
}

SizeFromList <- function(x){
  # Function that takes a decluster.runs object and gives just the 
  # size of each of the clusters
  #
  # Args:
  #   x: decluster.runs object
  #
  # Returns:
  #   Array containing the size of each cluster defined using the runs estimator
  #
  # Function START
  
  return( x$size )
}

MaxValIfFirst <- function(x){
  # Function that takes the size of the consecutive clusters defined by the runs
  # estimator and returns a value according to the event C^(i) on p4 of my paper
  #
  # Args:
  #   x: Set of sizes of the consecutive clusters
  #
  # Returns:
  #   The first value unless a longer cluster is observed after the first event
  #   which results in an output of zero
  #
  # Function START
  
  if (max(x)>=x[1]){
    out.put <- x[1]
  } else {
    out.put <- 0
  }
  return( out.put )
}

ExtInd <- function(dist){
  # Function that returns the extremal index from a duration distribution
  #
  # Args:
  #   dist: duration distribution
  #
  # Returns:
  #   The extremal index for the given duration distribution
  #
  # Function START
  
  return(1/sum((1:length(dist))*dist))
}

RemMissVals <- function(x){
  # Function that removes missing values in the paired bootstrapped data
  #
  # Args:
  #   x: nx2 data matrix that contains one set of bootstrapped data
  #
  # Returns:
  #   The input data with any missing values (-999.9 in our data set) removed
  #
  # Function START
  
  rem.xdat <- which(x[,1] < -900)      # Remove any missing values which are
  x <- x[-rem.xdat,]                   # defined as -999.9
  rem.xdat <- which(x[,2] < -900) 
  x <- x[-rem.xdat,]
  
  return( x )
}

DepFromList <- function(x){
  # Function that returns the dependence parameters from a H+T model fit object
  #
  # Args:
  #   x: An object obtained from a fit of the conditional extremes model
  #
  # Returns:
  #   The dependence parameters (alpha,beta,mu,sigma^2)
  #
  # Function START
  
  return( x$par )
}

zFromList <- function(x){
  # Function that returns the empirical estimate of G from a H+T model fit object
  #
  # Args:
  #   x: An object obtained from a fit of the conditional extremes model
  #
  # Returns:
  #   The empirical estimate of G
  #
  # Function START
  
  return( x$origz )
}

SwitchCols <- function(x){
  # Function that switches the columns of a data matrix with 2 columns. Function
  # is used to fit the conditional model X_{t}|X_{t+1}>u
  #
  # Args:
  #   x: A nx2 data matrix
  #
  # Returns:
  #   The original data set with the columns reversed. 
  #
  # Function START
  
  return( x[,c(2,1)] )
}

sig.u.to.v <- function(p,peak.val,thresh){
  # Function that converts from sigma.u to sigma.v
  #
  # Args:
  #   p: The marginal parameters (sigma.u,xi)
  #   peak.val: The peak value which serves as level v for evaluation of sigma.v
  #   thresh: Modelling threshold given on original scale
  #
  # Returns:
  #   Value of sigma.v at the given peak value
  #
  # Function START
  
  return(p[1] + p[2]*(peak.val-thresh))
}

newDate <- function(date.comp){
  # Function that takes the year, month and day and returns
  # an unique 8 digit number yyyymmdd
  #
  # Args:
  #   dat.comp: an array that contains (year,month,day)
  #
  # Returns:
  #   Unique 8 digit number in the form yyyymmdd
  #
  # Function START
  return( date.comp[1]*10000 + date.comp[2]*100 + date.comp[3] )
}

TS2PairedData <- function(dat,JJA.length){
  # Function that takes a time-series and obtains a nx2 matrix
  # with that values for X_{t+1} in the first column and X_{t}
  # in the second column. Also removes any overlap from year
  # to year
  #
  # Args:
  #   dat: time series of data
  #   JJA.length: length of the summer season (92 for observations, 90 for model data)
  #
  # Returns:
  #   A nx2 matrix with X_{t+1} in the first column and X_{t} in the second column
  #
  # Function START
  rem.nos <- seq(1,length(dat)+1,by=JJA.length)
  paired.dat <- cbind(c(NA,dat),c(dat,NA))
  
  return( paired.dat[-rem.nos,] )
}

CIsFromOptimObj <- function(fit,sig.lev){
  # Function that takes an object from an optim fit and
  # generates a 95% confidence interval for each of the parameters based upon 
  # the standard error of the respoective parameters
  #
  # Args:
  #   fit: an object obtained from a fit of the optim function
  #   sig.lev: (1-sig.lev)*100% confidence interval
  #
  # Returns:
  #   Confidence intervals for the dependence and marginal GPD parameters (amount of
  #   values will vary depending on whether covariates included)
  #
  # Error handling:
  if (is.null(fit$hessian)){
    stop("Need optim function to produce hessian matrix")
  }
  # Function START
  fisher.info <- solve(fit$hessian)
  prop.sigma <- sqrt(diag(fisher.info))
  
  return( rbind(fit$par - qnorm(1-(sig.lev/2))*prop.sigma,fit$par + qnorm(1-(sig.lev/2))*prop.sigma) )
}

firstMinusLast <- function(x){
  # Function that looks at the difference between the first and last values to give an idea of
  # the change in the global mean temp. across the different GCMs
  #
  # Args:
  #   x: A vector from which to take the difference between the first and last numbers
  #
  # Returns:
  #   The difference between the first and last numbers of the vector
  #
  # Function START
  return(x[length(x)]-x[1])
}

getTimeLagkData <- function(dat,k,seas.length=90,overlap=TRUE){
  # Function to obtain consecutive values for time lag k.
  # Is a general version of the function 'getTimeLag2Data'
  #
  # Args:
  #   dat: Data from which we wish to obtain values for t,t+1,...,t+k
  #   k: Time lag
  #   seas.length: If there is some overlap from year to year where there shouldn't be (i.e. taking only summer months)
  #   overlap: Is there an overlap from year to year?
  #
  # Returns:
  #   A nx(k+1) array that contains the original data shifted to different time lags 0,1,2,...,k
  #
  # Function START
  consec.dat <- array(NA,dim=c(length(dat)+k,k+1))
  for ( i in 1:(k+1) ){
    consec.dat[,i] <- c(rep(NA,length.out=(k-i+1)),dat,rep(NA,length.out=(i-1)))   # Add NAs to the top and bottom of the data to allow time-lag shift
  }
  if (overlap==TRUE){   # Is there an overlap between years which is not natural?
    overlap.vals.list <- list()
    for ( j in 1:k ){
      overlap.vals.list[[j]] <- ((2:(dim(consec.dat)[1]/seas.length)-1)*seas.length+j)   # Take the points where the overlap occurs
    }
    overlap.vals <- unlist(x=overlap.vals.list)
    out.dat <- consec.dat[-c(1:k,overlap.vals,dim(consec.dat)[1]-((k-1):0)),]   # Give the final data with the overlap and artifically inserted NAs removed
  } else {
    out.dat <- consec.dat[-c(1:k,dim(consec.dat)[1]-((k-1):0)),]   # Even if no overlap need to remove artificially inserted NAs
  }
  return( out.dat )
}

unifFromGpdVal <- function(xu,xd,u,ud,p){
  # Function to go from uniform value to original scale
  #
  # Args:
  #   xu: Data on uniform margins
  #   xd: Data on original margins
  #   u: Modelling threshold on uniform margins
  #   ud: Modelling threshold on original margins
  #   p: GPD parameters from marginal fit 
  #
  # Returns:
  #   Values on the original scale
  #
  #Function START
  out.put <- xu
  out.put[xu>u] <- ud+(p[1]/p[2])*(((1-xu[xu>u])/(p[3]))^(-p[2])-1)
  out.put[xu<=u] <- quantile(xd,probs=xu[xu<=u])
  return( out.put )
}
UnifFromGpdVal <- unifFromGpdVal

midValsFromEvent <- function(x,md=6){
  # Function that takes in a chain simulated using peak value
  # estimation (or an actual event) and gives the values that
  # occur around the maximum point.
  #
  # Args:
  #   x: Simulated or observed event with peak value
  #   md: The value at which the peak value occurs
  #
  # Returns:
  #   A set of values around the peak value
  #
  # Function START
  clusts <- decluster.runs(z=x,r=1)
  which.clust.mid.val <- clusts$cluster[clusts$s==md]
  which.val.mid.clust <- clusts$s[clusts$cluster==which.clust.mid.val]
  return( which.val.mid.clust )
}
MidValsFromEvent <- midValsFromEvent

################################################################################
"decluster.runs" <-
function(z, r, blocks=NULL) {

  # Description:
  #
  #   Performs runs declustering.
  #
  # Usage:
  #
  #   decluster.runs(z, r)
  #
  # Arguments:
  #
  #   z: logical vector indicating which positions correspond to
  #      extreme values.
  #   r: integer run length
  #
  # Value:
  #
  #   A list containing the following elements.
  #
  #    scheme: name of declustering scheme
  #       par: value of declustering parameter (run length)
  #        nc: number of clusters
  #      size: vector of cluster sizes
  #         s: vector of times of extremes
  #   cluster: vector of numbers identifying clusters to which
  #            extremes belong
  #         t: vector of times between extremes
  #     inter: vector of intercluster time indicators (logical)
  #     intra: vector of intracluster time indicators (logical)
  #
  # Details:
  #
  #   Extremes separated by fewer than `r' non-extremes belong
  #   to the same cluster. Setting `r' < 1 causes each extreme
  #   to form a separate cluster.
  #
  # Warning:
  #
  # References:
  #
  #   Smith RL (1989) Extreme value analysis of environmental time
  #   series: an application to trend detection in ground-level
  #   ozone. Statistical Science 4, 367-393.
  #
  # See Also:
  #
  #   decluster.intervals
  #
  # Examples:
  #
  #   x <- rnorm(1000)
  #   decluster.runs(x > quantile(x, 0.9), 1)

  nx <- sum(z)
  s <- c(1:length(z))[z]
  #cat('nx ',nx,cr)
  #if (is.na(nx)) browser()
  cluster <- rep(1,nx)
  if(is.null(blocks)) {
    t <- diff(s)
    if(nx > 1) cluster[2:nx] <- 1 + cumsum(t > r)
  } else {
  b <- blocks[z]
  m <- aggregate(!is.na(z),by=list(blocks),sum)$x
  n <- aggregate(!is.na(z[z]),by=list(b),sum)$x
  cluster <- rep.int(1:length(m), times=m)
  ifun <- function(z,r) {
     out <- decluster.runs(z=z,r=r)
     return(out$cluster)
  } # end of 'ifun' internal function.
  tmp <- c(unlist(aggregate(z, by=list(cluster), ifun, r=r)$x))
  M <- c(0, aggregate(tmp, by=list(b), max, na.rm=TRUE)$x)
  M <- cumsum(M[-length(M)])
  M <- rep.int(M,times=n)
  cluster <- tmp+M
  ifun2 <- function(z,r) {
     if(sum(z)==1) return(Inf)
     else if(sum(z)==0) return(-Inf)
           out <- decluster.runs(z=z,r=r)
           return(out$t)
        } # end of 'ifun2' internal function.
  t <- c(unlist(aggregate(z,by=list(blocks),ifun2, r=r)$x)) 
  t <- t[t>0]
  t <- t[-1]
  }
  size <- tabulate(cluster)
  nc <- length(size)
  inter <- rep(FALSE, nx)
  inter[match(1:nc, cluster)] <- TRUE
  list(scheme = "runs", par = r, nc = nc, size = size, s = s, cluster = cluster, t = c(NA, t), inter = inter, intra = !inter, r=r, blocks=blocks)
}

