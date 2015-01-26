
EcdfWithGpd <- function(data,p,u){
  # Transform data from original margins onto uniform margins using GPD above 
  # threshold whilst using empirical cdf to points that lie below the threshold
  #
  # Args:
  #   data: Single array of data given on original margins
  #   p: Vector of parameters (sigma,xi,lambda)
  #   u: Threshold on original margins above which to fit GPD
  #
  # Returns:
  #   Array of data values on uniform margins
  #
  # Error handling:
  if (length(p)!=3){
    stop("Length of parameter vector incorrect")
  }
  # Function START
  fit.cdf <- ecdf(data)
  cdfs <- as.vector(sapply(data, fit.cdf))
  Fex <- 1 - p[3]*pmax(0,(1+(p[2]*((data[data>u]-u)/p[1]))))^(-1/p[2])   # pmax function gives the parallel maximum (element by element maximum)
  cdfs[data>u] <- Fex
  return(cdfs)
}