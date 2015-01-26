
BveHTDepPen <- function (data, u, vx, vy, nsim, sim.exc = rexp(1000), bwd = 0.1, lamPen = 30, 
                            shrink = TRUE, zSampKnown = FALSE, noiseKnown = FALSE, cond.list = list()){
  # Compute the dependence parameters (alpha,beta) for the conditional extremes approach 
  # and output new simulated sample. Has additional penalty function to ensure that 
  # dependence isn't lost to the parameters of the Normal distribution taken as a 
  # false working assumption
  #
  # Args:
  #   data: Two column data frame with values given on uniform margins
  #   u: Modelling threshold
  #   vx, vy: Critical levels in x and y direction respectively (often set equal to one another)
  #   nsim: Number of new data points to be simulated above the critical level
  #   bwd: Choice of bandwidth if kernal smoothing is added
  #   lamPen: Penalty value to ensure that dependence is not lost to discarded Normal parameters
  #   shrink: Is kernal shrinkage to be included
  #   zSampKnown: If the z-sample to be picked is known then set this as a specific value instead of FALSE
  #   noiseKnown: If the kernal noise added needs to be specified then set this as a specific value instead of FALSE
  #   cond.list: The list that contains all the output
  #
  # Returns:
  #   Numerous outputs added to a list, including the dependence parameter values, full
  #   sample z and ones picked to simulate new exceedances.
  #
  # Function START
  
  x.dat.l <- qlaplace(data[, 1])
  y.dat.l <- qlaplace(data[, 2])
  dat.l <- cbind(x.dat.l, y.dat.l)
  thresh.l <- qlaplace(u)
  dat.lu <- dat.l[dat.l[, 1] > thresh.l, ]
  x.dat.lu <- dat.lu[, 1]
  y.dat.lu <- dat.lu[, 2]
  cond.llik <- function(p, x, y) {
    return((length(x)/2) * (log(2 * pi)) + (length(x)/2) * log(p[4]) + 
             p[2] * sum(log(x)) + (1/((2 * p[4]))) * sum(((y - 
          p[1] * x - (p[3] * x^(p[2])))^2)/(x^(2 * p[2]))) + lamPen*(p[3]^2))
  }
  bl <- c(-1, -10, -10^6, 1e-04)
  bu <- c(1, 1, 10^6, 10^6)
  start <- c(0.1, 0.1, 0, 1)
  par.vec <- optim(start, cond.llik, method = "L-BFGS-B", x = x.dat.lu, 
                   y = y.dat.lu, lower = bl, upper = bu, hessian = TRUE)
  nw.par <- par.vec$par[1:4]
  nw.nllh <- par.vec$value
  z.samp <- (y.dat.lu - nw.par[1] * x.dat.lu)/(x.dat.lu^(nw.par[2]))
  nw.musig <- c(mean(z.samp), var(z.samp))
  h.thresh.xl <- qlaplace(vx)
  h.thresh.yl <- qlaplace(vy)
  xn.l <- h.thresh.xl + sim.exc
  if (zSampKnown == FALSE) {
    zn.ls <- sample(z.samp, nsim, replace = T)
  }
  else {
    zn.ls <- zSampKnown
  }
  if (bwd == "scott") {
    h <- 1.06 * (min(sd(zn.ls), (IQR(zn.ls)/1.34))) * (nsim^(-1/5))
  }
  else {
    h <- bwd
  }
  if (shrink == TRUE) {
    a <- sqrt(1 - (h^2))
    zn.lm <- mean(zn.ls)
    zn.lsd <- sd(zn.ls)
    zn.bsd <- h * zn.lsd
    if (noiseKnown == FALSE) {
      gNoise <- rnorm(nsim, mean = 0, sd = zn.bsd)
    }
    else {
      gNoise <- noiseKnown
    }
    zn.l <- a * zn.ls + (1 - a) * zn.lm + gNoise
  }
  else {
    zn.lm <- mean(zn.ls)
    zn.lsd <- sd(zn.ls)
    zn.bsd <- h * zn.lsd
    if (noiseKnown == FALSE) {
      gNoise <- rnorm(nsim, mean = 0, sd = zn.bsd)
    }
    else {
      gNoise <- noiseKnown
    }
    zn.l <- zn.ls + gNoise
  }
  yn.l <- nw.par[1] * xn.l + (xn.l^(nw.par[2])) * zn.l
  pAcX <- sum(yn.l > h.thresh.yl)/length(yn.l)
  pX <- 1 - vx
  pA <- pAcX * pX
  cond.list$diag <- c(pX, pAcX, pA)
  cond.list$x <- plaplace(xn.l)
  cond.list$y <- plaplace(yn.l)
  cond.list$z <- plaplace(zn.l)
  cond.list$zsamp <- zn.ls
  cond.list$gNoise <- gNoise
  cond.list$origz <- plaplace(z.samp)
  cond.list$par <- nw.par
  cond.list$nllh <- nw.nllh
  cond.list$hessian <- par.vec$hessian
  return(cond.list)
#> str(cond.list)
#List of 10
# $ diag   : num [1:3] 0.01111 0.4 0.00444
# $ x      : num [1:10] 0.996 0.993 0.995 0.992 0.994 ...  X(t)
# $ y      : num [1:10] 0.999 0.799 0.998 0.999 0.984 ...  X(t+1)
# $ z      : num [1:10] 0.951 0.69 0.929 0.951 0.867 ...
# $ zsamp  : num [1:10] 2.317 0.479 1.946 2.323 1.321 ...
# $ gNoise : num [1:10] 0 0 0 0 0 0 0 0 0 0
# $ origz  : num [1:476] 0.548 0.638 0.855 0.961 0.541 ...
# $ par    : num [1:4] -0.108 0.73 1.072 0.544
# $ nllh   : num 851
# $ hessian: num [1:4, 1:4] 1.46e+03 1.15e+03 1.13e+03 2.55e-02 1.15e+03 ...
}
