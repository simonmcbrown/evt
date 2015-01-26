# poss_rubbish.R

# obs equivalent
# Additions by Hugo to create more realistic check marks DATE: 22/10/14
p3.xo <- q3.xo <- q4.xo <- array(NA,dim=c(length(udayo),length(p)))
 x.save.no.listo <- unlist(x.saveo)
xx.save.no.listo <- unlist(xx.saveo)
for (d in 1:length(udayo)){   
  p3.xo[d,] <- 1 -(1-p)/(1-cumsum(c(0,p2.n.dayo[udayo]))[d])   # P(S<=s|D>=d) calculated as P(S<=s,D>=d)/P(D>=d)
                                                      # 1-p                                     is: P(Event>e)
                                                      # (1-cumsum(c(0,p2.n.day))[d]             is: P(D>=d)
                                                      # (1-p)/(1-cumsum(c(0,p2.n.day))[d])      is: P(S>=s|D>=d)
                                                      # 1 - (1-p)/(1-cumsum(c(0,p2.n.day))[d])  is: P(S<=s|D>=d)  # need S<=s for the quantile function below
  p3.x.tempo <- p3.xo[d,]
  p3.x.tempo[p3.x.tempo<0] <- NA   # Will get negative values that are just ignored
  # Estimate the quantile using all the data for which D>=d and the probabilities calculated above
  q3.xo[d,] <- quantile(x.save.no.listo[(cumsum(c(0,n2.dayo[udayo]))[d]+1):n0o],probs = p3.x.tempo)
  q4.xo[d,] <- quantile(xx.save.no.listo[(cumsum(c(0,n2.dayo[udayo]))[d]+1):n0o],probs = p3.x.tempo)
  #readline("Stop")
}


# obs equivalent
n0o       <- length(obs.events[[3]])
n.dayo    <- NULL
p.n.dayo  <- NULL
n2.dayo   <- NULL
p2.n.dayo <- NULL
x.saveo   <- list()   # obs equivalent severity
xx.saveo   <- list()  # obs equivalent peak temperature during the heatwave
for (d in udayo) {
  ixo           <- which(obs.events[[3]] == d)
  xo            <- obs.events[[1]][ixo]
   x.saveo[[d]] <- xo                           # Add line that saves the severity for each duration d 
  xx.saveo[[d]] <- obs.events[[4]][ixo]            # Add line that saves the peak     for each duration d 
  n2.dayo[d]    <- length(ixo)                  # number of events with heatwave length of =d days
  p2.n.dayo[d]  <- n2.dayo[d]/n0o                # probability of having a heatwave of length = d days
}

