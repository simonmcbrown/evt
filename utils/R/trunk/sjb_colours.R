# load various colour tables

BuOr_12<- function()	      
{
cname <- c('R','G','B','H','S','V','C','M','Y','K','R2','G2','B2')
ctbl  <- read.table('/home/h03/hadsx/extremes/R/colours/BuDOr_12.txt',colClasses='numeric',col.name=cname, skip=2) 
c1    <- rgb(ctbl$R,ctbl$G,ctbl$B)
c1
}

BrBu_12<- function()	      
{
cname <- c('R','G','B','H','S','V','C','M','Y','K','R2','G2','B2')
ctbl  <- read.table('/home/h03/hadsx/extremes/R/colours/BrBu_12.txt',colClasses='numeric',col.name=cname, skip=2) 
c1    <- rgb(ctbl$R,ctbl$G,ctbl$B)
c1
}

BuOr_18<- function()	      
{
cname <- c('R','G','B','H','S','V','C','M','Y','K','R2','G2','B2')
ctbl  <- read.table('/home/h03/hadsx/extremes/R/colours/BuDOr_18.txt',colClasses='numeric',col.name=cname, skip=2) 
c1    <- rgb(ctbl$R,ctbl$G,ctbl$B)
c1
}

BuRd_12<- function()	      
{
cname <- c('R','G','B','H','S','V','C','M','Y','K','R2','G2','B2')
ctbl  <- read.table('/home/h03/hadsx/extremes/R/colours/BuDRd_12.txt',colClasses='numeric',col.name=cname, skip=2) 
c1    <- rgb(ctbl$R,ctbl$G,ctbl$B)
c1
}

BuRd_18<- function()	      
{
cname <- c('R','G','B','H','S','V','C','M','Y','K','R2','G2','B2')
ctbl  <- read.table('/home/h03/hadsx/extremes/R/colours/BuDRd_18.txt',colClasses='numeric',col.name=cname, skip=2) 
c1    <- rgb(ctbl$R,ctbl$G,ctbl$B)
c1
}

GrMg_16<- function()	      
{
cname <- c('R','G','B','H','S','V','C','M','Y','K','R2','G2','B2')
ctbl  <- read.table('/home/h03/hadsx/extremes/R/colours/GrMg_16.txt',colClasses='numeric',col.name=cname, skip=2) 
c1    <- rgb(ctbl$R,ctbl$G,ctbl$B)
c1
}

StepSeq_25<- function()	      
{
cname <- c('R','G','B','H','S','V','C','M','Y','K','R2','G2','B2')
ctbl  <- read.table('/home/h03/hadsx/extremes/R/colours/StepSeq_25.txt',colClasses='numeric',col.name=cname, skip=2) 
c1    <- rgb(ctbl$R,ctbl$G,ctbl$B)
c1
}






