#source("/home/h03/hadsx/extremes/R/libs/simon_fields.R")

source("/home/h03/hadsx/extremes/R/libs/simon_stats.R")  
source("/home/h03/hadsx/extremes/R/libs/pp_io.R")  

#######################################################
"ppfield.fit.surface" <-
function (pp,np=1,iwhich=NULL)
# np degree of polynomial surface eg 1=plane
{
if (is.null(iwhich)) iwhich = 1:length(pp$data[1,1,]) 

ppsm          <- extract.fields.pp(pp,iwhich)
ppsm$data[,,] <- ppsm$bmdi[1]

id2 <- 0
for (id in iwhich) {
	id2 <- id2 +1
	
	### NOT SURE THIS IS WHAT WAS INTENDED BPLON & BPLAT DEFINE THE POLE NOT THE START OF THE GRIDD
	x <- rep(    seq(from=pp$bplon[id], by=pp$bdx[id], length=pp$lbnpt[id]), pp$lbrow[id])
	y <- rep(    seq(from=pp$bplat[id], by=pp$bdy[id], length=pp$lbrow[id]), 1, each=pp$lbnpt[id])
	z      <- pp$data[,,id]
	dim(z) <- c(1,length(z))
	z      <- drop(z)
	inmdi  <- which(z != pp$bmdi[id])
	imdi   <- which(z == pp$bmdi[id])

	df1          <- data.frame(x=x[inmdi],y=y[inmdi],z=z[inmdi])
	sm1          <- surf.ls(np,df1)
	trsurf.sm1.b <- trmat.sjb(sm1,x[1],x[length(x)],y[1],y[length(y)],pp$bdx[id],pp$bdy[id])

	trsurf.sm1.b$z[imdi] <- pp$bmdi[id]

	ppsm$data[,,id2] <- trsurf.sm1.b$z
}

invisible(ppsm)
}


#######################################################
