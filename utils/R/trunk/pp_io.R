# functions in this file are for use with pp format data
# it includes:
# read.row.pp read a whole row of a pp field as read.point so slow.
# read.point.pp
# read.pp
# write.pp
# clone.pp
# extract.fields.pp
# inquire.pp 

#  26.01.05 read.pp default changed to read whole file if no ixdfields supplied

# 04.01.08 added code to all pp reading fns to check if data is gzipped.  If so file is copied to /data/local/hadsx/tmp_R and unzipped.  Code checks to see if this has been done already.  Use clean.pp.temp_R to tidy up files


#######################################################################

read.row.pp <- function(pp.file, irow, idxfields=NA, quiet=TRUE) {

##  NB irow is in R coordinates ie for first row irow=1  ##

dyn.load("/home/h03/hadsx/extremes/R/C/C_pp.sl")

# test to see if file is gzipped, if so copy to localdata/tmp_R and unzip
is.gz <- is.finite(grep('.gz',pp.file)[1])
if(is.gz[1]>0) {
	# test if file has already been unzipped
	pp.file.nogz <- strsplit(basename(pp.file),'.gz')[[1]]
	cmd1 <- paste('ls ','/data/local/hadsx/tmp_R/',pp.file.nogz,sep='')
	a1   <- system(cmd1,TRUE,TRUE)
	if(is.na(a1[1])) { # file is not unzipped so unzip
		if(!quiet) cat('GZIPPED FILE.  Unzipping to /data/local/hadsx/tmp_R \n')
		cmd2 <- paste('mkdir','/data/local/hadsx/tmp_R',sep=' ')
		a2   <- system(cmd2,TRUE,TRUE)
		cmd3 <- paste('cp',pp.file,'/data/local/hadsx/tmp_R',sep=' ')
		a3   <- system(cmd3,TRUE,TRUE)
		cmd4 <- paste('gunzip ','/data/local/hadsx/tmp_R/',basename(pp.file),sep='')
		a4   <- system(cmd4,TRUE,TRUE)
	}
	cmd5 <- 	paste('echo ','/data/local/hadsx/tmp_R/',basename(pp.file.nogz),sep='')
	pp.file <-system(cmd5,TRUE,TRUE)
}


# count fields and make pp structure to fit

	o1 <- .C("c_inquire_pp_file",as.character(pp.file),
				count=integer(1),nrow=integer(1), npt=integer(1), quiet=quiet)

	nrow  <- o1$nrow
	npt   <- o1$npt

if (any(is.na(idxfields))){
	count <- o1$count
	idxfields <- 1:count
} else {
	count <- length(idxfields)
}

int.dummy <- 0
dbl.dummy <- 0

# cat("Number of points/fields to be read",npt,count,cr)

o2 <- .C("c_read_row_pp_file",irow=as.integer(irow),
					filename=as.character(pp.file),
					nfields=as.integer(count),
					idxfields=as.integer(idxfields),
					lbyr=integer(count),
					lbmon=integer(count),
					lbdat=integer(count),
					lbhr=integer(count),
					lbmin=integer(count),
					lbday=integer(count),
					lbyrd=integer(count),
					lbmond=integer(count),
					lbdatd=integer(count),
					lbhrd=integer(count),
					lbmind=integer(count),
					lbdayd=integer(count),
					lbtim=integer(count),
					lbft=integer(count),
					lblrec=integer(count),
					lbcode=integer(count),
					lbhem=integer(count),
					lbrow=integer(count),
					lbnpt=integer(count),
					lbext=integer(count),
					lbpack=integer(count),
					lbrel=integer(count),
					lbfc=integer(count),
					lbcfc=integer(count),
					lbproc=integer(count),
					lbvc=integer(count),
					lbrvc=integer(count),
					lbexp=integer(count),
					lbegin=integer(count),
					lbnrec=integer(count),
					lbproj=integer(count),
					lbtyp=integer(count),
					lblev=integer(count),
					lbrsvd=as.integer(array(int.dummy,dim=c(4,count))),
					lbsrce=integer(count),
					lbuser=as.integer(array(int.dummy,dim=c(7,count))),
					brsvd=as.single(array(dbl.dummy,dim=c(4,count))),
					bdatum=single(count),
					bacc=single(count),
					blev=single(count),
					brlev=single(count),
					bhlev=single(count),
					bhrlev=single(count),
					bplat=single(count),
					bplon=single(count),
					bgor=single(count),
					bzy=single(count),
					bdy=single(count),
					bzx=single(count),
					bdx=single(count),
					bmdi=single(count),
					bmks=single(count),
					data=as.single(array(dbl.dummy,dim=c(npt,count))),
					buf1=integer(count),
					buf2=integer(count),
					buf3=integer(count),
					buf4=integer(count)
					)

#					data=as.single(array(dbl.dummy,dim=c(1,1,count))),

dyn.unload("/home/h03/hadsx/extremes/R/C/C_pp.sl")
					
dim(o2$data)   <- c(npt,count)
dim(o2$lbrsvd) <- c(4,count)
dim(o2$lbuser) <- c(7,count)
dim(o2$brsvd)  <- c(4,count)

o2

}

#######################################################################

read.point.pp <- function(pp.file, ipt, irow, idxfields=NA, quiet=TRUE) {

dyn.load("/home/h03/hadsx/extremes/R/C/C_pp.sl")

# test to see if file is gzipped, if so copy to localdata/tmp_R and unzip
is.gz <- is.finite(grep('.gz',pp.file)[1])
if(is.gz[1]>0) {
	# test if file has already been unzipped
	pp.file.nogz <- strsplit(basename(pp.file),'.gz')[[1]]
	cmd1 <- paste('ls ','/data/local/hadsx/tmp_R/',pp.file.nogz,sep='')
	a1   <- system(cmd1,TRUE,TRUE)
	if(is.na(a1[1])) { # file is not unzipped so unzip
		cat('GZIPPED FILE.  Unzipping to /data/local/hadsx/tmp_R \n')
		cmd2 <- paste('mkdir','/data/local/hadsx/tmp_R',sep=' ')
		a2   <- system(cmd2,TRUE,TRUE)
		cmd3 <- paste('cp',pp.file,'/data/local/hadsx/tmp_R',sep=' ')
		a3   <- system(cmd3,TRUE,TRUE)
		cmd4 <- paste('gunzip ','/data/local/hadsx/tmp_R/',basename(pp.file),sep='')
		a4   <- system(cmd4,TRUE,TRUE)
	}
	cmd5 <- 	paste('echo ','/data/local/hadsx/tmp_R/',basename(pp.file.nogz),sep='')
	pp.file <-system(cmd5,TRUE,TRUE)
}


# count fields and make pp structure to fit
	o1 <- .C("c_inquire_pp_file",as.character(pp.file),
				count=integer(1),nrow=integer(1), npt=integer(1), quiet=quiet)
	nrow  <- o1$nrow
	npt   <- o1$npt


if (any(is.na(idxfields))){
	count <- o1$count
	idxfields <- 1:count
} else {
	count <- length(idxfields)
}

int.dummy <- 0
dbl.dummy <- 0

o2 <- .C("c_read_point_pp_file",ipt=as.integer(ipt),irow=as.integer(irow),
					filename=as.character(pp.file),
					nfields=as.integer(count),
					idxfields=as.integer(idxfields),
					lbyr=integer(count),
					lbmon=integer(count),
					lbdat=integer(count),
					lbhr=integer(count),
					lbmin=integer(count),
					lbday=integer(count),
					lbyrd=integer(count),
					lbmond=integer(count),
					lbdatd=integer(count),
					lbhrd=integer(count),
					lbmind=integer(count),
					lbdayd=integer(count),
					lbtim=integer(count),
					lbft=integer(count),
					lblrec=integer(count),
					lbcode=integer(count),
					lbhem=integer(count),
					lbrow=integer(count),
					lbnpt=integer(count),
					lbext=integer(count),
					lbpack=integer(count),
					lbrel=integer(count),
					lbfc=integer(count),
					lbcfc=integer(count),
					lbproc=integer(count),
					lbvc=integer(count),
					lbrvc=integer(count),
					lbexp=integer(count),
					lbegin=integer(count),
					lbnrec=integer(count),
					lbproj=integer(count),
					lbtyp=integer(count),
					lblev=integer(count),
					lbrsvd=as.integer(array(int.dummy,dim=c(4,count))),
					lbsrce=integer(count),
					lbuser=as.integer(array(int.dummy,dim=c(7,count))),
					brsvd=as.single(array(dbl.dummy,dim=c(4,count))),
					bdatum=single(count),
					bacc=single(count),
					blev=single(count),
					brlev=single(count),
					bhlev=single(count),
					bhrlev=single(count),
					bplat=single(count),
					bplon=single(count),
					bgor=single(count),
					bzy=single(count),
					bdy=single(count),
					bzx=single(count),
					bdx=single(count),
					bmdi=single(count),
					bmks=single(count),
					data=as.single(array(dbl.dummy,dim=c(count))),
					buf1=integer(count),
					buf2=integer(count),
					buf3=integer(count),
					buf4=integer(count)
					)

#					data=as.single(array(dbl.dummy,dim=c(1,1,count))),

dyn.unload("/home/h03/hadsx/extremes/R/C/C_pp.sl")
					
#dim(o2$data)   <- c(1,1,count)
dim(o2$data)   <- c(count)
dim(o2$lbrsvd) <- c(4,count)
dim(o2$lbuser) <- c(7,count)
dim(o2$brsvd)  <- c(4,count)

o2

}

#######################################################################

read.pp <- function(pp.file, idxfields=NA, quiet=TRUE) {

dyn.load("/home/h03/hadsx/extremes/R/C/C_pp.sl")

# test to see if file is gzipped, if so copy to localdata/tmp_R and unzip
is.gz <- is.finite(grep('.gz',pp.file)[1])
if(is.gz[1]>0) {
	# test if file has already been unzipped
	pp.file.nogz <- strsplit(basename(pp.file),'.gz')[[1]]
	cmd1 <- paste('ls ','/data/local/hadsx/tmp_R/',pp.file.nogz,sep='')
	a1   <- system(cmd1,TRUE,TRUE)
	if(is.na(a1[1])) { # file is not unzipped so unzip
		cat('GZIPPED FILE.  Unzipping to /data/local/hadsx/tmp_R \n')
		cmd2 <- paste('mkdir','/data/local/hadsx/tmp_R',sep=' ')
		a2   <- system(cmd2,TRUE,TRUE)
		cmd3 <- paste('cp',pp.file,'/data/local/hadsx/tmp_R',sep=' ')
		a3   <- system(cmd3,TRUE,TRUE)
		cmd4 <- paste('gunzip ','/data/local/hadsx/tmp_R/',basename(pp.file),sep='')
		a4   <- system(cmd4,TRUE,TRUE)
	}
	cmd5 <- 	paste('echo ','/data/local/hadsx/tmp_R/',basename(pp.file.nogz),sep='')
	pp.file <-system(cmd5,TRUE,TRUE)
}


# count fields and make pp structure to fit

	o1 <- .C("c_inquire_pp_file",as.character(pp.file),
				count=integer(1),nrow=integer(1), npt=integer(1), quiet=quiet)

	nrow  <- o1$nrow
	npt   <- o1$npt
	
MEM_CORRUPT_FIX <- FALSE

if (any(is.na(idxfields))){
	count <- o1$count
	idxfields <- 1:count
} else {
	if (length(idxfields)>1) count <- length(idxfields)
	else { # temp bodge to avoid memory error in C_pp.c when idxfields is only one field
		idxfields <- c(idxfields,idxfields)
		count <-2
		#cat("CAUTION:  Returning two fields to avoid memory corruption!",cr)
		MEM_CORRUPT_FIX <- TRUE
		#does not work idxfields <- as.integer(idxfields)
		#does not work count <-1
	}
}
#} else { # this does not work too
#	idxfields2 <- idxfields	# to avoid probles with temporary constants eg (read.pp(st1,1)
#	idxfields  <- idxfields2	# to avoid probles with temporary constants eg (read.pp(st1,1)
#	count <- length(idxfields2)
#}

int.dummy <- 0
dbl.dummy <- 0

o2 <- .C("c_read_pp_file",filename=as.character(pp.file),
					nfields=as.integer(count),
					idxfields=as.integer(idxfields),
					lbyr=integer(count),
					lbmon=integer(count),
					lbdat=integer(count),
					lbhr=integer(count),
					lbmin=integer(count),
					lbday=integer(count),
					lbyrd=integer(count),
					lbmond=integer(count),
					lbdatd=integer(count),
					lbhrd=integer(count),
					lbmind=integer(count),
					lbdayd=integer(count),
					lbtim=integer(count),
					lbft=integer(count),
					lblrec=integer(count),
					lbcode=integer(count),
					lbhem=integer(count),
					lbrow=integer(count),
					lbnpt=integer(count),
					lbext=integer(count),
					lbpack=integer(count),
					lbrel=integer(count),
					lbfc=integer(count),
					lbcfc=integer(count),
					lbproc=integer(count),
					lbvc=integer(count),
					lbrvc=integer(count),
					lbexp=integer(count),
					lbegin=integer(count),
					lbnrec=integer(count),
					lbproj=integer(count),
					lbtyp=integer(count),
					lblev=integer(count),
					lbrsvd=as.integer(array(int.dummy,dim=c(4,count))),
					lbsrce=integer(count),
					lbuser=as.integer(array(int.dummy,dim=c(7,count))),
					brsvd=as.single(array(dbl.dummy,dim=c(4,count))),
					bdatum=single(count),
					bacc=single(count),
					blev=single(count),
					brlev=single(count),
					bhlev=single(count),
					bhrlev=single(count),
					bplat=single(count),
					bplon=single(count),
					bgor=single(count),
					bzy=single(count),
					bdy=single(count),
					bzx=single(count),
					bdx=single(count),
					bmdi=single(count),
					bmks=single(count),
					data=as.single(array(dbl.dummy,dim=c(npt,nrow,count))),
					buf1=integer(count),
					buf2=integer(count),
					buf3=integer(count),
					buf4=integer(count)
					)

dyn.unload("/home/h03/hadsx/extremes/R/C/C_pp.sl")

dim(o2$data)   <- c(npt,nrow,count)
dim(o2$lbrsvd) <- c(4,count)
dim(o2$lbuser) <- c(7,count)
dim(o2$brsvd)  <- c(4,count)

if(MEM_CORRUPT_FIX) {
	o1           <- clone.pp(o2,1)
	o1$data      <- o2$data[,,1]
	o1$idxfields <- o2$idxfields[1]
	o2           <- o1
	dim(o2$data) <- c(npt,nrow,1)
}
					

o2

}

#######################################################################

write.pp <- function(pp1, fname) {

dyn.load("/home/h03/hadsx/extremes/R/C/C_pp.sl")

# if fails with NA/NaN/Inf in foreign function call (arg 55)
# then there are non-finite numbers in data.  Use this to fix
# nf <- which(!is.finite(ppomax[[seas]]$data)) 
# ppomax[[seas]]$data[nf] <- ppomax[[seas]]$bmdi[1]
#

o2 <- .C("c_write_pp_file",as.character(fname),
					as.integer(pp1$nfields),
					as.integer(pp1$lbyr),
					as.integer(pp1$lbmon),
					as.integer(pp1$lbdat),
					as.integer(pp1$lbhr),
					as.integer(pp1$lbmin),
					as.integer(pp1$lbday),
					as.integer(pp1$lbyrd),
					as.integer(pp1$lbmond),
					as.integer(pp1$lbdatd),
					as.integer(pp1$lbhrd),
					as.integer(pp1$lbmind),
					as.integer(pp1$lbdayd),
					as.integer(pp1$lbtim),
					as.integer(pp1$lbft),
					as.integer(pp1$lblrec),
					as.integer(pp1$lbcode),
					as.integer(pp1$lbhem),
					as.integer(pp1$lbrow),
					as.integer(pp1$lbnpt),
					as.integer(pp1$lbext),
					as.integer(pp1$lbpack),
					as.integer(pp1$lbrel),
					as.integer(pp1$lbfc),
					as.integer(pp1$lbcfc),
					as.integer(pp1$lbproc),
					as.integer(pp1$lbvc),
					as.integer(pp1$lbrvc),
					as.integer(pp1$lbexp),
					as.integer(pp1$lbegin),
					as.integer(pp1$lbnrec),
					as.integer(pp1$lbproj),
					as.integer(pp1$lbtyp),
					as.integer(pp1$lblev),
					as.integer(pp1$lbrsvd),
					as.integer(pp1$lbsrce),
					as.integer(pp1$lbuser),
					as.single(pp1$brsvd),
					as.single(pp1$bdatum),
					as.single(pp1$bacc),
					as.single(pp1$blev),
					as.single(pp1$brlev),
					as.single(pp1$bhlev),
					as.single(pp1$bhrlev),
					as.single(pp1$bplat),
					as.single(pp1$bplon),
					as.single(pp1$bgor),
					as.single(pp1$bzy),
					as.single(pp1$bdy),
					as.single(pp1$bzx),
					as.single(pp1$bdx),
					as.single(pp1$bmdi),
					as.single(pp1$bmks),
					as.single(pp1$data),
					as.single(pp1$buf1),
					as.single(pp1$buf2),
					as.single(pp1$buf3),
					as.single(pp1$buf4)
					)

dyn.unload("/home/h03/hadsx/extremes/R/C/C_pp.sl")
}


#######################################################################

clone.pp <- function(pp1, nfields) {

# from the pp field passed in create pp structure n fields long inheriting as much as 
# possible from input field.

int.dummy <- 0
dbl.dummy <- 0
str.dummy <- "noname"
npt  <- pp1$lbnpt[1]
nrow <- pp1$lbrow[1]

#a <- array(dbl.dummy,dim=c(npt,nrow,nfields))
#a <- 1

pp2 <- list(
					filename=as.character(str.dummy),
					nfields=as.integer(nfields),
					lbyr=as.integer(rep(pp1$lbyr[1], times=nfields)),
					lbmon=as.integer(rep(pp1$lbmon[1], times=nfields)),
					lbdat=as.integer(rep(pp1$lbdat[1], times=nfields)),
					lbhr=as.integer(rep(pp1$lbhr[1], times=nfields)),
					lbmin=as.integer(rep(pp1$lbmin[1], times=nfields)),
					lbday=as.integer(rep(pp1$lbday[1], times=nfields)),
					lbyrd=as.integer(rep(pp1$lbyrd[1], times=nfields)),
					lbmond=as.integer(rep(pp1$lbmond[1], times=nfields)),
					lbdatd=as.integer(rep(pp1$lbdatd[1], times=nfields)),
					lbhrd=as.integer(rep(pp1$lbhrd[1], times=nfields)),
					lbmind=as.integer(rep(pp1$lbmind[1], times=nfields)),
					lbdayd=as.integer(rep(pp1$lbdayd[1], times=nfields)),
					lbtim=as.integer(rep(pp1$lbtim[1], times=nfields)),
					lbft=as.integer(rep(pp1$lbft[1], times=nfields)),
					lblrec=as.integer(rep(pp1$lblrec[1], times=nfields)),
					lbcode=as.integer(rep(pp1$lbcode[1], times=nfields)),
					lbhem=as.integer(rep(pp1$lbhem[1], times=nfields)),
					lbrow=as.integer(rep(pp1$lbrow[1], times=nfields)),
					lbnpt=as.integer(rep(pp1$lbnpt[1], times=nfields)),
					lbext=as.integer(rep(pp1$lbext[1], times=nfields)),
					lbpack=as.integer(rep(pp1$lbpack[1], times=nfields)),
					lbrel=as.integer(rep(pp1$lbrel[1], times=nfields)),
					lbfc=as.integer(rep(pp1$lbfc[1], times=nfields)),
					lbcfc=as.integer(rep(pp1$lbcfc[1], times=nfields)),
					lbproc=as.integer(rep(pp1$lbproc[1], times=nfields)),
					lbvc=as.integer(rep(pp1$lbvc[1], times=nfields)),
					lbrvc=as.integer(rep(pp1$lbrvc[1], times=nfields)),
					lbexp=as.integer(rep(pp1$lbexp[1], times=nfields)),
					lbegin=as.integer(rep(pp1$lbegin[1], times=nfields)),
					lbnrec=as.integer(rep(pp1$lbnrec[1], times=nfields)),
					lbproj=as.integer(rep(pp1$lbproj[1], times=nfields)),
					lbtyp=as.integer(rep(pp1$lbtyp[1], times=nfields)),
					lblev=as.integer(rep(pp1$lblev[1], times=nfields)),
					
					lbrsvd=as.integer(rep(pp1$lbrsvd[1:4,1], times=nfields)),
					
					lbsrce=as.integer(rep(pp1$lbsrce[1], times=nfields)),

					lbuser=as.integer(rep(pp1$lbuser[1:7,1], times=nfields)),

					brsvd=as.single(rep(pp1$brsvd[1:4,1], times=nfields)),

					bdatum=as.single(rep(pp1$bdatum[1], times=nfields)),
					bacc=as.single(rep(pp1$bacc[1], times=nfields)),
					blev=as.single(rep(pp1$blev[1], times=nfields)),
					brlev=as.single(rep(pp1$brlev[1], times=nfields)),
					bhlev=as.single(rep(pp1$bhlev[1], times=nfields)),
					bhrlev=as.single(rep(pp1$bhrlev[1], times=nfields)),
					bplat=as.single(rep(pp1$bplat[1], times=nfields)),
					bplon=as.single(rep(pp1$bplon[1], times=nfields)),

					bgor=as.single(rep(pp1$bgor[1], times=nfields)),
					bzy=as.single(rep(pp1$bzy[1], times=nfields)),
					bdy=as.single(rep(pp1$bdy[1], times=nfields)),
					bzx=as.single(rep(pp1$bzx[1], times=nfields)),
					bdx=as.single(rep(pp1$bdx[1], times=nfields)),
					bmdi=as.single(rep(pp1$bmdi[1], times=nfields)),
					bmks=as.single(rep(pp1$bmks[1], times=nfields)),
					
					data=as.single(array(dbl.dummy,dim=c(npt,nrow,nfields))),

					buf1=as.integer(rep(pp1$buf1[1], times=nfields)),
					buf2=as.integer(rep(pp1$buf2[1], times=nfields)),
					buf3=as.integer(rep(pp1$buf3[1], times=nfields)),
					buf4=as.integer(rep(pp1$buf4[1], times=nfields))
					)

dim(pp2$data)   <- c(npt,nrow,nfields)
dim(pp2$lbrsvd) <- c(4,nfields)
dim(pp2$lbuser) <- c(7,nfields)
dim(pp2$brsvd)  <- c(4,nfields)

pp2

}


#######################################################################

extract.fields.pp <- function(pp1, field.index) {

# from the pp field passed in create pp structure length(field.index) fields long and paste in data from fields selected in the field.index.

int.dummy <- 0
dbl.dummy <- 0
str.dummy <- "noname"
npt     <- pp1$lbnpt[1]
nrow    <- pp1$lbrow[1]
nfields <- length(field.index)


pp2 <- list(
					filename=as.character(str.dummy),
					nfields=as.integer(nfields),
					lbyr=as.integer(rep(pp1$lbyr[1], times=nfields)),
					lbmon=as.integer(rep(pp1$lbmon[1], times=nfields)),
					lbdat=as.integer(rep(pp1$lbdat[1], times=nfields)),
					lbhr=as.integer(rep(pp1$lbhr[1], times=nfields)),
					lbmin=as.integer(rep(pp1$lbmin[1], times=nfields)),
					lbday=as.integer(rep(pp1$lbday[1], times=nfields)),
					lbyrd=as.integer(rep(pp1$lbyrd[1], times=nfields)),
					lbmond=as.integer(rep(pp1$lbmond[1], times=nfields)),
					lbdatd=as.integer(rep(pp1$lbdatd[1], times=nfields)),
					lbhrd=as.integer(rep(pp1$lbhrd[1], times=nfields)),
					lbmind=as.integer(rep(pp1$lbmind[1], times=nfields)),
					lbdayd=as.integer(rep(pp1$lbdayd[1], times=nfields)),
					lbtim=as.integer(rep(pp1$lbtim[1], times=nfields)),
					lbft=as.integer(rep(pp1$lbft[1], times=nfields)),
					lblrec=as.integer(rep(pp1$lblrec[1], times=nfields)),
					lbcode=as.integer(rep(pp1$lbcode[1], times=nfields)),
					lbhem=as.integer(rep(pp1$lbhem[1], times=nfields)),
					lbrow=as.integer(rep(pp1$lbrow[1], times=nfields)),
					lbnpt=as.integer(rep(pp1$lbnpt[1], times=nfields)),
					lbext=as.integer(rep(pp1$lbext[1], times=nfields)),
					lbpack=as.integer(rep(pp1$lbpack[1], times=nfields)),
					lbrel=as.integer(rep(pp1$lbrel[1], times=nfields)),
					lbfc=as.integer(rep(pp1$lbfc[1], times=nfields)),
					lbcfc=as.integer(rep(pp1$lbcfc[1], times=nfields)),
					lbproc=as.integer(rep(pp1$lbproc[1], times=nfields)),
					lbvc=as.integer(rep(pp1$lbvc[1], times=nfields)),
					lbrvc=as.integer(rep(pp1$lbrvc[1], times=nfields)),
					lbexp=as.integer(rep(pp1$lbexp[1], times=nfields)),
					lbegin=as.integer(rep(pp1$lbegin[1], times=nfields)),
					lbnrec=as.integer(rep(pp1$lbnrec[1], times=nfields)),
					lbproj=as.integer(rep(pp1$lbproj[1], times=nfields)),
					lbtyp=as.integer(rep(pp1$lbtyp[1], times=nfields)),
					lblev=as.integer(rep(pp1$lblev[1], times=nfields)),
					
					lbrsvd=as.integer(rep(pp1$lbrsvd[1:4,1], times=nfields)),
					
					lbsrce=as.integer(rep(pp1$lbsrce[1], times=nfields)),

					lbuser=as.integer(rep(pp1$lbuser[1:7,1], times=nfields)),

					brsvd=as.single(rep(pp1$brsvd[1:4,1], times=nfields)),

					bdatum=as.single(rep(pp1$bdatum[1], times=nfields)),
					bacc=as.single(rep(pp1$bacc[1], times=nfields)),
					blev=as.single(rep(pp1$blev[1], times=nfields)),
					brlev=as.single(rep(pp1$brlev[1], times=nfields)),
					bhlev=as.single(rep(pp1$bhlev[1], times=nfields)),
					bhrlev=as.single(rep(pp1$bhrlev[1], times=nfields)),
					bplat=as.single(rep(pp1$bplat[1], times=nfields)),
					bplon=as.single(rep(pp1$bplon[1], times=nfields)),

					bgor=as.single(rep(pp1$bgor[1], times=nfields)),
					bzy=as.single(rep(pp1$bzy[1], times=nfields)),
					bdy=as.single(rep(pp1$bdy[1], times=nfields)),
					bzx=as.single(rep(pp1$bzx[1], times=nfields)),
					bdx=as.single(rep(pp1$bdx[1], times=nfields)),
					bmdi=as.single(rep(pp1$bmdi[1], times=nfields)),
					bmks=as.single(rep(pp1$bmks[1], times=nfields)),
					
					data=as.single(array(dbl.dummy,dim=c(npt,nrow,nfields))),

					buf1=as.integer(rep(pp1$buf1[1], times=nfields)),
					buf2=as.integer(rep(pp1$buf2[1], times=nfields)),
					buf3=as.integer(rep(pp1$buf3[1], times=nfields)),
					buf4=as.integer(rep(pp1$buf4[1], times=nfields))
					)

dim(pp2$data)   <- c(npt,nrow,nfields)
dim(pp2$lbrsvd) <- c(4,nfields)
dim(pp2$lbuser) <- c(7,nfields)
dim(pp2$brsvd)  <- c(4,nfields)

# past in data to pp2

pp2$filename <- pp1$filename
pp2$nfields <- pp1$nfields
pp2$lbyr <- pp1$lbyr[field.index]
pp2$lbmon <- pp1$lbmon[field.index]
pp2$lbdat <- pp1$lbdat[field.index]
pp2$lbhr <- pp1$lbhr[field.index]
pp2$lbmin <- pp1$lbmin[field.index]
pp2$lbday <- pp1$lbday[field.index]
pp2$lbyrd <- pp1$lbyrd[field.index]
pp2$lbmond <- pp1$lbmond[field.index]
pp2$lbdatd <- pp1$lbdatd[field.index]
pp2$lbhrd <- pp1$lbhrd[field.index]
pp2$lbmind <- pp1$lbmind[field.index]
pp2$lbdayd <- pp1$lbdayd[field.index]
pp2$lbtim <- pp1$lbtim[field.index]
pp2$lbft <- pp1$lbft[field.index]
pp2$lblrec <- pp1$lblrec[field.index]
pp2$lbcode <- pp1$lbcode[field.index]
pp2$lbhem <- pp1$lbhem[field.index]
pp2$lbrow <- pp1$lbrow[field.index]
pp2$lbnpt <- pp1$lbnpt[field.index]
pp2$lbext <- pp1$lbext[field.index]
pp2$lbpack <- pp1$lbpack[field.index]
pp2$lbrel <- pp1$lbrel[field.index]
pp2$lbfc <- pp1$lbfc[field.index]
pp2$lbcfc <- pp1$lbcfc[field.index]
pp2$lbproc <- pp1$lbproc[field.index]
pp2$lbvc <- pp1$lbvc[field.index]
pp2$lbrvc <- pp1$lbrvc[field.index]
pp2$lbexp <- pp1$lbexp[field.index]
pp2$lbegin <- pp1$lbegin[field.index]
pp2$lbnrec <- pp1$lbnrec[field.index]
pp2$lbproj <- pp1$lbproj[field.index]
pp2$lbtyp <- pp1$lbtyp[field.index]
pp2$lblev <- pp1$lblev[field.index]
pp2$lbrsvd <- pp1$lbrsvd[,field.index]
pp2$lbsrce <- pp1$lbsrce[field.index]
pp2$lbuser <- pp1$lbuser[,field.index]
pp2$brsvd <- pp1$brsvd[,field.index]

pp2$bdatum <- pp1$bdatum[field.index]
pp2$bacc <- pp1$bacc[field.index]
pp2$blev <- pp1$blev[field.index]
pp2$brlev <- pp1$brlev[field.index]
pp2$bhlev <- pp1$bhlev[field.index]
pp2$bhrlev <- pp1$bhrlev[field.index]
pp2$bplat <- pp1$bplat[field.index]
pp2$bplon <- pp1$bplon[field.index]
pp2$bgor <- pp1$bgor[field.index]
pp2$bzy <- pp1$bzy[field.index]
pp2$bdy <- pp1$bdy[field.index]
pp2$bzx <- pp1$bzx[field.index]
pp2$bdx <- pp1$bdx[field.index]
pp2$bmdi <- pp1$bmdi[field.index]
pp2$bmks <- pp1$bmks[field.index]

pp2$data <- pp1$data[,,field.index]

pp2$buf1 <- pp1$buf1[field.index]
pp2$buf2 <- pp1$buf2[field.index]
pp2$buf3 <- pp1$buf3[field.index]
pp2$buf4 <- pp1$buf4[field.index]

# bodge to fix the dropping of dim of length 1
dim(pp2$data)   <- c(npt,nrow,nfields)
dim(pp2$lbrsvd) <- c(4,nfields)
dim(pp2$lbuser) <- c(7,nfields)
dim(pp2$brsvd)  <- c(4,nfields)

pp2

}
#######################################################################

inquire.pp <- function(pp.file, quiet=TRUE) {

dyn.load("/home/h03/hadsx/extremes/R/C/C_pp.sl")

# test to see if file is gzipped, if so copy to localdata/tmp_R and unzip
is.gz <- is.finite(grep('.gz',pp.file)[1])
if(is.gz[1]>0) {
	# test if file has already been unzipped
	pp.file.nogz <- strsplit(basename(pp.file),'.gz')[[1]]
	cmd1 <- paste('ls ','/data/local/hadsx/tmp_R/',pp.file.nogz,sep='')
	a1   <- system(cmd1,TRUE,TRUE)
	if(is.na(a1[1])) { # file is not unzipped so unzip
		cat('GZIPPED FILE.  Unzipping to /data/local/hadsx/tmp_R \n')
		cmd2 <- paste('mkdir','/data/local/hadsx/tmp_R',sep=' ')
		a2   <- system(cmd2,TRUE,TRUE)
		cmd3 <- paste('cp',pp.file,'/data/local/hadsx/tmp_R',sep=' ')
		a3   <- system(cmd3,TRUE,TRUE)
		cmd4 <- paste('gunzip ','/data/local/hadsx/tmp_R/',basename(pp.file),sep='')
		a4   <- system(cmd4,TRUE,TRUE)
	}
	cmd5 <- 	paste('echo ','/data/local/hadsx/tmp_R/',basename(pp.file.nogz),sep='')
	pp.file <-system(cmd5,TRUE,TRUE)
}


# count fields and return info

o1 <- .C("c_inquire_pp_file",as.character(pp.file),
				count=integer(1),nrow=integer(1), npt=integer(1), quiet=quiet)

count <- o1$count
nrow  <- o1$nrow
npt   <- o1$npt

dyn.unload("/home/h03/hadsx/extremes/R/C/C_pp.sl")
					
list(nfields=count,lbrow=nrow,lbnpt=npt)

}

#######################################################################

clean.pp.tmp_R <- function() {

	cat('REMOVING ALL .pp files from /data/local/hadsx/tmp_R \n')
	cmd1 <- paste('rm','/data/local/hadsx/tmp_R/*.pp',sep=' ')
	a1   <- system(cmd1,TRUE,TRUE)

}

