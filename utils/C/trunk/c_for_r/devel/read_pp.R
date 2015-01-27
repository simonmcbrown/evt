read.pp <- function(pp.file) {

dyn.load("/home/hc1400/hadsx/extremes/R/C/C_pp.sl")

# count fields and make pp structure to fit

o1 <- .C("c_inquire_pp_file",as.character(pp.file),
				count=integer(1),nrow=integer(1), npt=integer(1))
count <- o1$count
nrow  <- o1$nrow
npt   <- o1$npt

int.dummy <- 0
dbl.dummy <- 0

o2 <- .C("c_read_pp_file",filename=as.character(pp.file),
					nfields=as.integer(count),
					LBYR=integer(count),
					LBMON=integer(count),
					LBDAT=integer(count),
					LBHR=integer(count),
					LBMIN=integer(count),
					LBDAY=integer(count),
					LBYRD=integer(count),
					LBMOND=integer(count),
					LBDATD=integer(count),
					LBHRD=integer(count),
					LBMIND=integer(count),
					LBDAYD=integer(count),
					LBTIM=integer(count),
					LBFT=integer(count),
					LBLREC=integer(count),
					LBCODE=integer(count),
					LBHEM=integer(count),
					LBROW=integer(count),
					LBNPT=integer(count),
					LBEXT=integer(count),
					LBPACK=integer(count),
					LBREL=integer(count),
					LBFC=integer(count),
					LBCFC=integer(count),
					LBPROC=integer(count),
					LBVC=integer(count),
					LBRVC=integer(count),
					LBEXP=integer(count),
					LBEGIN=integer(count),
					LBNREC=integer(count),
					LBPROJ=integer(count),
					LBTYP=integer(count),
					LBLEV=integer(count),
					LBRSVD=as.integer(array(int.dummy,dim=c(4,count))),
					LBSRCE=integer(count),
					LBUSER=as.integer(array(int.dummy,dim=c(7,count))),
					BRSVD=as.single(array(dbl.dummy,dim=c(4,count))),
					BDATUM=single(count),
					BACC=single(count),
					BLEV=single(count),
					BRLEV=single(count),
					BHLEV=single(count),
					BHRLEV=single(count),
					BPLAT=single(count),
					BPLON=single(count),
					BGOR=single(count),
					BZY=single(count),
					BDY=single(count),
					BZX=single(count),
					BDX=single(count),
					BMDI=single(count),
					BMKS=single(count),
					data=as.single(array(dbl.dummy,dim=c(npt,nrow,count))),
					BUF1=integer(count),
					BUF2=integer(count),
					BUF3=integer(count),
					BUF4=integer(count)
					)

dyn.unload("/home/hc1400/hadsx/extremes/R/C/C_pp.sl")
					
dim(o2$data) <- c(npt,nrow,count)

o2

}
