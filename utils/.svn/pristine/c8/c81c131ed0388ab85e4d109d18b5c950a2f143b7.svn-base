# read data into created structure

output <- .C("c_read_pp_file",as.character(pp.file),
					as.integer(count),as.integer(nrow), as.integer(npt),
					BUF1=integer(count),
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
					BUF2=single(count),
					BUF3=single(count),
					data=as.single(array(dbl.dummy,dim=c(npt,nrow,count))),
					BUF4=single(count) )
					
					#[[c(1,4:60)]]
					
# make list f the header and data elements


#  return pp object

















#dyn.load("C/C_for_R.sl")

pp.file <- "/data/cr1/hadsx/reference/cm3_field_x5.pp"

int.dummy = 0
dbl.dummy = 0.0

# count fields and make pp structure to fit

#o1      <- .C("c_inquire_pp_file",as.character(pp.file),count=integer(1),nrow=integer(1), npt=integer(1))
o1 <- .C("c_inquire_pp_file",as.character(pp.file),
				count=integer(1),nrow=integer(1), npt=integer(1))
count <- o1$count
nrow  <- o1$nrow
npt   <- o1$npt

print(c("count was",count))


o2 <- .C("c_read_pp_file",as.character(pp.file),
					BUF1=integer(count),
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
					BUF2=single(count),
					BUF3=single(count),
					data=as.single(array(dbl.dummy,dim=c(npt,nrow,count))),
					BUF4=single(count) )
					
