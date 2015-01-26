# source("heatwave_model_into_grid_pp.R")

# read in the individual point fits and gridd desired parameters

source("/home/h03/hadsx/extremes/R/libs/pp_io.R")

stpp  <- '/net/home/h03/hadsx/extremes/tawn/hugo/heatwave/europe_bigger.pp'
sthw0 <- '/net/home/h03/hadsx/extremes/tawn/hugo/time_mk1/hw_models/gem2/ant/'
sthw1 <- 'DurSevFromObs_ajayoa_'
stout <- '/net/home/h03/hadsx/extremes/tawn/hugo/time_mk1/hw_models/gem2/'

pp1 <- read.pp(stpp)
pp1$data[,,] <- NA
pp4 <- clone.pp(pp1, 4)

flist <- list.files(path=sthw0,pattern=sthw1, full.names=TRUE)

for (f in flist) {

    fname <- strsplit(basename(f),'_',fixed=T)[[1]]
    x1    <- as.integer(substr(fname[length(fname)],2,5))
    if (x1 != 21 ) {
        if (x1 >185) x1 <- x1 - 185 else x1 <- x1 + 7
        #if (x1 >185) x1 <- x1 - 185 else x1 <- x1 + 6
        y1    <- as.integer(substr(fname[length(fname)-1],2,5))
        y1    <- y1 - 99

        cat('Loading ',y1,x1,basename(f),cr)
        load(f)
        
        pp4$data[x1,y1,1:2] <- ht.dep.fit$par[1:2]
        pp4$data[x1,y1,3:4] <- ht.dep.fit.bwd$par[1:2]
    } 
}

fcing   <- tail(strsplit(sthw0,'/',fixed=T)[[1]],1)
stppout <- paste(stout,sthw1,fcing,'_europe_bigger.pp',sep='')
write.pp(pp4, stppout)






