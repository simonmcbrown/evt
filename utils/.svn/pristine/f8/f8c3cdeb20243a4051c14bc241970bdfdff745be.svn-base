# Package to make plots showing data from a PP file

#pp.map.internal.wm <- map('world',interior=FALSE,plot=FALSE)
#is.na(pp.map.internal.wm$x[8836])=T  # Remove Antarctic southern line 
                
# Pair of maps, one above the other
pp.pmap<-function(pp1,pp2,...) {
  grid.newpage()
    pushViewport(viewport(x=0,y=0.5,width=1,height=0.5,just=c('left','bottom')))
     m1<-pp.map(pp1,...,draw=F)
     print(m1,newpage=F)
  popViewport()
  pushViewport(viewport(x=0,y=0,width=1,height=0.5,just=c('left','bottom')))
     m2<-pp.map(pp2,...,draw=F)
     print(m2,newpage=F)
  popViewport()
}

pp.map <-function(pp,palette="diverging",ncols=17,levels=NA,
                  draw=TRUE,
                  lat_range=NA,
                  lat_scale=NA,
                  base_lon=0,lon_range=NA,
                  lon_scale=NA,
                  contour=FALSE,region=TRUE,pretty=FALSE) {


  lats<-pp.get.lats(pp)
  if(length(lat_scale)==1) lat_scale=pretty(lats)
  longs<-pp.get.longs(pp)
  if(length(lon_scale)==1) lon_scale=pretty(longs)
  
    # Guess the base longitude if unspecified
    if(is.na(base_lon)) {
    	range = max(longs,na.rm=T)-min(longs,na.rm=T)
        longs_s = longs
        for(i in seq(1,length(longs_s))) {
            if (!is.na(longs_s[i]) && longs_s[i]< 0) {
                longs_s[i] = longs_s[i]+360
            }
        }
	if(max(longs_s,na.rm=T)-min(longs_s,na.rm=T)<range) base_lon=180
	else base_lon=0
    }
	    
    # Get continental outline data and set to the chosen base latitude
    pp.map.internal.wm <- map('world',interior=FALSE,plot=FALSE)
    is.na(pp.map.internal.wm$x[8836])=T  # Remove Antarctic bug
  
    # If the field has a rotated pole, rotate the continent outlines to match
    # Strictly bplat==0 indicates a rotated pole, but usually this is not so.
    if((pp@bplat !=90 && pp@bplat!=0) || pp@bplon!=0) {
        for(i in seq(1,length(pp.map.internal.wm$x))) {
          nl<-pp.pole.to.pole(pp.map.internal.wm$y[i],pp.map.internal.wm$x[i],
                              90,0,pp@bplat,pp@bplon)
           pp.map.internal.wm$y[i]<-nl[1]
           pp.map.internal.wm$x[i]<-nl[2]
           if(i>1 && !is.na(pp.map.internal.wm$x[i]) 
                   && !is.na(pp.map.internal.wm$x[i-1])
                   && abs(pp.map.internal.wm$x[i]-pp.map.internal.wm$x[i-1])>50) {
                is.na(pp.map.internal.wm$x[i])=T
           } # Remove any polygons which are distorted by the change in pole
        }
    }
  
    longs_s = longs  
    if(base_lon != 0) {
        for(i in seq(1,length(pp.map.internal.wm$x))) {
            if(!is.na(pp.map.internal.wm$x[i])) {
                if (pp.map.internal.wm$x[i]< base_lon-180) {
                    pp.map.internal.wm$x[i] = pp.map.internal.wm$x[i]+360
                }
                if (pp.map.internal.wm$x[i]> base_lon+180) {
                    pp.map.internal.wm$x[i] = pp.map.internal.wm$x[i]-360
                }
            }
            if(i>1 && !is.na(pp.map.internal.wm$x[i]) 
                   && !is.na(pp.map.internal.wm$x[i-1])
                   && abs(pp.map.internal.wm$x[i]-pp.map.internal.wm$x[i-1])>50) {
                is.na(pp.map.internal.wm$x[i])=T
            }
        }
        # Correct the data longitudes for the base
        for(i in seq(1,length(longs_s))) {
            if (longs_s[i]< base_lon-180) {
                longs_s[i] = longs_s[i]+360
            }
            if (longs_s[i]> base_lon+180) {
                longs_s[i] = longs_s[i]-360
            }
        }
    }

      
    # Set default ranges
    if(is.na(lon_range)) lon_range=c(max(base_lon-180,min(longs_s,na.rm=T)-0.01),
                                     min(base_lon+180,max(longs_s,na.rm=T)+0.01))
    if(is.na(lat_range)) lat_range=c(max(-90,min(lats,na.rm=T)-0.01),
                                     min(90,max(lats,na.rm=T)+0.01))

  mappanel <- function(x,y,...) {
      panel.contourplot(x,y,...)
      llines(pp.map.internal.wm$x,pp.map.internal.wm$y,col="black")
  }

  c<-0
  if(length(levels)>1) {
      ncols<-length(levels)
      c<-contourplot(pp@data ~ longs_s * lats,
         ylab="Latitude",xlab="Longitude",
         xlim=lon_range,
         ylim=lat_range,
         scales=list(x=list(at=lon_scale),
                     y=list(at=lat_scale)
                    ),
         panel=mappanel,
         aspect="iso",
         region=region,
         contour=contour,
         pretty=pretty,
         cuts=ncols-1,
         at=levels,
         col.regions=switch(palette,
             diverging=pp.map.internal.getPalette.diverging(ncols),
             sequential=pp.map.internal.getPalette.sequential(ncols),
             greyscale=pp.map.internal.getPalette.greyscale(ncols))
      )
  }
  else {
      c<-contourplot(pp@data ~ longs_s * lats,
         ylab="Latitude",xlab="Longitude",
         xlim=lon_range,
         ylim=lat_range,
         scales=list(x=list(at=lon_scale),
                     y=list(at=lat_scale)
                    ),
         panel=mappanel,
         aspect="iso",
         region=region,
         contour=contour,
         pretty=pretty,
         cuts=ncols-1,
         col.regions=switch(palette,
             diverging=pp.map.internal.getPalette.diverging(ncols),
             sequential=pp.map.internal.getPalette.sequential(ncols),
             greyscale=pp.map.internal.getPalette.greyscale(ncols))
      )
  }
  if(draw) { print(c) }
  else {return(c)}

}
    

# Use the Light and Bartlein sequential and diverging colour schemes
pp.map.internal.getPalette.diverging<-colorRampPalette(
       c(
        rgb( 36,  0,   216 ,maxColorValue = 255 ),
        rgb( 24,  28,  247 ,maxColorValue = 255 ),
        rgb( 40,  87,  255 ,maxColorValue = 255 ),
        rgb( 61,  135, 255 ,maxColorValue = 255 ),
        rgb( 86,  176, 255 ,maxColorValue = 255 ),
        rgb( 117, 211, 255 ,maxColorValue = 255 ),
        rgb( 153, 234, 255 ,maxColorValue = 255 ),
        rgb( 188, 249, 255 ,maxColorValue = 255 ),
        rgb( 234, 255, 255, maxColorValue = 255 ),
        rgb( 255, 255, 234, maxColorValue = 255 ),
        rgb( 255, 241, 188 ,maxColorValue = 255 ),
        rgb( 255, 214, 153 ,maxColorValue = 255 ),
        rgb( 255, 172, 117 ,maxColorValue = 255 ),
        rgb( 255, 120,  86 ,maxColorValue = 255 ),
        rgb( 255,  61,  61 ,maxColorValue = 255 ),
        rgb( 247,  39,  53 ,maxColorValue = 255 ),
        rgb( 216,  21,  47 ,maxColorValue = 255 ),
        rgb( 165,   0,  33 ,maxColorValue = 255 )
       )
   )

pp.map.internal.getPalette.sequential<-colorRampPalette(
       c(
         rgb( 229, 255, 255 ,maxColorValue = 255 ),
         rgb( 204, 250, 255 ,maxColorValue = 255 ),
         rgb( 178, 242, 255 ,maxColorValue = 255 ),
         rgb( 153, 229, 255 ,maxColorValue = 255 ),
         rgb( 127, 212, 255 ,maxColorValue = 255 ),
         rgb( 101, 191, 255 ,maxColorValue = 255 ),
         rgb( 76,  165, 255 ,maxColorValue = 255 ),
         rgb( 50,  136, 255 ,maxColorValue = 255 ),
         rgb( 25,  101, 255 ,maxColorValue = 255 ),
         rgb( 0,   63,  255 ,maxColorValue = 255 )
        )
   )
   
pp.map.internal.getPalette.greyscale<-colorRampPalette(
       c(
         rgb( 192, 192, 192 ,maxColorValue = 255 ),
         rgb( 160, 160, 160 ,maxColorValue = 255 ),
         rgb( 128, 128, 128 ,maxColorValue = 255 ),
         rgb(  96,  96,  96 ,maxColorValue = 255 ),
         rgb(  64,  64,  64 ,maxColorValue = 255 ),
         rgb(  32,  32,  32 ,maxColorValue = 255 ),
         rgb(   0,   0,   0 ,maxColorValue = 255 )
        )
   )

