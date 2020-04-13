# Use the OBIS R package

setwd("C:/daten/r/obis")


library(robis)

# Retrieve occurrence records after checking the appropriate taxonomic name
# Example: You are interested in kelp and want to test the hypothesis that kelp and reef coral occurrences are mutually exclusive
# because of different ecological constraints

# Kelp is an informal term. With Google (Wikipedia) you get the info that Kelp are brown algae in the order Laminariales

 kelp <- occurrence(scientificname = "Laminariales") # takes two minutes and retreives 48k occurrences
 # Save as RData object (faster save and load than csv files)
 save(kelp, file ="Kelp.RData")
 
 # You can get the coral data also
# records <- occurrence(scientificname = "Scleractinia") # retrieve records directly from OBIS
 
 # But better use a cleaned file of reef corals that WK has produced
 load("ReefCorals.Rdata")

 
# Now show occurrences on maps
 
 library(sp) # for mapping
 library(rgdal) # also 
 w <- readOGR("d:/shapefiles/TM_WORLD_BORDERS_SIMPL-0.3.shp", "TM_WORLD_BORDERS_SIMPL-0.3") # 
 
 
 # Plot a control map
 # Kelp data
 x1 <- kelp$decimalLongitude
 y1 <- kelp$decimalLatitude
 
 # Coral data
 x2 <- reef_corals$decimalLongitude
 y2 <- reef_corals$decimalLatitude
 
 # Individual points
 yl <- c(-90, 90)
 op <- par(xaxs="i", yaxs="i", mar=c(5,4.5,2,1))
 #  par(mar=c(5,4.5,2,0))
 plot(0,0, xlim=c(-180,180), ylim=yl, type="n", xlab="Longitude",
      ylab="Latitude", cex.lab=1.4)
 plot(w, col="grey80", add=T, border=NA)
 abline(h=0, lty=2, col="grey")
 abline(h= -23.433, lty=2, col="grey"); abline(h= 23.433, lty=2, col="grey")
 # plot simple points
 # points(cols$collections.lngdec, cols$collections.latdec
 #        , pch=19, col="red", cex=1.2)
 # plot occurrence counts per collection instead
 points(x1, y1, pch=19, col="brown", cex=0.4)
 points(x2, y2, pch=19, col="lightblue", cex=0.4)
 box()
 
 # Rasterize the data
 library(raster)
 r <- raster(resolution=5)
 cr <- subset(reef_corals, select=c(decimalLongitude, decimalLatitude))
 ke <- subset(kelp, select=c(decimalLongitude, decimalLatitude))
 
 crr <- rasterize(cr, r, fun=function(x,...)length(x))
 ker <- rasterize(ke, r, fun=function(x,...)length(x))
 
 # Rasterized
 windows(h=6, w=12)
 op <- par(xaxs="i", yaxs="i", mar=c(4,3,2,7))
 
 plot(0,0, xlim=c(-180,180), ylim=yl, type="n", xlab="Longitude",
      ylab="Latitude", cex.lab=1.4)
 plot(w, col="grey80", add=T, border=NA)
 plot(crr, col=rev(heat.colors(10)), add=T, 	legend.args=list(text='reef corals',                                                  	
                                                              side=3, font=2, line=0.5, adj=0.5))
 
 
 plot(0,0, xlim=c(-180,180), ylim=yl, type="n", xlab="Longitude",
      ylab="Latitude", cex.lab=1.4)
 plot(w, col="grey80", add=T, border=NA)
 plot(ker, col=rev(heat.colors(10)), add=T, 	legend.args=list(text='kelp',                                                  	
                                                              side=3, font=2, line=0.5, adj=0.5))
 
 par(op)
 
 # Check options with raster create plots of ker - crr and so on ...