# Analyze Grid similarities

library(Hmisc) # for error bars

# This for Jurassic 5

  myData <- read.table(file="D:/R/Jura5occs.csv", header=TRUE, sep=";", quote="\"") # alternative approach


  # Delete duplicate genus names within collections
   myData <- unique(myData) 
  # Run all groups
   dat <- myData
  # Limit to bivalves
   dat <- subset(myData, occurrences.class_name=="Anthozoa")
   
# Start loop for bins
 # Summarize data to 5 degree grids
   lat <- dat$collections.paleolatdec
   lon <- dat$collections.paleolngdec

  # Occurrence matrix in 5 degree grids
   o <- data.frame()

  for (y in seq(from=87.5, to=-87.5, by=-5)) {
    for (x in seq(from=-177.5, to=177.5, by=5)) {
      gens <- table(dat$occurrences.genus_name[lat<y+2.5 & lat>=y-2.5 & lon>x-2.5 & lon<x+2.5])
   #  set threshold to 50 occurrences
       if (sum(gens) >= 50) { 
         gens <- gens[gens>0]
         gens <- as.data.frame(gens)
           Gen <- row.names(gens)
         o_gen <- cbind(Gen, gens, y, x)
         o <- rbind(o, o_gen)
       }
     }
    }

    colnames(o) <- c("Genus", "Freq", "Lat", "Long")

######
  # Calculate similarities
    # Unique grids
    t1 <- unique(subset(o, select=c(Lat, Long)))
  
  res <- data.frame()
    

   if (nrow(t1)>1)    {
   # Condition to avoid error message
  for (i in 1:(nrow(t1)-1)) {
       lat1 <- t1$Lat[i]
       lon1 <- t1$Long[i]
        o1 <- subset(o, Lat==lat1 & Long==lon1)
         so1 <- sum(o1$Freq) # sum of occurrences
         go1 <- nrow(o1) # number of occurrences
     for (j in (i+1):nrow(t1)) {
     	   lat2 <- t1$Lat[j]
         lon2 <- t1$Long[j]
         o2 <- subset(o, Lat==lat2 & Long==lon2)
          so2 <- sum(o2$Freq) # sum of occurrences
          go2 <- nrow(o2) # number of occurrences
     
    # Calculate similarity
     com1 <- o1$Genus %in% o2$Genus # common in both collections with positions in first
       com1 <- subset(o1, com1==TRUE)
        p1 <- com1$Freq/so1 # for percent similarity
        common <- length(p1) # For Jaccard - only needed once
     com2 <- o2$Genus %in% o1$Genus # common in both collections with positions in second     
       com2 <- subset(o2, com2==TRUE)
        p2 <- com2$Freq/so2
      
       sim <- sum(pmin(p1,p2))  # = percent similarity
       jac <- common/(nrow(o1)+nrow(o2)-common)

     rt <- data.frame(lat1, lon1, lat2, lon2, sim, jac)
     res <- rbind(res,rt)
     }
    }
   


  # Calculate great circle distance between all grids above threshold
        lat1 <- res$lat1*pi/180
        lon1 <- res$lon1*pi/180
        lat2 <- res$lat2*pi/180
        lon2 <- res$lon2*pi/180
   # Total distance
    cosD <- (sin(lat1) * sin(lat2)) + (cos(lat1) * cos(lat2) * cos(lon2-lon1))
    D <- acos(cosD)*180/pi*(40000/360) # should be 40024 but this for simplicity
    D <- replace(D, is.na(D), 20000) # This for rounding error in R

   # Combine results
    simfin <- cbind(res, D)

   # Similarity as a function of distance (2000 km distance bins)
     nsim<-numeric(10); msim<-numeric(10); sdsim<-numeric(10); mjac<-numeric(10); sdjac<-numeric(10)
     int <- seq(1000, 19000, 2000)
     for (i in 1:10) {
        nsim[i] <- length(simfin$sim[simfin$D>(i-1)*2000 & simfin$D<i*2000])
        if (nsim[i]>0) {   
        msim[i] <- mean(simfin$sim[simfin$D>(i-1)*2000 & simfin$D<=i*2000]) 
       sdsim[i] <- sd(simfin$sim[simfin$D>(i-1)*2000 & simfin$D<=i*2000]) 
        mjac[i] <- mean(simfin$jac[simfin$D>(i-1)*2000 & simfin$D<=i*2000]) 
       sdjac[i] <- sd(simfin$jac[simfin$D>(i-1)*2000 & simfin$D<=i*2000]) 
            }
           }

 # Combine results
  ex1 <- data.frame(nsim, msim, sdsim, mjac, sdjac)
  ex2 <- cbind(int,ex1)

} # end of if condition

 colnames(ex2) <- c("Distance", "N", "p.sim", "sd.sim", "jac.sim", "sd.jac")

 # write.table(ex2, file="D:/arbeit/papers/Beta/R_output/dist_sim_all.csv", row.names=F, sep=";") # all Taxa
 

###############################

attach(ex2)

errsim <- 1.96*sd.sim/sqrt(N)

 # Now plot
    errbar(Distance[N>1],p.sim[N>1], p.sim[N>1]+errsim[N>1],
            p.sim[N>1]-errsim[N>1], cap=0, col=1, 
            xlab="Distance", ylab="Similarity", main="Jurassic 5 - Bivalves")

detach(ex2)
