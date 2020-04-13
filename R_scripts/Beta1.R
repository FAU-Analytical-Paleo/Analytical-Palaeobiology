# Beta-diversity of Jurassic radiolarians
  # Data from Dissertation of Kiessling (1995)

  setwd("c:/daten/R/Lehre")
 
  rad <- read.table(file="Beta_div.csv", sep=";", header=T)

  pK20 <- rad$K_20_1H/sum(rad$K_20_1H)
  pK25 <- rad$K_25H/sum(rad$K_25H)
  
  p20_25 <- pmin(pK20, pK25)
  
   sum(p20_25)
  
  
 # The hard way

  # Jaccard
   test <- cbind(rad$K_20_1H, rad$K_25H)   
    a <- length(which(test[,1]>0 & test[,2]>0))
    b <- length(which(test[,1]>0 & test[,2]==0))
    c <- length(which(test[,1]==0 & test[,2]>0))

    jac <- a/(a+b+c)


  # Percent similarity
     # Transform to porportions
    for (i in 1:ncol(test))
      test[,i] <- test[,i]/sum(test[,i])
    
     psim <- sum(pmin(test[,1], test[,2]))

###

 # Distances instead of similarities
    d.jac <- 1-jac
    d.psim <- 1-psim


###########
 sp <- rad$Species
  rad <- subset(rad, select=c(3:16))

  row.names(rad) <- sp


 # distance matrix
  # Jaccard
    j.dis <- dist(t(rad), method="binary") # compare with d.jac


  # Percent similarity matrix
    # Proportions
    for (i in 1:ncol(rad))
      rad[,i] <- rad[,i]/sum(rad[,i])
  
     sim <- matrix(nrow=ncol(rad), ncol=ncol(rad), 
           dimnames = list(names(rad), names(rad)))

    for (j in 1:(ncol(rad)-1)) {
       for (i in j:(ncol(rad)-1)) {
         sim[i+1,j] <- sum(pmin(rad[,j], rad[,i+1]))
                   }
                 }

      p.dis <- 1-sim
     p.dis <- as.dist(p.dis)

     
  # Compare with bray-curtis distance in vegan package
     require(vegan)
       vegdist(t(rad)) # Bray is the default metric
     
          
####
 # Don't cluster shamelessly
    # Jaccard
       hc.co <- hclust(j.dis, method="ward.D2")
    plot(hc.co, hang=-0.1)
   # Psim
       hc.co <- hclust(p.dis, method="ward.D2")
     plot(hc.co, hang=-0.1)

 # Parametric
   MDS <- cmdscale(p.dis)
  z <- row.names(MDS)
 plot(MDS)
   text(MDS[,1], MDS[,2], labels=z)

  # Non-parametric with Meta-MDS
   MDS2 <- metaMDS(p.dis)
    plot(MDS2, type="text")   
    