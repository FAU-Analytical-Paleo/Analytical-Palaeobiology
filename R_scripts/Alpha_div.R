# Diversity
# Script for course

setwd("c:/daten/R/Lehre")

# Alpha Diversity
# PBDB collection 31618
# Bangtoupo F30, Qingyan, China: Pelsonian - Illyrian, China
dat <- read.table(file="Diversity1.csv", header=TRUE, sep=";")



attach(dat)

windows()
barplot(Individuen, xlab="Species", ylab="Abundance", col="lightblue")
barplot(sort(Individuen, decreasing=T), xlab="Species", ylab="Abundance", col="lightblue")

# Immediate values
 S <- nrow(dat) # = number of species
 N <- sum(Individuen) # N
 cl <- length(levels(factor(Class))) # Number of classes
 gen <- length(levels(factor(Genus))) # Number of genera
 menh <- S/sqrt(N) # Menhinick Index
 marg <- (S-1)/log(N) # Margalef Index

# Prepare data for other indices
 psp <- Individuen/N  # Proportions of each individual in assemblage
 sha <- psp*log(psp)  # For Shannon-Index
 p2 <- psp^2          # For Hurlbert's PIE and Simpson's index

 # Calculate indices
      H <- -sum(sha)             ## Shannon H
      J <- H/log(S)              ## Shannon J
      E <- exp(H)/S              ## Equitability
      pD <- max(psp)             ## Berger-Parker Dominanz
      D <- sum(p2)               ## Simpson's D
      PIE <- S/(S-1)*(1-sum(p2)) ## Hurlbert's PIE
 
  # Fisher's alpha  S = a ln(1+N/a) 
     alpha <- 1
     F <- 1
     while(F <= S) {
           F <- alpha * log(1+N/alpha)
           alpha <- alpha+0.01
            }

 # Get one value for 100 individuals
    gensp <- paste(Genus, Species)
     abu <- rep(gensp, Individuen)
     trial <- 1000 # subsampling trials
        rardiv <- numeric()
         erdiv <- numeric()
      div <- numeric()

     for (i in 1:trial) {
       z <- sample(abu, 100) # subsampling without replacement
       div[i] <- length(levels(factor(z)))    
           }  


 


  # Empirical rarefaction
     # Expected number of species if x individuals are collected
     gensp <- paste(Genus, Species)
     abu <- rep(gensp, Individuen)
     trial <- 100 # subsampling trials
        rardiv <- numeric()
         erdiv <- numeric()
       count <- 1 
        sq <- seq(1, 651, by=10)
     for (j in sq)  {

      div <- numeric(trial)

     for (i in 1:trial) {
       z <- sample(abu, j) # subsampling without replacement
       div[i] <- length(levels(factor(z)))    
           }  

      # Rarefied diversity
       rardiv[count] <- mean(div)
       erdiv[count] <- sd(div)
      count <- count+1
  }



op <- par(mfrow=c(2,1), lwd=2, mar=c(4,4,1,1))
 plot(sq, rardiv, type="l", xlab="", ylab="S")
 plot(sq, rardiv, log="xy", type="l", xlab="N", ylab="S")
  x <- seq(600); y <- x^0.8
  points(x,y, type="l", lty=2)
par(op)


# Summarize results
resname <- c("Species", "Individuals", "H", "J", "Simpson's D", "1-D", "PIE", "Berger-Parker", "Fisher's alpha",
   "Margalef", "Menhinick", "Rarefied.200", "SD_Rarefied.200")                       
resu <- c(S, N, H, J, D, 1-D, PIE, pD, alpha, marg, menh, rardiv[9], erdiv[9])
resround <- round(resu, 3)
x <- data.frame(cbind(resname, resround))
x

write.table(x, file="Result_div1.csv", sep=";", row.names=FALSE)


####
 ## Rank-abundance curve
 z <- sort(Individuen, decreasing=T)
 op <- par(cex=1.5, lwd=1.5, mar=c(5,4,2,1))

 # Linear plot
 plot(z, type="b", xlab="Rank", ylab="Abundance", col="red", lwd=2)

 # Log plot
 plot(z, type="b", log="y", xlab="Rank", ylab="Abundance", col="red", lwd=2)

 # Cummulative plot
 plot(cumsum(z), type="l", log="y", xlab="Rank", ylab="Abundance", col="red", lwd=2)
 # Cummulative proportional plot
 plot(cumsum(z/N), type="l", log="y", xlab="Rank", ylab="Abundance", col="red", lwd=2)

par(op)


# Create model distributions given data
 # Normal distribution
   Nm <- mean(Individuen)
   Nsd <- sd(Individuen)
    nsa <- rnorm(S, Nm, Nsd)
    hist(nsa)
     z2 <- sort(nsa, decreasing=T)
     plot(z2, type="l", log="y", xlab="Rank", ylab="Abundance", col="red", lwd=2)
 # Lognormal distribution
    lN <- log(Individuen)
    lNm <- mean(lN)
    lNsd <- sd(lN)
    lsa <- rlnorm(S, lNm, lNsd) 
    hist(lsa)
     z3 <- sort(lsa, decreasing=T)
     plot(z3, type="l", log="y", xlab="Rank", ylab="Abundance", col="red", lwd=2)
 # Geometric series
   # Determine rate
       zn <- z[1] # use dominant
       rt <- S/zn/min(Individuen) # overall slope
       z4 <- numeric()
       for (i in 1:S) z4 <- c(zn, z4/rt)
       # use regression model
       reg <- lm(log(z) ~ seq(1,length(z)))
       reg2 <- exp(predict(reg))

     plot(reg2, type="l", log="y", xlab="Rank", ylab="Abundance", col="red", lwd=2)

   

### Multiplot
plot(z, type="b", log="y", xlab="Rank", ylab="Abundance", col="red", lwd=2)
 points(z2, type="l", col="grey")
 points(z3, type="l", col="blue")
 points(reg2, type="l", col="green")

library(vegan)
 fit <- radfit(Individuen)
 plot(fit)
 
detach(dat)