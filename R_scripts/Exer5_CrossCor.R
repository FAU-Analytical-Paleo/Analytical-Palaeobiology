

# Load Results from PBDB after rarefaction

  pbdb <- read.table(file="ss_curve_TT2500_O1,3W10x.csv", header=TRUE, sep=",")

   pbdb <- pbdb[order(pbdb$Bin),]

   ext <- pbdb$Extinction.rate[pbdb$Bin>2 & pbdb$Bin<49]
   ori <- pbdb$Origination.rate[pbdb$Bin>2 & pbdb$Bin<49]
   age <- pbdb$Midpoint..Ma.[pbdb$Bin>2 & pbdb$Bin<49]


# Plot time series
 op <- par(xaxs="i", yaxs="i")

  th <- 1.5 #Line thickness in plot
 plot(age, ext, xlab="Age (Ma)", ylab="Rate", 
      pch=16, type="b", col="red", lwd=th, xlim=c(550,0), ylim=range(-0.1,2))
    points(age, ori, type="b", col="green")
     legend("topright", lty=1, lwd=th, col=c("red", "green"), legend=c("Extinction", "Origination"))
 par(op)


  acf(ext, ci.type = "ma")
  acf(ori, ci.type = "ma")
 
 # First differences
  d.ext <- diff(ext)
  d.ori <- diff(ori)

 # Cross-correlation
  ccf(d.ori, d.ext)

  x <- ccf(d.ori, d.ext)

 plot(d.ext, d.ori, col="red", xlab=expression(paste(Delta, "  Extinction rate")),
       ylab=expression(paste(Delta, "  Origination rate")), pch=19, cex=1.4)
  abline(lm(d.ori~d.ext), lwd=2) 

  cor.test(d.ext, d.ori)

 d.ex <- c(NA, d.ext)
 d.or <- c(d.ori, NA)

 cbind(d.ex, d.or)

 plot(d.ex, d.or, col="red", xlab=expression(paste(Delta, "  Extinction rate")),
       ylab=expression(paste(Delta, "  Origination rate at lag 1")), pch=19, cex=1.4)
  abline(lm(d.or~d.ex), lwd=2) 

  cor.test(d.ex, d.or)

##########
 # Now implement generalized differencing before testing for cross-correlation



##################################

# Temporal trend
 # Show extinction trend
   ext.lm <- lm(ext~age)
  plot(age, ext, xlab="Age (Ma)", ylab="Rate", 
      pch=16, type="b", col="red", lwd=th, xlim=c(550,0), ylim=range(-0.1,2))
    abline(ext.lm, col="red") 
  # confidence intervals
    prd <- predict(ext.lm,newdata=data.frame(x=age),interval = c("confidence"), 
                   level = 0.95, type="response") # confidence for regression line
    prd2 <- predict(ext.lm,newdata=data.frame(x=age), interval = c("prediction"), 
                level = 0.95, type="response") # confidence for predicted values
  lines(age, prd[,2],col="red",lty=2)
  lines(age,  prd[,3],col="red",lty=2)
   lines(age, prd2[,2],col="orange",lty=3)
   lines(age,  prd2[,3],col="orange",lty=3)
 
 # Test significance
  cor.test(age, ext)
  cor.test(age, ori)

  cor.test(age, ext, method="spearman")
  cor.test(age, ori, method="spearman")

 
 # Test for exponential decline
 # fit non-linear model
   ext.exp <- nls(ext ~ exp(a + b * age),  start = list(a = 0, b = 0))
 
      lines(age, predict(ext.exp, list(x = age)))
 
   ext.exp <- lm(ext ~ age + I(age^2))
 
 # Which fit is better?
   summary(ext.lm)
   summary(ext.exp)
 
    AIC(ext.lm)
    AIC(ext.exp)
 

 
 ###################################
 
 
# Remove long-term trend and plot
   bck <- lm(ext~age)

  dt.ext <- ext - (bck$coefficients[2]*age + bck$coefficients[1]) 
  dt.ext2 <- resid(lm(ext ~ seq(along = age))) #another method to do the same

 # Plot results
 plot(age, dt.ext, xlab="Age (Ma)", ylab="Rate", 
      pch=16, type="b", col="red", lwd=th, xlim=c(550,0))

 # Make all values positive
 plot(age, dt.ext+abs(min(dt.ext)), xlab="Age (Ma)", ylab="Rate", 
      pch=16, type="b", col="red", lwd=th, xlim=c(550,0))

 