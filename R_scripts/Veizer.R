# Read Veizer and Prokoph data (2015)

dat <- read.csv(file="C:/Daten/Extern/Veizer/ProkophVeizer2015.csv", header=TRUE)

 table(dat$climate)
 
 # exclude arctic
  dat <- subset(dat, climate!="arctic")
  
   # plot raw data
   plot(dat$gts2012, dat$d18O)
  
   # Detrend the data using equ 2 in Veizer & Prokoph (2015)
   SMOW <- -0.00003*dat$gts2012^2 + 0.0046*dat$gts2012
   cor.o.iso <- dat$d18O - SMOW
   plot(dat$gts2012, cor.o.iso)
   # Get temperature values
   dat$Te <- 16.9 - 4*(dat$d18O-SMOW-0.27)

   
########################
   # Compute mean values per time interval
   
 # bin to reef 10 myr intervals
    stages <- read.csv("C:/Daten/R/PARED/Reef_bins_new.csv", sep=";", header=T)
  age <- stages$age.mid
  
  
 # Assign bins  
  v.bin <- numeric()
  
  for (i in 1:(nrow(stages)-1)) {
    ix  <- which(dat$gts2012<= stages$Age_base[i] &  dat$gts2012>= stages$Age_base[i+1])
    v.bin[ix] <- i
  }
  
  dat.b <- cbind(dat, v.bin)
  
  # compute average temperature per bin
  m.O <- tapply(dat.b$Te, dat.b$v.bin, mean, na.rm=T)
  
  # captured bins
  b <- as.numeric(names(m.O))
  
  # Plot
  plot(age[min(b):max(b)], m.O, type="b", xlim=c(530,0))
  
  
  
#########################
   # Running means
   # Delete empty rows
   dat.b <- subset(dat.b, !is.na(d18O))

    # elegant solution but number of points maintained (unlike Veizer)
    n <- 50 # window size
   cx <- c(0,cumsum(dat.b$Te))
  rsum <- (cx[(n+1):length(cx)] - cx[1:(length(cx) - n)]) / n

  plot(rsum) 
  plot(dat.b$gts2012[1:length(rsum)], rsum, xlim=c(520,0),type="l")
  
  ####
  # Trying the Veizer approach with loop and current time scale
  steps <- 5
  winds <- 10
  
  # determine number of steps
  rang <- max(dat.b$gts2012)-min(dat.b$gts2012)
  n.st <- round(rang/steps)
   ages <- seq(0, rang, steps)

  
   m.T <- numeric() # container for smoothed temperature
   j <- 0 # the window start
  for (i in 1:n.st) {
    temp <- subset(dat.b, gts2012>=j & gts2012<=j+winds)
    m.T[i] <- mean(temp$Te)
    j <- j + steps
  }
  
   # linear interpolation for missing values
   require(zoo)
    T2 <- zoo(m.T)
     T2 <- na.approx(T2)
   
   plot(ages, T2, type="l", xlim=c(530,0)) # reverse to compare with Veizer et al. (2000)
   
  
  
########################
  # Loess smoothing
  # Local Polynomial Regression Fitting
  sm <- loess(dat.b$Te ~ dat.b$gts2012)
  
  plot(dat.b$gts2012, sm$fitted, xlim=c(520,0), type="l") # default span = 0.75 # first oder trends
  
  sm <- loess(Te ~ dat.b$gts2012, span=0.1)
  plot(dat.b$gts2012, sm$fitted, xlim=c(520,0), type="l") # many more details
  
  #####
  # What is the appropriate smoothing parameter?
  require(fANCOVA)
  lo <- loess.as(dat.b$gts2012, sm$fitted, degree = 1, criterion = "aicc",  user.span = NULL, plot = TRUE) 
  lo$pars$span
  