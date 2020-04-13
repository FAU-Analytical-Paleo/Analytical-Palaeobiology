# Analyse Mauna Loa CO2 data

# get CO2 data from Mauna Loa observatory
ww1 <- "ftp://aftp.cmdl.noaa.gov/products/"
ww2 <- "trends/co2/co2_mm_mlo.txt"
CO2 <- read.table(paste(ww1,ww2,sep=""))[,c(1,2,5)]
# assign better column names
colnames(CO2) <- c("year","month","ppm")


# create a time series (ts) object from the CO2 data
co2 <- ts(data=CO2$ppm, frequency=12,
          start=c(CO2[1,"year"],CO2[1,"month"]))

# plot the ts
plot.ts(co2, ylab=expression(paste("CO"[2]," (ppm)")))

# Manual work

# weights for moving avg
fltr <- c(1/2,rep(1,times=11),1/2)/12

# estimate of trend
co2.trend <- filter(co2, filter=fltr, method="convo", sides=2)
# plot the trend
plot.ts(co2.trend, ylab="Trend", cex=1)


# seasonal effect over time
co2.1T <- co2 - co2.trend

# plot the monthly seasonal effects
plot.ts(co2.1T, ylab="Seasonal effect", xlab="Month", cex=1)


# length of ts
ll <- length(co2.1T)
# frequency (ie, 12)
ff <- frequency(co2.1T)
# number of periods (years); %/% is integer division
periods <- ll %/% ff
# index of cumulative month
index <- seq(1,ll,by=ff) - 1
# get mean by month
mm <- numeric(ff)


for(i in 1:ff) {
  mm[i] <- mean(co2.1T[index+i], na.rm=TRUE)
}
# subtract mean to make overall mean=0
mm <- mm - mean(mm)

# plot the monthly seasonal effects
plot.ts(mm, ylab="Seasonal effect", xlab="Month", cex=1)


#######################
 # Use package forecast
 require(forecast)
 co2.trend2 <- ma(co2, order=12, centre=T)

  # Detect the trend
plot(as.ts(co2))
lines(co2.trend2)
plot(as.ts(co2.trend2))

 # Detrend
  dt <- co2-co2.trend2
 plot(dt)

  # Get the seasonal component
  m.co2 <- t(matrix(data = dt[1:720], nrow=12))
  co2.seas <- colMeans(m.co2, na.rm=T)
  plot(co2.seas, type="l")
  
 # Get the "random noise"
  rand <- co2[1:720] - co2.trend2[1:720] - co2.seas
   plot(as.ts(rand))  
  
  
 # Use function to everything automatically
   decom.co2 <- decompose(co2, "additive")
  
   plot(decom.co2$trend) # etc.
   
#####################

    spectrum(co2)
 