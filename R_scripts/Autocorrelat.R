# Übung zu Autokorrelationen

myData <- read.table(file="c:/daten/r/Lehre/CO2.csv", header=TRUE, sep=";")
 # Omit last point (no origination rate)
  myData <- subset(myData, AgeMid>1)

 vl <- nrow(myData)-1

attach(myData)

# Plot relevant data
plot(AgeMid, RCO2, type="l", col="red")
 points(AgeMid, Origin_Rate*10, type="l", col="green")

# Raw correlations
  cor.test(RCO2, Origin_Rate, method="spearman")

# CCF with raw data
 ccf(RCO2, Origin_Rate)

# Test for autocorrelations
 acf(RCO2)
 acf(Origin_Rate)


# Remove autocorrelations by differencing (lag 1)
 d.CO2 <- diff(RCO2)
 d.ori <- diff(Origin_Rate)

# Test again 
  cor.test(d.CO2, d.ori, method="spearman")

# Any lags? Cross-correlation function
 ccf(d.CO2, d.ori)


# Generalized differencing
 a.CO2 <- acf(RCO2)
 a.ori <- acf(Origin_Rate)

  g.ori <- numeric()
  g.CO2 <- numeric()
   for (i in 1:vl) {
      g.ori[i] <- Origin_Rate[i+1]-a.ori$acf[2]*Origin_Rate[i]
      g.CO2[i] <- RCO2[i+1]-a.CO2$acf[2]*RCO2[i]
      }

 cor.test(g.CO2, g.ori, method="spearman")

   plot(g.CO2, g.ori)





 
  