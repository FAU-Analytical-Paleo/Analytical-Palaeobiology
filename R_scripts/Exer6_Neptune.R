# Smoothing time series

# Load Neptune data
  nep <- read.table(file="Neptune.csv", header=TRUE, sep=";")

 attach(nep)

  plot(age, S, type="l", xlim=c(66,0))

 S3 <- smooth(S)

  plot(age, S3, type="l", xlim=c(66,0))


 # Remove a linear trend
  bck <- lm(S~age) # linear trend
 plot(age, S, type="l", xlim=c(66,0))
  abline(bck)
  # Either the complicated way ...
   S.bck <- S-(bck$coefficients[2]*age + bck$coefficients[1]) 
  # ... or the easy way
  S.bck <- resid(lm(S ~ seq(along = age))) 


  plot(age, S.bck, type="l", xlim=c(66,0))