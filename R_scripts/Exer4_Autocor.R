##### Autocorrelations

###FIRST PART: examine autocorrelation

 data <- read.table(file="bin_div.csv", header=TRUE, sep=";")
 # Omit first point (no diversity)
  data <- subset(data, BIN>1)

 names(data)

attach(data)

# Plot relevant data
plot(AGE_MID, Div, type="l", col="red", xlim=c(520,0))
 
# Test for autocorrelation
 acf(Div, ci.type="ma")
 # Save as variable
 x <- acf(Div)

 plot(x, ci.type="ma")

 
###SECOND PART: DIFFERENCING
# Remove autocorrelations by differencing
 d.Div <- diff(Div)

# Test again
 acf(d.Div, ci.type="ma")

# Generalized differencing
  g.Div <- numeric()
   for (i in 1:length(Div)-1) g.Div[i] <- Div[i+1]-x$acf[2]*Div[i]
 acf(g.Div)
  