# Master-Übung Dienstag
 # Influence of climate change on marine biodiversity patterns
setwd("c:/daten/R/Lehre")

 # Get diversity data
  dat1 <- read.table(file="bin_div.csv", header=TRUE, sep=";")
   # Omit first point (no diversity)
    dat1 <- subset(dat1, BIN>1)

 # Get oxygen isotope data by bin
  dat2 <- read.table(file="O18bins.csv", header=TRUE, sep=";")
   # Omit first point (no data)
    dat2 <- subset(dat2, BIN>1)

 # Extract the relevant data
  age <- dat1$AGE_MID
  div <- dat1$Div
  temp <- dat2$Tm

 # Look at raw data
  plot(age, div, xlim=c(530, 0), type="l")
   abline(lm(div~age))
  plot(age, temp, xlim=c(530, 0), type="l")
   abline(lm(temp~age))

   # log the data
     div <- log(div)
     temp <- log(temp)
