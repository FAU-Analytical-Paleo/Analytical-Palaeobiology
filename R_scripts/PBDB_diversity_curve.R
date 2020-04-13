# PLOT PBDB DIVERSITY CURVES

 setwd("D:/arbeit/presentations/Vorlesungen/Analytische Pal/Daten")

# This for Raw data
myData <- read.table(file="subsampled_curve_data.csv", header=TRUE, sep=",")

attach(myData)
age1 <- Base..Ma.
age2 <- Midpoint..Ma.
sg <- Mean.boundary.crossers.diversity[age1<520] # do not use the first two bins
ext <- Extinction.rate[age2<515&age2>12]
ori <- Origination.rate[age2<515&age2>12]

detach(myData)

age <- age1[age1<520]
mage <- age2[age1<520]

# Simple plot
 plot(age, sg, type="l")

# Scatterplot Extinction-Origination
plot(ext, ori)



# Plot diversity curve in Black and White
#windows(height=6, width=8) # Define window size ## Not really needed because window can be resized any time
 par(xaxs="i", yaxs="i")
plot(mage, sg, xlab="Age (Ma)", ylab="Sampled number of genera at 2500 occurrences", pch=16, type="b", 
       xlim=c(550,0), ylim=range(-30,770), frame=F,  axes=F)
axis(1, pos=-30, at=seq(0, 550, by=50), labels=c("0", " ", "100", " ", "200", " ", "300", " ", "400", " ", "500", " "))
axis(2, pos=550)

abline(v=251) # Mark Permian-Triassic boundary
abline(v=65.5) # Mark KT boundary

abline(lm(sg~age), col="red") # Add linear regression line

# Add boxes  for legend of time
bt <- -30 # Bottom of rectangle
to <- 0 # Top of rectangle
rect(xleft=542, ybottom=bt, xright=488.3, ytop=to) # Cambrian
rect(xleft=488.3, ybottom=bt, xright=443.7, ytop=to) # Ordovician
rect(xleft=443.7, ybottom=bt, xright=416, ytop=to) # Silurian
rect(xleft=416, ybottom=bt, xright=359.2, ytop=to) # Devonian
rect(xleft=359.2, ybottom=bt, xright=299, ytop=to) # Carboniferous
rect(xleft=251, ybottom=bt, xright=299, ytop=to) # Permian
rect(xleft=251, ybottom=bt, xright=199.6, ytop=to) # Triassic
rect(xleft=145.5, ybottom=bt, xright=199.6, ytop=to) # Jurassic
rect(xleft=145.5, ybottom=bt, xright=65.5, ytop=to) # Cretaceous
rect(xleft=23, ybottom=bt, xright=65.5, ytop=to) # Paleogene
rect(xleft=23, ybottom=bt, xright=0, ytop=to) # Neogene

# Add Text for legend of time
bt <- (bt+to)/2 # Level of text
text(x=515, y=bt, labels="Cm")
text(x=468, y=bt, labels="O")
text(x=430, y=bt, labels="S")
text(x=390, y=bt, labels="D")
text(x=330, y=bt, labels="C")
text(x=275, y=bt, labels="P")
text(x=226, y=bt, labels="Tr")
text(x=175, y=bt, labels="J")
text(x=105, y=bt, labels="K")
text(x=45, y=bt, labels="Pg")
text(x=10, y=bt, labels="N")








# Plot in color using USGS color codes
windows(height=6, width=8) # Define window size
 par(xaxs="i", yaxs="i")

th <- 1.5 #Line thickness in plot
plot(mage, sg, xlab="Age (Ma)", ylab="Sampled number of genera at 2500 occurrences", 
      pch=16, type="b", col="red", lwd=th, xlim=c(550,0), ylim=range(-30,750))

# , ylim=range(-10,700)

abline(v=251, lwd=th) # Mark Permian-Triassic boundary
abline(v=65.5, lwd=th) # Mark KT boundary

# Add boxes  for legend of time
bt <- -30 # Bottom of rectangle
to <- 0 # Top of rectangle
rect(xleft=542, ybottom=bt, xright=488.3, ytop=to, col="#fb805f") # Cambrian
rect(xleft=488.3, ybottom=bt, xright=443.7, ytop=to, col="#f981a6") # Ordovician
rect(xleft=443.7, ybottom=bt, xright=416, ytop=to, col="#b172b6") # Silurian
rect(xleft=416, ybottom=bt, xright=359.2, ytop=to, col="#9999c9") # Devonian
rect(xleft=359.2, ybottom=bt, xright=299, ytop=to, col="#99bdda") # Carboniferous
rect(xleft=251, ybottom=bt, xright=299, ytop=to, col="#67c6dd") # Permian
rect(xleft=251, ybottom=bt, xright=199.6, ytop=to, col="#67c3b7") # Triassic
rect(xleft=145.5, ybottom=bt, xright=199.6, ytop=to, col="#4db47e") # Jurassic
rect(xleft=145.5, ybottom=bt, xright=65.5, ytop=to, col="#7fc31c") # Cretaceous
rect(xleft=23, ybottom=bt, xright=65.5, ytop=to, col="#ffb300") # Paleogene
rect(xleft=23, ybottom=bt, xright=0, ytop=to, col="#fdcc8a") # Neogene

# Add Text for legend of time
bt <- (bt+to)/2 # Level of text
text(x=515, y=bt, labels="Cm")
text(x=468, y=bt, labels="O")
text(x=430, y=bt, labels="S")
text(x=390, y=bt, labels="D")
text(x=330, y=bt, labels="C")
text(x=275, y=bt, labels="P")
text(x=226, y=bt, labels="Tr")
text(x=175, y=bt, labels="J")
text(x=105, y=bt, labels="K")
text(x=45, y=bt, labels="Pg")
text(x=10, y=bt, labels="N")
