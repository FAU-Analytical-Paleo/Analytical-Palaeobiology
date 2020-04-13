# Import einer Datei und Auswertung

remove(list=ls())

# PBDB raw data
myData <- read.table(file="C:/Daten/nceas/PBDB/Scleractinia.csv", header=TRUE, sep=",", as.is=TRUE)
attach(myData)

# Distinct genera in dataset
genus <- occurrences.genus_name
f <- factor(genus)
levels(f) 
length(levels(f))


detach(myData)


# PBDB ouput raw data
myData <- read.table(file="C:/Daten/nceas/PBDB/Scleractinia_rawcurve.csv", header=TRUE, sep=",", as.is=TRUE)
# edit(myData)

attach(myData)


age1 <- Base..Ma.
age2 <- Midpoint..Ma.
sg <- Sampled.genera
bg <- Boundary.crosser.genera
rg <- Range.through.genera
e <- Extinction.rate
o <- Origination.rate
co <- Collections
oc <- Occurrences




hist(e, col="red", xlab="Extinction rate", main="Histogram of extinction rates")

# Number of collections
plot(age2, co, xlab="Age (Ma)", ylab="Number of collections", pch=16, type="b", xlim=c(250,0), ylim<-c(-5, 500), frame=F,  axes=F)
axis(1, pos=-25, at=seq(0, 250, by=50), labels=c("0", "50", "100", "150", "200", "250")) # for collections
axis(2, pos=251)
# Definition of legend boxes
bt <- -25 # Bottom of rectangle
to <- 5 # Top of rectangle
bt2 <- -5 # Level of text

# Number of occurrences
plot(age2, oc, xlab="Age (Ma)", ylab="Number of occurrences", log="y", pch=16, type="b", xlim=c(250,0), ylim<-c(50, 5000), frame=F,  axes=F)
axis(1, pos=42, at=seq(0, 250, by=50), labels=c("0", "50", "100", "150", "200", "250")) # for occurrences
axis(2, pos=251)
# Definition of legend boxes
bt <- 42 # Bottom of rectangle
to <- 60 # Top of rectangle
bt2 <- 50 # Level of text


# Boundary crosser and SIB diversity
lt1=2
lt2=1
plot(age1, bg, type="b", col="blue", xlim=c(250,0), ylim<-c(-5, 160), xlab="Age (Ma)", pch=21, lty=lt1, ylab="Number of genera", frame=F)
points(age2, sg, pch=16, type="b", lty=lt2)

abline(v=199.6, col="red") # Mark Triassic-Jurassic boundary
abline(v=65.5, col="red") # Mark KT boundary

legend(150, 35, bty="n", legend = c("Boundary crossers", "SIB"), lty = lt1:lt2, 
         col = c("blue", "black"), pch=c(21, 16), title ="Counting methods")

# Definition of legend boxes
bt <- -25 # Bottom of rectangle
to <- 0 # Top of rectangle
bt2 <- -6 # Level of text




# PBDB ouput subsampled data
# Two timers
myData2 <- read.table(file="C:/Daten/nceas/PBDB/Scler_subs_tt.csv", header=TRUE, sep=",", as.is=TRUE)

attach(myData2)

age1 <- Base..Ma.
age2 <- Midpoint..Ma.
tt <- Mean.two.timers.diversity
sg <- Raw.SIB.diversity
cg <- Corrected.SIB.diversity
e <- Extinction.rate
o <- Origination.rate
pairs(myData2)

# Boundary crossers
myData3 <- read.table(file="C:/Daten/nceas/PBDB/Scler_subs_bc.csv", header=TRUE, sep=",", as.is=TRUE)
attach(myData3)

bg <- Mean.boundary.crossers.diversity
age2 <- Midpoint..Ma.
e <- Extinction.rate
o <- Origination.rate
t= e+o
d = o-e

# Turnover rates
lt1=2
lt2=1
plot(age2[t>0], e[t>0],xlim=c(250,0), ylim<-c(0, 0.55), col="red", xlab="Age (Ma)", type="b", pch=21, ylab="Per genus rate")
points(age2[t>0], o[t>0], pch=16, col="green", type="b", lty=lt2)

legend(65, 0.12, bty="n", legend = c("Extinction rate", "Origination rate"), lty = lt2, 
         col = c("red", "green"), pch=c(21, 16))

abline(v=199.6, col="red") # Mark Triassic-Jurassic boundary
abline(v=65.5, col="red") # Mark KT boundary

# Definition of legend boxes
bt <- -0.25 # Bottom of rectangle
to <- 0.02 # Top of rectangle
bt2 <- 0 # Level of text




# Boundary crosser and SIB diversity
windows(height=5, width=7)
lt1=2
lt2=1
plot(age1, bg, type="b", col="blue", xlim=c(250,0), ylim<-c(-2, 60), xlab="Age (Ma)", pch=21, lty=lt1, ylab="Number of genera", frame=F)
points(age2, sg, pch=16, type="b", lty=lt2)

abline(v=199.6, col="red") # Mark Triassic-Jurassic boundary
abline(v=65.5, col="red") # Mark KT boundary

legend(150, 15, bty="n", legend = c("Boundary crossers", "SIB"), lty = lt1:lt2, 
         col = c("blue", "black"), pch=c(21, 16), title ="Counting methods")

# Definition of legend boxes
bt <- -10 # Bottom of rectangle
to <- 0 # Top of rectangle
bt2 <- -2 # Level of text




# New plot
windows(height=5, width=7)
plot(age2, sg, xlim=c(250,0), ylim<-c(-2, 60), xlab="Age (Ma)", ylab="Number of genera", type="n")
abline(v=199.6, col="red") # Mark Triassic-Jurassic boundary
abline(v=65.5, col="red") # Mark KT boundary

# Definition of equilibrium states


s1 <- mean(sg[age2<250&age2>160])
s2 <- mean(sg[age2<160&age2>66])
s3 <- mean(sg[age1<66])

ser1 <- sd(sg[age2<250&age2>160])/sqrt(length(sg[age2<250&age2>160]))
ser2 <- sd(sg[age2<160&age2>66])/sqrt(length(sg[age2<160&age2>66]))
ser3 <- sd(sg[age1<66])/sqrt(length(sg[age1<66]))

rect(xleft=240, ybottom=s1-ser1, xright=160, ytop=s1+ser1, col="gray")
rect(xleft=160, ybottom=s2-ser2, xright=66, ytop=s2+ser2, col="gray")
rect(xleft=66, ybottom=s3-ser3, xright=0, ytop=s3+ser3, col="gray")

segments(240, s1, 160, s1)
segments(160, s2, 65.5, s2)
segments(65.5, s3, 0, s3)


points(age2, sg, pch=16, type="b", lty=lt2)


# Definition of legend boxes
bt <- -10 # Bottom of rectangle
to <- 0 # Top of rectangle
bt2 <- -2 # Level of text


# Fetch combined bivalve-coral data for comparison
bdat <- read.table(file="C:/Daten/nceas/PBDB/Combined_curve.csv", header=TRUE, sep=";", as.is=TRUE)
attach(bdat)
lt1=2
lt2=1
plot(age2, Occ_biv, type="b", col="blue", xlim=c(250,0), ylim=c(10, 20000), log="y", xlab="Age (Ma)", pch=21, lty=lt1, ylab="Number of occurrences", frame=F)
points(age2, Occ_cora, pch=16, type="b", log="y", lty=lt2)

abline(v=199.6, col="red") # Mark Triassic-Jurassic boundary
abline(v=65.5, col="red") # Mark KT boundary

legend(150, 35, bty="n", legend = c("Bivalves", "Corals"), lty = lt1:lt2, 
         col = c("blue", "black"), pch=c(21, 16))

# Definition of legend boxes
bt <- 5 # Bottom of rectangle
to <- 12 # Top of rectangle
bt2 <- 9 # Level of text

# Z-value plot
plot(age2, cor_biv_z, type="b", col="blue", xlim=c(250,0), xlab="Age (Ma)", ylab="Standardized difference corals-bivalves" )

abline(v=199.6, col="red") # Mark Triassic-Jurassic boundary
abline(v=65.5, col="red") # Mark KT boundary
abline(h=0, lty=2)
abline(h=-1.96, lty=2)

# Definition of legend boxes
bt <- -3.32 # Bottom of rectangle
to <- -3 # Top of rectangle
bt2 <- -3.17 # Level of text


# Z-value plot
plot(age2, cor_reef_z, type="b", col="blue", xlim=c(250,0), xlab="Age (Ma)", 
     ylab="Standardized difference corals-reefs" )

abline(v=199.6, col="red") # Mark Triassic-Jurassic boundary
abline(v=65.5, col="red") # Mark KT boundary
abline(h=0, lty=2)
abline(h=-1.96, lty=2)

# Definition of legend boxes
bt <- -1.22 # Bottom of rectangle
to <- -1.05 # Top of rectangle
bt2 <- -1.12 # Level of text

# Draw boxes
rect(xleft=251, ybottom=bt, xright=199.6, ytop=to) # Triassic
rect(xleft=145.5, ybottom=bt, xright=199.6, ytop=to) # Jurassic
rect(xleft=145.5, ybottom=bt, xright=65.5, ytop=to) # Cretaceous
rect(xleft=23, ybottom=bt, xright=65.5, ytop=to) # Paleogene
rect(xleft=23, ybottom=bt, xright=0, ytop=to) # Neogene

# Add Text for legend of time
text(x=226, y=bt2, labels="Tr")
text(x=175, y=bt2, labels="J")
text(x=105, y=bt2, labels="K")
text(x=45, y=bt2, labels="Pg")
text(x=10, y=bt2, labels="N")