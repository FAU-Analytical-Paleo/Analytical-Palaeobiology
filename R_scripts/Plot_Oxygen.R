# Plot Veizers Oxygen-Isotope data



# This for Raw data
dat <- read.csv("O18bins.csv", sep=";", header=T)

 age <- dat$slage
 dt.Om <- resid(lm(dat$Om ~ dat$slage))


# Plot diversity curve in Black and White
windows(height=6, width=8) # Define window size ## Not really needed because window can be resized any time
 par(xaxs="i", yaxs="i")
plot(age, dt.Om, xlab="Age (Ma)", ylab= expression(paste(delta, {}^18, "O")), pch=16, type="b", 
       xlim=c(550,0), ylim=c(-4,3), frame=F,  axes=F)
axis(1, pos=-30, at=seq(0, 550, by=50), labels=c("0", " ", "100", " ", "200", " ", "300", " ", "400", " ", "500", " "))
axis(2, pos=550)

abline(v=251) # Mark Permian-Triassic boundary
abline(v=65.5) # Mark KT boundary


# Add boxes  for legend of time
bt <- -4 # Bottom of rectangle
to <- -3.7 # Top of rectangle

# Black and white version

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
