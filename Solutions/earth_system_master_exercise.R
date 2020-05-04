# Master exercise: Earth System Parameters
# Explore data of Cardenas and Harries (2010)

dat <- read.csv("Daten/div_earthsystem.csv", sep=";", header=T)
# Look at data
View(dat)

# Plot data
windows()
plot(dat)

acf(dat$ori)
acf(dat$O.isotop)
acf(dat$C.isotop)
acf(dat$Sr.isotop)

# First differences
d.dat <- as.data.frame(apply(dat, 2, diff))    
round(cor(d.dat), 2)

# Plots as is Cardenas and Harries (2010)
plot(d.dat) # check row 2

cor.test(d.dat$div, d.dat$Sr.isotop)    
cor.test(d.dat$ori, d.dat$Sr.isotop)
cor.test(d.dat$ori, d.dat$S.isotop)  

# Turn correlation into regression
mod <- lm(ori ~ Sr.isotop + S.isotop + O.isotop + C.isotop + sea.level, data=d.dat)
summary(mod)

fin <- step(mod)
summary(fin) 

acf(residuals(fin))


# Generalized differences

# Generalized differences as function
gdiff <- function(x) {
  ac <- acf(x)$acf[2]
  g.x <- numeric()
  for (i in 1:length(x)-1) g.x[i] <- x[i+1] - ac*x[i]
  return(g.x)
} 

g.dat <- as.data.frame(apply(dat, 2, gdiff)) 

round(cor(g.dat), 2)

cor.test(g.dat$div, g.dat$Sr.isotop)    
cor.test(g.dat$ori, g.dat$Sr.isotop)
cor.test(g.dat$ori, g.dat$S.isotop)  

# Turn correlation into regression
mod <- lm(ori ~ Sr.isotop + S.isotop + O.isotop + C.isotop + sea.level, data=g.dat)
summary(mod)

fin <- step(mod)
summary(fin) 

acf(residuals(fin))

# Try generalized least squares
library(nlme)
mod1 <- gls(ori ~ Sr.isotop, correlation=corARMA(p=1), data=dat)

acf(residuals(mod1))


# Additional ####

# How to write a function

myfirstfunction <- function(name){
  temp <- paste("My name is", name)
  
  return(temp)
}

myfirstfunction("Nuss")    

num <- c(9, 7, 5, 7, 4, 0, 9, 8, 6)    

meansdmax <- function(x){
  m <- mean(x)
  s <- sd(x)
  ma <- max(x)
  
  y <- c(m, s, ma)
  names(y) <- c("mean", "sd", "max")
  return(y)
}

meansdmax(num)  

