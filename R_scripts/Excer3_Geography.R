# Geographic analyses
setwd("c:/daten/R/Lehre")

geo <- read.table(file="geo2.csv", header=TRUE, sep=";")

attach(geo)
 names(geo)

# detach(geo)
# x <- subset(geo, occurrences.class_name=="Anthozoa")
# attach(x)


 # Histograms
 op <- par(mfrow=c(3,1), mar=c(4.5,4,3,2))
  hist(d.max)
  hist(n.max)
  hist(range)
 par(op)
 # Logged
  op <- par(mfrow=c(3,1), mar=c(4.5,4,3,2))
  hist(log(d.max))
  hist(log(n.max))
  hist(log(range))
 par(op)


 # Correlations between longevity and other variables
   cor.test(d.max, range, method="spearman")
   cor.test(n.max, range, method="spearman")
   cor.test(n.tot, range, method="spearman")

 # Multiple regression
   summary(lm(range ~ d.max))
   summary(lm(range ~ n.max))
   fit <- (lm(range ~ d.max+n.max))
 
 # Are both needed?
 library(MASS)
  stepAIC(fit, direction="both")