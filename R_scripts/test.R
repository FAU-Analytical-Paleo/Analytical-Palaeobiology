### Münzwurfschleife
  gewinn <- 0

  for (i in 1:100) {
    ergeb <- sample(c(-1,1), 1)
     gewinn <- gewinn + ergeb
  }


  gewinn <- numeric()
   gewinn[1] <- 0
  
  for (i in 1:1000) {
    ergeb <- sample(c(-1,1), 1)
    gewinn[i+1] <- gewinn[i] + ergeb
  }
  
  
  plot(gewinn)
  

####

setwd(choose.dir())

  dat <- read.csv("O18bins.csv", sep=";", header=T)

  plot(dat$slage, dat$Om, type="l")

  dt.Om <- resid(lm(dat$Om ~ seq(along = dat$slage))) 
  dt.Om <- resid(lm(dat$Om ~ dat$slage)) 

  plot(dat$slage, dt.Om, type="l", xlim=c(540,0))
