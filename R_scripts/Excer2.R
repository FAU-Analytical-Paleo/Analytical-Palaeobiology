setwd(choose.dir())
setwd("C:/Daten/R/Lehre")

# Compare counts with z-values

data <- read.table(file="Excer2.csv", header=TRUE, sep=";")

 edit(data)

  q1 <- data$Quadrat.1
  q2 <- data$Quadrat.2

 # z.q1 <- (q1- mean(q1))/sd(q1)
 # z.q2 <- (q2- mean(q2))/sd(q2)

   z.q1 <- scale(q1)
   z.q2 <- scale(q2)
  d.z <- abs(z.q1-z.q2)

 res <- cbind(data, z.q1, z.q2, d.z)

  res <- res[order(-res$d.z),] # Datmatrix ordnen

  # Beurteilung ? #
