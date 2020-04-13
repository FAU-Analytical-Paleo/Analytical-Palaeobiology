# Coral analysis

dat <- read.csv(file="corals.csv", header=TRUE)


# Stage definitions
stages <- read.table(file="C:/Daten/PBDB/stages_2014PBDB.csv", header=TRUE, sep=";")
x <- stages$Age.m

table(dat$stg.num)

dat$gensp <- paste(dat$occurrence.genus_name, dat$occurrence.species_name)

 levels(factor(dat$gensp))


z <- subset(dat, select=c("occurrence.genus_name", "ECOLOGY"))
z <- unique(z)

 gens <- levels(factor(z$occurrence.genus_name))

 # preferential environment for each genus
   pref.env <- character()
  for (i in 1:length(gens))  {
    temp <- subset(dat, occurrence.genus_name %in% gens[i])
     t.temp <- table(temp$bathnow)[1:2]
      if(t.temp[1]!=t.temp[2]) pref.env[i] <- names(t.temp)[t.temp==max(t.temp)]
      if(t.temp[1]==t.temp[2]) pref.env[i] <- NA
  }
    
fin <- cbind(z, pref.env)

 tab <- table(fin$ECOLOGY, fin$pref.env)[3:4,]

   odds.az <- (tab[1,1]/tab[2,1])/(tab[1,2]/tab[2,2]) # compute odds ratio
   log(odds.az)
     er.odds <- sqrt(1/tab[1,1] + 1/tab[2,1] + 1/tab[1,2] + 1/tab[2,2])

    # relative preference of az for deep
       paste(log(odds.az), "±", er.odds)
