# Analyse PBDB data

 setwd("c:/daten/PBDB")

  # Load linked occurence file
   pbd <- read.csv(file="Coral_occs_May14.csv", header=TRUE) 

   # Create a new clgen to avoid homonyms
    pbd$clgen <- paste(pbd$class_name, pbd$occurrence.genus_name)
 
   # Load 10 myr bins
     bins <- read.table(file="PBDB_bin.csv", header=TRUE, sep=";")
 
 # Merged file containing bin assignements now including non-assigned bins # changed
 comb<-merge(pbd, bins, by.x="X10_my_bin", by.y="BIN_NAME", all.x=TRUE, sort=FALSE) # 573900
 
  # Load stages
 stg <- read.table(file="C:/Daten/PBDB/stages_2013PBDB.csv", header=TRUE, sep=";")
 age <- stg$Age.m[1:94] 
 bin.n <- stg$Stage[1:94]
 age.b <- stg$Age.b
 durats <- -diff(stg$Age.b)
 
 
 # Assign stages by myr and stage names
 slc <- rep(NA, nrow(comb))
 for (i in 29:nrow(stg)-1) {
   st.n <- stg$stg.num[i]
   # st.b <- stg$BIN[i]
   b <- stg$Age.b[i]
   t <- stg$Age.b[i+1]
   slc <- replace(slc, which(comb$ma_max<=b & comb$ma_min>=t), st.n)
   slc <- replace(slc, which(as.character(stg$Stage[i])==as.character(comb$stage)), st.n)
 }
 
 
 comb <- cbind(comb, slc)
 
 
 x <- tabulate(comb$slc)
 

  # Create a raw diversity curve directly from occurrence data
   # Use all data
    pb.s <- pbd   

   start<-1
   end<-nrow(stg)-1
   vl<- nrow(stg)-1 # vector length

   

# Data vectors
   ex <- numeric(vl) #vector of extinction rates 
   or <- numeric(vl) #vector of origination rates
  # su <- numeric(vl) #vector of total survivors
   tt <- numeric(vl) #vector of two timers
   tht <- numeric(vl) #vector of three timers 
   ptm <- numeric(vl) #vector for part timers
   sg <- numeric(vl) #vector for singletons
   thr <- numeric(vl) #vector for through-ranging taxa 
   sib <- numeric(vl) #vector for SIB diversities
   ocs <- numeric(vl) # occurrences
   cols <- numeric(vl) # collections

  attach(pb.s)

  # Start loop
   for (i in start:end) {
      cols[i] <- length(unique(collection_no[slc==i]))
    
      rfl<-levels(factor(clgen[slc==i])) # All genera in bin
      rfupl<-levels(factor(clgen[slc>i])) # All genera in younger bins
      rfdol<-levels(factor(clgen[slc<i])) # All genera in older bins
      rfotl<- levels(factor(clgen[slc!=i])) # All genera outside bin
      rf1d<-levels(factor(clgen[slc==(i-1)])) # All genera in previous bin
      rf1u<- levels(factor(clgen[slc==(i+1)])) # All genera in subsequent bin

      s1 <- length(rfl [rfl %in% rfupl]) #Survivors from bin
      s2 <- length(rfl [rfl %in% rfdol]) #Survivors to bin
      ns <- length(rfl [rfl %in% rfotl]) #non-singletons
      tt[i] <- length(rfl [rfl %in% rf1d]) # Two-timers at bottom
      tht[i] <- length(rfl [rfl %in% rf1d & rfl %in% rf1u]) # Three-timers
      ptm[i] <- length(rf1d [rf1d %in% rf1u])-tht[i] # Part timers
      thr[i] <- length(rfdol [rfdol %in% rfupl]) # Through-rangers

   sg[i]<-length(rfl)-ns # Singleton taxa
   sib[i]<-length(rfl) # sampled in bin taxa
   ex[i]<- length(rfl)-s1 # extinct taxa
   or[i]<- length(rfl)-s2 # originating taxa
   ocs[i] <- nrow(subset(pb.s, slc==i))

   }

detach(pb.s)

fext<- -log(thr/(thr+ex-sg))        # Foote extinction rates based on total ranges
fori<- -log(thr/(thr+or-sg))        # Foote origination rates based on total ranges
ori <- or-sg
ext <- ex-sg
bc <- thr+ext                       # Boundary crossers
all <- thr+ext+ori                 # Total diversity without singletons
p.exb <- ext/bc                    # proportion of boundary crossers extinct
p.ex  <- ext/all                   # proportion of all genera extinct



ps <- tht/(tht+ptm) # Sampling probability = Three timer sampling stat 
    ps. <- c(ps[2:94],NA) # Sampling probability in subsequent bin
    tt. <- c(tt[2:94], NA) # Two timers in subsequent bin

   ttext <- log(tt/tht) # two-timer/three-timer ratio
     ttext. <- ttext + log(ps.) # corrected two-timer extinction rate

   ttori <- log(tt./tht)
    ttori.  <- ttori + log(ps) # corrrected two-timer origination rate   

 
 

# Plot 
  plot(age, ttext, type="l", xlim=c(550, 0), col="red")
    points(age, ttori, type="l", col="green")

  plot(age, ttext., type="l", xlim=c(550, 0), col="red")
    points(age, ttori., type="l", col="green")
  
  plot(age, fext, type="l", xlim=c(550, 0), col="red")
    points(age, fori, type="l", col="green")

 
  plot(age, ps, type="l", xlim=c(550, 0))

