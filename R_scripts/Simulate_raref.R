# Simulate the effect of evenness on rarefaction

 S <- 100 # number of species

  A <-  round(rlnorm(S))+1
  B <-  rgeom(S, prob=0.25)+1



 # S species names
  spec <- c(LETTERS, letters, paste(LETTERS, letters), paste(letters, LETTERS))
   spec <- spec[1:S]


   plot(sort(B, decreasing=TRUE), log="y")
   points(sort(A, decreasing=TRUE), col="red")

 
 # Rarefaction
     abu.A <- rep(spec, A)
     abu.B <- rep(spec, B)

     trial <- 200 # subsampling trials

        rardiv.A <- numeric()
         div.A <- numeric()
        rardiv.B <- numeric()
          div.B <- numeric()

     for (i in 1:trial) {
       a <- sample(abu.A, 100) # subsampling without replacement
       b <- sample(abu.B, 100)
       div.A[i] <- length(levels(factor(a)))    
       div.B[i] <- length(levels(factor(b)))
           }  

  mean(div.A)
  mean(div.B)

  # significant difference?
   t.test(div.A, div.B)

############

  # Compare evenness values
    psp.A <- A/sum(A)  # Proportions of each individual in assemblage A
    psp.B <- B/sum(B)  # Proportions of each individual in assemblage B

   sha.A <- psp.A*log(psp.A)  # For Shannon-Index 
   sha.B <- psp.B*log(psp.B)  # For Shannon-Index 
   p2.A <- psp.A^2
   p2.B <- psp.B^2
     H.A <- -sum(sha.A)             ## Shannon A
     H.B <- -sum(sha.B)             ## Shannon B
     J.A <- H.A/log(S)              ## J A
     J.B <- H.B/log(S)              ## J B
     pie.A <- S/(S-1)*(1-sum(p2.A)) ## PIE A 
     pie.B <- S/(S-1)*(1-sum(p2.B)) ## PIE A 

############

# occurrences weighted by-list (OW) subsampling
          coll.A <- A^1.5
          coll.B <- B^1.3

# Rarefaction
     abu.A <- rep(spec, round(coll.A))
     abu.B <- rep(spec, round(coll.B))

     trial <- 200 # subsampling trials

        rardiv.A <- numeric()
         div.A <- numeric()
        rardiv.B <- numeric()
          div.B <- numeric()

     for (i in 1:trial) {
       a <- sample(abu.A, 100) # subsampling without replacement
       b <- sample(abu.B, 100)
       div.A[i] <- length(levels(factor(a)))    
       div.B[i] <- length(levels(factor(b)))
           }  

  mean(div.A)
  mean(div.B)

  # significant difference?
   t.test(div.A, div.B)



