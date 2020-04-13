 setwd("c:/daten/PBDB")

  dat <- read.csv("Scleractinia.csv", header=T) 

install.packages("https://github.com/adamkocsis/divDyn/raw/master/_bin/divDyn_0.2.9.zip", repos=NULL)


library(divDyn)  
data(stages)
dat$me_ma <- apply(dat[, c("max_ma", "min_ma")], 1, mean)

################

# Assign stages by myr 
bin <- rep(NA, nrow(dat))
for (i in 1:nrow(stages)-1) {
  st.n <- stages$num[i]
  b <- stages$bottom[i]
  t <- stages$top[i]
  bin <- replace(bin, which(dat$me_ma<=b & dat$me_ma>=t), st.n)
}

data <- cbind(dat, bin)


res <- divDyn(data, tax="genus", bin="bin")

windows(h=5, w=7)
plotTS(stages, shading="series", boxes="per", xlim=c(250,0), 
       ylab="range-through diversity (genera)", ylim=c(0,300))
lines(stages$mid[1:94], res$divRT, col="red", lwd=2)
