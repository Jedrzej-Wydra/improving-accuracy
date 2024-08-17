CZAS <- Sys.time()

#library(readxl)
#library(dplyr)
#library(ggplot2)
#library(MLmetrics)


Necrodes_littoralis <- read_excel(path)

dane <- data.frame(X=Necrodes_littoralis$LENGTH,Y=Necrodes_littoralis$`ADD 8,49`)

P=sample(dane$X,10)
L=sample(P,1)

KM <- 469
SM <- 25

Kt <- function(k=KM,sek=SM,sel=sd(P),l,ml=mean(P))
{
  return(k-(sek/sel)*(l-ml))
}

sample_10 <- sample(dane$X,10)

Kt(sel=sd(sample_10),ml=mean(sample_10),l=13)

SG <- matrix(c(25^2,0,0,sd(sample_10)),2)

pmvnorm(lower=c(504.1426 ,-Inf), upper=c(Inf,13), mean=c(469,mean(sample_10)), sigma=SG)[[1]]
P <- 0
SG <- 0
COV=seq(-25*sd(sample_10),0,by=0.01)
for(i in 1:5869)
{
  SG <- matrix(c(25^2,COV[i],COV[i],sd(sample_10)),2)
  P <- P+pmvnorm(lower=c(504.1426 ,-Inf), upper=c(Inf,13), mean=c(469,mean(sample_10)), sigma=SG)[[1]]
}
(P/5869)/pnorm(13,mean(sample_10),sd(sample_10))

      