library(readxl)
library(dplyr)
library(ggplot2)
library(MLmetrics)
#C_maxillosus_dane_do_ostatecznych_analiz <- read_excel(path)

#Necrodes_littoralis <- read_excel(path)

#dane <- data.frame(X=C_maxillosus_dane_do_ostatecznych_analiz$d?ugo??,Y=C_maxillosus_dane_do_ostatecznych_analiz$`k przy 11,58 og?lny`)
#dane <- data.frame(X=Necrodes_littoralis$LENGTH,Y=Necrodes_littoralis$`ADD 8,49`)

P=sample(dane$X,10)
L=sample(P,1)

KM <- 405.156
SM <- 14.63
#KM <- 469
#SM <- 25

Kt <- function(k=KM,sek=SM,sel=sd(P),l,ml=mean(P))
{
  return(k-(sek/sel)*(l-ml))
}

PU1 <- function(k=KM,ml=mean(P),l)
{
  p1=-25*l+k+25*ml
  p2=-0.05*l+k+0.05*ml
  if(l>ml) return(c(p1,p2)) else return(c(p2,p1))
}

PU2 <- function(k=KM,ml=mean(P),l)
{
  p1=-25*l+k+25*ml
  p2=Kt(sel=sd(P,l=l,ml=ml))
  ifelse(l<ml,c(p1,p2),c(p2,p1))
}

Kt(l=L)
PU1(l=L)

mean(subset(dane, dane$X==20.5)$Y)

length(subset(dane$X,dane$X==16.5))

samples <- list()
predyktor <- sample(dane$X,1)
for(i in 2:20)
{
samples[[i-1]] <- c(predyktor,sample(dane$X,i-1))
}
FF <- function(x,y=1) sample(x,size = y)
wielkosc_proby <- 2:20
test <- data.frame(X=wielkosc_proby, SD=sapply(samples, sd), M=sapply(samples, mean), L=rep(predyktor,length(wielkosc_proby)))
View(test)

test <- test %>% mutate(KT=Kt(sel=SD,ml=M,l=L))

ggplot(test,aes(x=X,y=KT))+geom_line()+geom_point()+geom_abline(slope=0,intercept = mean(subset(dane,dane$X==predyktor)$Y))+geom_abline(slope=0,intercept = KM)

MSE(test$KT,rep(mean(subset(dane,dane$X==predyktor)$Y),length(test$KT)))
MSE(rep(KM,length(test$KT)),rep(mean(subset(dane,dane$X==predyktor)$Y),length(test$KT)))

