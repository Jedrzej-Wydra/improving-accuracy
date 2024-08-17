dane <- Necrodes_littoralis
dane <- data.frame(K=dane$`ADD 8,49`, L=dane$LENGTH)

min(dane$K)
max(dane$K)
mean(dane$K)
sd(dane$K)

dim(dane)

probka <- sample(1:954, 54)

proba <- dane$L[probka]
dane <- dane[-probka,]

lmodel2(L~K,dane)
lm(L~K,dane)

a1=cov(dane)[1,2]/var(dane$K)
b1=mean(dane$L)-a1*mean(dane$K)
a2=-sqrt(var(dane$L)/var(dane$K))
b2=mean(dane$L)-a2*mean(dane$K)

ggplot(dane, aes(K, L))+geom_point()+geom_smooth(se=F,method = "lm")+geom_abline(slope = a2, intercept = b2, col="red",size=1)+theme_minimal()

mean(proba)
sd(proba)

sample(proba,1)

469-(25/1.88)*(15-17.29)

-25*15+469+25*17.29

-0.05*15+469+0.05*17.29

(15-b1)/a1
(15-b2)/a2

a11=cov(dane)[1,2]/var(dane$L)
b11=mean(dane$K)-a11*mean(dane$L)
a21=-sqrt(var(dane$K)/var(dane$L))
b21=mean(dane$K)-a21*mean(dane$L)

ggplot(dane, aes(L, K))+geom_point()+geom_smooth(se=F,method = "lm")+geom_abline(slope = a21, intercept = b21, col="red",size=1)+theme_minimal()
lmodel2(K~L,dane)

a11*15+b11

dane2 <- Necrodes_littoralis
dane2 <- data.frame(K=dane2$`ADD 8,49`, L=dane2$LENGTH)

mean(subset(dane2,L==15,c(K))$K)
sd(subset(dane2,L==15,c(K))$K)

sd(dane$K)*sd(dane$L)

SG <- matrix(c(25^2,-43.74,-43.74,3.55),2)

pmvnorm(lower=c(499.45,-Inf), upper=c(Inf,15), mean=c(469,17.29), sigma=SG)[[1]]
P <- 0
COV=seq(-25*1.79,-25,by=0.01)
for(i in 1:1976)
{
  SG <- matrix(c(25^2,COV[i],COV[i],3.55),2)
  P <- P+pmvnorm(lower=c(499.45,-Inf), upper=c(Inf,15), mean=c(469,17.29), sigma=SG)[[1]]
}
P <- P/1976
P/pnorm(15,17.29,3.55)
dim(subset(dane2,L<=15&K<=499.45))
62/954
cov(dane2)
