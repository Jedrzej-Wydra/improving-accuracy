dane <- Necrodes_littoralis
dane <- data.frame(K=dane$`ADD 8,49`, L=dane$WEIGHT)

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

469-(25/53.64)*(265.33-158.49)

-25*265.33+469+25*158.49

-0.05*265.33+469+0.05*158.49

(265.33-b1)/a1
(265.33-b2)/a2

a11=cov(dane)[1,2]/var(dane$L)
b11=mean(dane$K)-a11*mean(dane$L)
a21=-sqrt(var(dane$K)/var(dane$L))
b21=mean(dane$K)-a21*mean(dane$L)

ggplot(dane, aes(L, K))+geom_point()+geom_smooth(se=F,method = "lm")+geom_abline(slope = a21, intercept = b21, col="red",size=1)+theme_minimal()
lmodel2(K~L,dane)

a11*265.33+b11

dane2 <- Necrodes_littoralis
dane2 <- data.frame(K=dane2$`ADD 8,49`, L=dane2$WEIGHT)

mean(subset(dane2,L<=266.25&L>=264.75,c(K))$K)

sd(dane$K)*sd(dane$L)

SG <- matrix(c(25^2,-43.74,-43.74,3.55),2)
var(proba)
pmvnorm(lower=-Inf, upper=c(499.45,15), mean=c(469,17.29), sigma=SG)[[1]]
P <- 0
COV=seq(-2443.4,-25,by=0.01)
for(i in 1:241841)
{
  SG <- matrix(c(25^2,COV[i],COV[i],2877.07),2)
  P <- P+pmvnorm(lower=-Inf, upper=c(419.21,265.33), mean=c(469,158.49), sigma=SG)[[1]]
}
P <- P/241841
P
dim(subset(dane2,L<=265.33&K<=419.21))
88/954
cov(dane2)
