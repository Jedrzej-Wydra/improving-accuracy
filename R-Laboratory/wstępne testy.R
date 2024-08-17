T <- data.frame(x=rnorm(10000, 0, 3),y=rnorm(10000, 4, 5))
library(ggplot2)
s <- 1
ggplot(T,aes(x,y))+geom_point()+geom_abline(col="red",slope = 0, intercept = 4-s*5)+geom_abline(col="red",slope = 0, intercept = 4+s*5)+geom_vline(xintercept = s*3, col="red")+geom_vline(xintercept = -s*3, col="red")+geom_abline(slope = 0,intercept = 4,col="white")+geom_vline(xintercept = 0,col="white")
cor(T,method="pearson")

X=rnorm(10000, 0, 3)
Y=-0.5*X+rnorm(10000, 4, 5)
T2 <- data.frame(x=X, y=Y)
ggplot(T2,aes(x,y))+geom_point()+geom_abline(col="red",slope = 0, intercept = 4-s*5)+geom_abline(col="red",slope = 0, intercept = 4+s*5)+geom_vline(xintercept = s*3, col="red")+geom_vline(xintercept = -s*3, col="red")+geom_abline(slope = 0,intercept = 4,col="white")+geom_vline(xintercept = 0,col="white")+geom_abline(slope = -5/3,intercept = 4,col="green")+geom_smooth(method = "lm",se=F,col="purple")+geom_abline(intercept = 4,slope=-1.714459,col="orange",size=1)+geom_abline(intercept = 4,slope=-6.029901,col="purple",size=1)

cor(T2,method="pearson")

summary(lm(y~x,T2))
B <- coef(lm(x~y,T2))

cov(T2)

lmodel2(y~x,T2)

ggplot(T2,aes(x,y))+geom_point()+geom_smooth(se=F)+theme_minimal()

ggplot(T2,aes(x,y))+geom_point()+geom_abline(col="red",slope = 0, intercept = 4-s*5)+geom_vline(xintercept = s*3, col="red")+geom_abline(slope = 0,intercept = 4,col="orange")+geom_vline(xintercept = 0,col="orange")+geom_smooth(method = "lm",se=F,col="purple")+geom_abline(intercept = 4,slope=-1.714459,col="purple",size=1)+theme_minimal()

ggplot(T2,aes(x,y))+geom_point()+geom_smooth(method = "lm",se=F)+geom_abline(slope = 1/B[2],intercept=4,col="red",size=1)+geom_abline(slope = -1.7339208, intercept = 4,col="orange",size=1)+theme_minimal()

X1=rnorm(10000, 0, 3)
Y1=-X1+rnorm(10000, 4, 5)
T3 <- data.frame(x=X1, y=Y1)
C <- coef(lm(x~y,T3))

ggplot(T3,aes(x,y))+geom_point()+geom_smooth(method = "lm",se=F)+geom_abline(slope = 1/C[2],intercept=4,col="red",size=1)+geom_abline(slope = -1.972345, intercept = 4,col="orange",size=1)+theme_minimal()
cov(T3)

X2=rnorm(10000, 0, 3)
Y2=rnorm(10000, 4, 5)
T4 <- data.frame(x=X2, y=Y2)
D <- coef(lm(x~y,T4))

ggplot(T4,aes(x,y))+geom_point()+geom_smooth(method = "lm",se=F)+geom_abline(slope = 1/D[2],intercept=4,col="red",size=1)+geom_abline(slope = -1.66447850, intercept = 4,col="orange",size=1)+theme_minimal()
cov(T4)
