T <- data.frame(x=rnorm(10000, 0, 3),y=rnorm(10000, 4, 5))
library(ggplot2)
s <- 2
ggplot(T,aes(x,y))+geom_point()+geom_abline(col="red",slope = 0, intercept = 4-s*5)+geom_abline(col="red",slope = 0, intercept = 4+s*5)+geom_vline(xintercept = s*3, col="red")+geom_vline(xintercept = -s*3, col="red")+geom_abline(slope = 0,intercept = 4,col="white")+geom_vline(xintercept = 0,col="white")
cor(T,method="pearson")

X=rnorm(10000, 0, 3)
Y=-X+rnorm(10000, 4, 5)
T2 <- data.frame(x=X, y=Y)
ggplot(T2,aes(x,y))+geom_point()+geom_abline(col="red",slope = 0, intercept = 4-s*5)+geom_abline(col="red",slope = 0, intercept = 4+s*5)+geom_vline(xintercept = s*3, col="red")+geom_vline(xintercept = -s*3, col="red")+geom_abline(slope = 0,intercept = 4,col="white")+geom_vline(xintercept = 0,col="white")+geom_smooth(method = "lm",se=F,col="purple")+geom_abline(intercept = 4,slope=-1.940038,col="orange",size=1)+geom_abline(intercept = 4,slope=-3.755163,col="purple",size=1)+geom_abline(slope = 0,intercept = mean(T2$y)-s*sd(T2$y),col="yellow")+geom_vline(xintercept = mean(T2$x)+s*sd(T2$x),col="yellow") #+geom_abline(slope = -5/3,intercept = 4,col="green")

cor(T2,method="pearson")

summary(lm(y~x,T2))
B <- coef(lm(x~y,T2))

cov(T2)

lmodel2(y~x,T2)

4-s*5
mean(T2$y)-s*sd(T2$y)
mean(T2$y)
sd(T2$y)
mean(T2$x)
