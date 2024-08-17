T <- data.frame(x=rnorm(10000, 0, 3),y=rnorm(10000, 4, 5))
library(ggplot2)
s <- 2
ggplot(T,aes(x,y))+geom_point()+geom_abline(col="red",slope = 0, intercept = 4-s*5)+geom_abline(col="red",slope = 0, intercept = 4+s*5)+geom_vline(xintercept = s*3, col="red")+geom_vline(xintercept = -s*3, col="red")+geom_abline(slope = 0,intercept = 4,col="white")+geom_vline(xintercept = 0,col="white")
cor(T,method="pearson")

X=rnorm(10000, 0, 3)
Y=rnorm(10000, 4, 5)
T2 <- data.frame(x=X, y=Y)
ggplot(T,aes(x,y))+geom_point()+geom_abline(col="red",slope = 0, intercept = 4-s*5)+geom_abline(col="red",slope = 0, intercept = 4+s*5)+geom_vline(xintercept = s*3, col="red")+geom_vline(xintercept = -s*3, col="red")+geom_abline(slope = 0,intercept = 4,col="white")+geom_vline(xintercept = 0,col="white")+geom_smooth(method = "lm",se=F,col="purple")+geom_abline(intercept = 4,slope=-1.66886037,col="orange",size=1)+geom_abline(intercept = 4,slope=261.4165,col="purple",size=1)+geom_abline(slope = 0,intercept = mean(T$y)-s*sd(T$y),col="yellow")+geom_vline(xintercept = mean(T$x)+s*sd(T$x),col="yellow") #+geom_abline(slope = -5/3,intercept = 4,col="green")

cor(T,method="pearson")

summary(lm(y~x,T))
B <- coef(lm(x~y,T))

cov(T)

lmodel2(y~x,T2)

4-s*5
mean(T2$y)-s*sd(T2$y)
mean(T$y)
sd(T$y)
mean(T$x)
