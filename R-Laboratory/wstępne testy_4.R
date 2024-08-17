T <- data.frame(x=rnorm(10000, 0, 3),y=rnorm(10000, 4, 5))
ggplot(T,aes(x,y))+geom_point()+geom_smooth(method = "lm",se=F)

cov(T)

lm(x~y,T)

X=rnorm(10000, 0, 3)
Y=X+rnorm(10000, 4, 5)


T2 <- data.frame(x=X*cos(pi/1.5)-Y*sin(pi/1.5), y=X*sin(pi/1.5)+Y*cos(pi/1.5))

cov(T2)

ggplot(T2,aes(x,y))+geom_point()+geom_smooth(method = "lm",se=F)+scale_y_continuous(limits = c(-10,10))

cor(T2,method = "pearson")

summary(lm(y~x,T2))

var(Y)
mean(Y)
var(X)
mean(X)
