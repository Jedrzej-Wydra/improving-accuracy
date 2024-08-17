Necrodes_littoralis <- read_excel(path)

K <- NULL
for(i in 1:10)
{
  a <- as.numeric(levels(factor(Necrodes_littoralis$TEMP.)))[i]
  K[i] <- median(subset(Necrodes_littoralis,Necrodes_littoralis$TEMP.==a)$`TIME (D)`)

}
sd(K)

match(K,Necrodes_littoralis$`TIME (D)`)



length(order(subset(Necrodes_littoralis,Necrodes_littoralis$TEMP.==14)$`TIME (D)`))
order(subset(Necrodes_littoralis,Necrodes_littoralis$TEMP.==14)$`TIME (D)`)[floor(length(subset(Necrodes_littoralis,Necrodes_littoralis$TEMP.==14)$`TIME (D)`)/2)]
subset(Necrodes_littoralis,Necrodes_littoralis$TEMP.==14)$`TIME (D)`[c(51,42)]
mean(subset(Necrodes_littoralis,Necrodes_littoralis$TEMP.==14)$`TIME (D)`[c(51,42)])

K[1] <- subset(Necrodes_littoralis,Necrodes_littoralis$TEMP.==14)$`TIME (D)`[c(42)]

dane <- Necrodes_littoralis[match(K,Necrodes_littoralis$`TIME (D)`),]

dane <- data.frame(Days=dane$`TIME (D)`,Temp = dane$TEMP.)

dane %>% mutate(DT=Days*Temp) -> dane

dane2 <- as.data.frame(dane[c(-4,-8),])

ggplot(dane2,aes(Days,DT))+geom_text(aes(label=Temp))+geom_smooth(method = "lm")+stat_function(fun = function(x) 464.9671+x*8.575708,color="red",size=1)

summary(lm(DT~Days,dane[c(-4,-8),]))

sma(DT~Days,dane[c(-4,-8),])

dane3 <- data.frame(Days=Necrodes_littoralis$`TIME (D)`,Temp = Necrodes_littoralis$TEMP.)

dane3 %>% mutate(DT=Days*Temp) -> dane3

ggplot(dane3,aes(Days,DT))+geom_point()+geom_smooth(method = "lm")

summary(lm(DT~Days,dane3))
sma(DT~Days,dane3)

ggplot(Necrodes_littoralis[match(K,Necrodes_littoralis$`TIME (D)`),], aes(x=LENGTH,y=`TIME (D)`))+geom_point()+geom_smooth(method = "lm")

L <- NULL
for(i in 1:18)
{
  a <- as.numeric(levels(factor(Necrodes_littoralis$LENGTH)))[i]
  A <- subset(Necrodes_littoralis,Necrodes_littoralis$LENGTH==a)$`TIME (D)`
  AK <- subset(Necrodes_littoralis,Necrodes_littoralis$LENGTH==a)$`ADD 8,49`
  M <- order(A)[ceiling(length(order(A))/2)]
  L[i] <- AK[M]
  
}
L

daneK <- data.frame(K=L,Length=as.numeric(levels(factor(Necrodes_littoralis$LENGTH))))

summary(lm(K~Length,daneK))
sma(K~Length,daneK)

K_a <- function(x)
{
  469-0.5*sqrt(10)*(25/sd(daneK$Length))*(x-mean(daneK$Length))
}

ggplot(daneK, aes(x=Length,y=K))+geom_point()+geom_smooth(method = "lm")+stat_function(fun = function(x) 862.6484+x*(-22.98531),color="red",size=1)+stat_function(fun = K_a,color="green",size=1)+stat_function(fun = function(x) 469,color="orange",size=1)

fitted <- c(MSE(795.490+daneK$Length*(-19.079),daneK$K),
            MSE(862.6484+daneK$Length*(-22.98531),daneK$K),
            MSE(K_a(daneK$Length),daneK$K),
            MSE(rep(469,18),daneK$K))
model <- c("lm",
           "sma",
           "Ka",
           "general")

comparison <- data.frame(MODEL=model,MSE=fitted,color=model)

fitted2 <- c(MAPE(795.490+daneK$Length*(-19.079),daneK$K),
            MAPE(862.6484+daneK$Length*(-22.98531),daneK$K),
            MAPE(K_a(daneK$Length),daneK$K),
            MAPE(rep(469,18),daneK$K))

comparison2 <- data.frame(MODEL=model,MAPE=fitted2,color=model)

ggplot(comparison,aes(x=reorder(MODEL,MSE),y=MSE,fill=color))+geom_bar(stat="identity",show.legend = FALSE)

ggplot(comparison2,aes(x=reorder(MODEL,MAPE),y=MAPE,fill=color))+geom_bar(stat="identity",show.legend = FALSE)

daneTEST <- data.frame(X=Necrodes_littoralis$LENGTH,Y=Necrodes_littoralis$`ADD 8,49`)

daneTEST %>% mutate(Ka=K_a(X)) -> daneTEST

data.frame(Ka=MSE(daneTEST$Ka,daneTEST$Y),
General=MSE(rep(469, length(daneTEST$Y)),daneTEST$Y),
lm=MSE(predict(lm(Y~X,daneTEST),newdata = list(X=daneTEST$X)),daneTEST$Y))
