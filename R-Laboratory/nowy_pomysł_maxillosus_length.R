C_maxillosus <- read_excel(path)

L <- NULL
for(i in 1:19)
{
  a <- as.numeric(levels(factor(C_maxillosus$długość)))[i]
  A <- subset(C_maxillosus,C_maxillosus$długość==a)$`k przy 11,58 ogólny`
  M <- order(A)[ceiling(length(order(A))/2)]
  L[i] <- A[M]
  
}
L

daneK <- data.frame(K=L,Length=as.numeric(levels(factor(C_maxillosus$długość))))


summary(lm(K~Length,daneK))
sma(K~Length,daneK)

K_a <- function(x)
{
  405.156-0.5*sqrt(7)*(14.63/sd(daneK$Length))*(x-mean(daneK$Length))
}

ggplot(daneK, aes(x=Length,y=K))+geom_point()+geom_smooth(method = "lm")+stat_function(fun = function(x) 624.9984+x*(-10.851831),color="red",size=1)+stat_function(fun = K_a,color="green",size=1)+stat_function(fun = function(x) 405.156,color="orange",size=1)

fitted <- c(MSE(581.034+daneK$Length*(-8.597),daneK$K),
            MSE(624.9984+daneK$Length*(-10.851831),daneK$K),
            MSE(K_a(daneK$Length),daneK$K),
            MSE(rep(405.156,19),daneK$K))
model <- c("lm",
           "sma",
           "Ka",
           "general")

comparison <- data.frame(MODEL=model,MSE=fitted,color=model)

fitted2 <- c(MAPE(579.600+daneK$Length*(-8.547),daneK$K),
             MAPE(625.2110+daneK$Length*(-10.892135),daneK$K),
             MAPE(K_a(daneK$Length),daneK$K),
             MAPE(rep(405.156,19),daneK$K))

comparison2 <- data.frame(MODEL=model,MAPE=fitted2,color=model)

ggplot(comparison,aes(x=reorder(MODEL,MSE),y=MSE,fill=color))+geom_bar(stat="identity",show.legend = FALSE)

ggplot(comparison2,aes(x=reorder(MODEL,MAPE),y=MAPE,fill=color))+geom_bar(stat="identity",show.legend = FALSE)

daneTEST <- data.frame(X=C_maxillosus$długość,Y=C_maxillosus$`k przy 11,58 ogólny`)

daneTEST %>% mutate(Ka=K_a(X)) -> daneTEST

data.frame(Ka=MSE(daneTEST$Ka,daneTEST$Y),
           General=MSE(rep(405.156, length(daneTEST$Y)),daneTEST$Y),
           lm=MSE(predict(lm(Y~X,daneTEST),newdata = list(X=daneTEST$X)),daneTEST$Y))
