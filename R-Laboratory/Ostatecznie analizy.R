library(dplyr)
library(readxl)
library(mvtnorm)
library(ggplot2)
library(latex2exp)
C_maxillosus <- C_maxillosus_dane_do_ostatecznych_analiz
Necrodes_littoralis <- Necrodes_littoralis 
ggplotly(ggplot2:: ggplot(C_maxillosus,aes(y=`k przy 11,58 ogólny`, x=długość))+
  geom_point()+
  ylab('True k')+
  xlab('length')+
  geom_smooth(method = "lm")+
  theme_minimal()+
  ggtitle('Creophilus maxillosus L. (Staphylinidae)'))

ggplot2:: ggplot(Necrodes_littoralis,aes(y=`ADD 8,49`, x=LENGTH))+
  geom_point()+
  ylab('True k')+
  xlab('weight')+
  geom_smooth(method = "lm")+
  theme_minimal()+
  ggtitle('Necrodes littoralis L. (Silphidae)')+
  scale_x_continuous(breaks = seq(from=0,to=400,by=50))

Necrodes <- data.frame(Time=Necrodes_littoralis$`TIME (D)`,
                       Temperature=Necrodes_littoralis$TEMP.,
                       Length=Necrodes_littoralis$LENGTH,
                       Weight=Necrodes_littoralis$WEIGHT,
                       k=Necrodes_littoralis$`ADD 8,49`)

Maxillosus <- data.frame(Time=C_maxillosus$`Total immature development`,
                         Temperature=C_maxillosus$T,
                         Length=C_maxillosus$długość,
                         Weight=C_maxillosus$waga,
                         k=C_maxillosus$`k przy 11,58 ogólny`)

Necrodes %>% group_by(Length) %>% summarise(mk=median(k)) -> Necrodes2
Maxillosus %>% group_by(Length) %>% summarise(mk=median(k)) -> Maxillosus2

Necrodes %>% group_by(Length) %>% summarise(sd=sd(k)) -> Necrodes_sd
#Maxillosus[vec,] %>% group_by(Length) %>% summarise(sd=sd(k)) -> Maxillosus_sd

smatr:: sma(mk~Length,Necrodes2)
broom:: tidy(lm(mk~Length,Necrodes2))
smatr:: sma(mk~Length,Maxillosus2)
broom:: tidy(lm(mk~Length,Maxillosus2))
summary(lm(mk~Length,Necrodes2))
summary(lm(mk~Length,Maxillosus2))

ggplot2:: ggplot(Necrodes2,aes(Length,mk))+
  geom_point()+
  geom_smooth(method = "lm",se=F,color="#00B0F6")+
  stat_function(fun = function(x) -17.35671*x+761.1040,size=1,color="#F8766D")+
  stat_function(fun = function(x) 469,color="#E58700")+
  xlab('Length')+
  ylab('True k')+
  ggtitle('Necrodes littoralis L. (Silphidae)')+
  theme_bw()+
  scale_x_continuous(breaks = 12:23)+
  theme(plot.title = element_text(face = "italic"))+
  annotate("text", x = 22-0.5, y = -17.35671*22+761.1040-3, label = "RMA",color="#F8766D")+
  annotate("text", x = 22-0.5, y = -17.35671*22+761.1040+27, label = "OLS",color="#00B0F6")+
  annotate("text", x = 22-0.5, y = 478, label = "Constant k",color="#E58700")

ggplot2:: ggplot(Maxillosus2,aes(Length,mk))+
  geom_point()+
  geom_smooth(method = "lm",se=F,color="#00B0F6")+
  stat_function(fun = function(x) -10.255145*x+616.0064,size=1,color="#F8766D")+
  stat_function(fun = function(x) 405.156,color="#E58700")+
  xlab('Length')+
  ylab('True k')+
  ggtitle('Creophilus maxillosus L. (Staphylinidae)')+
  theme_bw()+
  scale_x_continuous(breaks = 14:25)+
  theme(plot.title = element_text(face = "italic"))+
  annotate("text", x = 24-0.3, y = -10.255145*24+616.0064-3, label = "RMA",color="#F8766D")+
  annotate("text", x = 24-0.3, y = -10.255145*24+616.0064+19, label = "OLS",color="#00B0F6")+
  annotate("text", x = 24-0.3, y = 410.156, label = "Constant k",color="#E58700")

mse_lm_necrodes <- MLmetrics:: MSE(Necrodes2$mk,
                                   predict(lm(mk~Length,Necrodes2),
                                           newdata = list(Length=Necrodes2$Length)))
mape_lm_necrodes <- MLmetrics:: MAPE(Necrodes2$mk,
                                   predict(lm(mk~Length,Necrodes2),
                                           newdata = list(Length=Necrodes2$Length)))
mse_sma_necrodes <- MLmetrics:: MSE(Necrodes2$mk,
                                    -17.35671*Necrodes2$Length+761.1040)
mape_sma_necrodes <- MLmetrics:: MAPE(Necrodes2$mk,
                                    -17.35671*Necrodes2$Length+761.1040)
mse_general_necrodes <- MLmetrics:: MSE(Necrodes2$mk,
                                        rep(469,length(Necrodes2$mk)))
mape_general_necrodes <- MLmetrics:: MAPE(Necrodes2$mk,
                                        rep(469,length(Necrodes2$mk)))
mse_lm_maxillosus <- MLmetrics:: MSE(Maxillosus2$mk,
                                   predict(lm(mk~Length,Maxillosus2),
                                           newdata = list(Length=Maxillosus2$Length)))
mape_lm_maxillosus <- MLmetrics:: MAPE(Maxillosus2$mk,
                                     predict(lm(mk~Length,Maxillosus2),
                                             newdata = list(Length=Maxillosus2$Length)))
mse_sma_maxillosus <- MLmetrics:: MSE(Maxillosus2$mk,
                                     -10.255145*Maxillosus2$Length+616.0064)
mape_sma_maxillosus <- MLmetrics:: MAPE(Maxillosus2$mk,
                                     -10.255145*Maxillosus2$Length+616.0064)
mse_general_maxillosus <- MLmetrics:: MSE(Maxillosus2$mk,
                                        rep(405.156,length(Maxillosus2$mk)))
mape_general_maxillosus <- MLmetrics:: MAPE(Maxillosus2$mk,
                                         rep(405.156,length(Maxillosus2$mk)))

comparison <- data.frame(Kryterium=c(rep(c("MSE","MAPE"),3)),
                         Metoda=c("OLS","OLS","RMA","RMA","Constant k from the general model","Constant k from the general model"),
                         Necrodes=c(mse_lm_necrodes,
                                    mape_lm_necrodes,
                                    mse_sma_necrodes,
                                    mape_sma_necrodes,
                                    mse_general_necrodes,
                                    mape_general_necrodes),
                         Maxillosus=c(mse_lm_maxillosus,
                                      mape_lm_maxillosus,
                                      mse_sma_maxillosus,
                                      mape_sma_maxillosus,
                                      mse_general_maxillosus,
                                      mape_general_maxillosus))

comparison

ggplot2:: ggplot(subset(comparison,comparison$Kryterium=='MSE'),aes(x=reorder(Metoda,Necrodes),y=Necrodes,fill=Metoda))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ylab("Mean Squared Error")+
  xlab("Method")+
  ggtitle('Necrodes littoralis L. (Silphidae)')+
  theme_bw()+ 
  scale_fill_manual("legend", values = c("Constant k from the general model" = "#E58700", "RMA" = "#F8766D", "OLS" = "#00B0F6"))+
  theme(plot.title = element_text(face = "italic"))

ggplot2:: ggplot(subset(comparison,comparison$Kryterium=='MAPE'),aes(x=reorder(Metoda,Necrodes),y=Necrodes,fill=Metoda))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ylab("MAPE")+
  xlab("Method")+
  ggtitle('Necrodes littoralis L. (Silphidae)')+
  theme_minimal()

ggplot2:: ggplot(subset(comparison,comparison$Kryterium=='MSE'),aes(x=reorder(Metoda,Maxillosus),y=Maxillosus,fill=Metoda))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ylab("Mean Squared Error")+
  xlab("Method")+
  ggtitle('Creophilus maxillosus L. (Staphylinidae)')+
  theme_bw()+ 
  scale_fill_manual("legend", values = c("Constant k from the general model" = "#E58700", "RMA" = "#F8766D", "OLS" = "#00B0F6"))+
  theme(plot.title = element_text(face = "italic"))

ggplot2:: ggplot(subset(comparison,comparison$Kryterium=='MAPE'),aes(x=reorder(Metoda,Maxillosus),y=Maxillosus,fill=Metoda))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ylab("MAPE")+
  xlab("Method")+
  ggtitle('Creophilus maxillosus L. (Staphylinidae)')+
  theme_minimal()

Ka <- function(x,k,se,lmax,lmin,m)
{
  k-(se*sqrt(3*m)/(lmax-lmin))*(x-0.5*(lmax+lmin))
}

plot1 <- 
  ggplot2:: ggplot(Necrodes2,aes(Length,mk))+
  geom_point(size=5)+
#  geom_smooth(method = "lm",se=T,color="#00B0F6")+
  stat_function(fun = function(x) -17.35671*x+761.1040,size=2,color="#F8766D")+
  xlab('Length [mm]')+
  ylab('True k [ADD]')+
  ggtitle(expression(paste(italic('Necrodes littoralis'))))+
  theme_bw()+
  scale_x_continuous(breaks = 12:23)+
  stat_function(fun = Ka,args = list(k=469,se=25,lmax=22,lmin=12,m=8),size=2,col="#6BB100")+
  stat_function(fun = function(x) 469,color="#E58700",size=2)+
  annotate("text", x = 22-1, y = -17.35671*22+761.1040+10, label = "RMA",color="#F8766D", angle=-33.69007,size=5)+
  #annotate("text", x = 22-0.5, y = -17.35671*22+761.1040+27, label = "OLS",color="#00B0F6")+
  annotate("text", x = 22-1, y = -17.35671*22+761.1040+34, label = TeX("$K_c$ model"),color="#6BB100",angle = -25,size=5)+
  annotate("text", x = 22-1, y = 464, label = "constant k",color="#E58700",size=5)+
  theme(text = element_text(size=20))

plot2 <- ggplot2:: ggplot(Maxillosus2,aes(Length,mk))+
  geom_point(size=5)+
  #geom_smooth(method = "lm",se=T,color="#00B0F6")+
  stat_function(fun = function(x) -10.255145*x+616.0064,size=2,color="#F8766D")+
  xlab('Length [mm]')+
  ylab('')+
  ggtitle(expression(paste(italic('Creophilus maxillosus'))))+
  theme_bw()+
  stat_function(fun = function(x) 405.156,color="#E58700",size=2)+
  scale_x_continuous(breaks = 14:25)+
  stat_function(fun = Ka,args = list(k=405.156,se=14.63,lmax=24,lmin=15,m=7),size=2,col="#6BB100")+
  theme(text = element_text(size=20))+
  annotate("text", x = 15.6, y = -10.255145*15+616.0064-11, label = "RMA",color="#F8766D",angle=-31,size=5)+
  #annotate("text", x = 15.3, y = -10.255145*15+616.0064-19, label = "OLS",color="#00B0F6")+
  annotate("text", x = 15.6, y = -10.255145*15+616.0064-33, label = TeX("$K_c$ model"),color="#6BB100",angle=-23,size=5)+
  annotate("text", x = 15.7, y = 410.156-9, label = "constant k",color="#E58700",size=5)

library(gridExtra)

mse_ka_necrodes <- MLmetrics:: MSE(Necrodes2$mk,
                                   Ka(x=Necrodes2$Length,k=469,se=25,lmax=22,lmin=12,m=8))
mape_ka_necrodes <- MLmetrics:: MAPE(Necrodes2$mk,
                                   Ka(x=Necrodes2$Length,k=469,se=25,lmax=22,lmin=12,m=8))
mse_ka_maxillosus <- MLmetrics:: MSE(Maxillosus2$mk,
                                    Ka(x=Maxillosus2$Length,k=405.156,se=14.63,lmax=24,lmin=15,m=7))
mape_ka_maxillosus <- MLmetrics:: MAPE(Maxillosus2$mk,
                                     Ka(x=Maxillosus2$Length,k=405.156,se=14.63,lmax=24,lmin=15,m=7))

comparison2 <- data.frame(Kryterium=c(rep(c("MSE","MAPE"),3)),
                         Metoda=c("RMA","RMA","constant k","constant k","model","model"),
                         Necrodes=c(mse_sma_necrodes,
                                    mape_sma_necrodes,
                                    mse_general_necrodes,
                                    mape_general_necrodes,
                                    mse_ka_necrodes,
                                    mape_ka_necrodes),
                         Maxillosus=c(mse_sma_maxillosus,
                                      mape_sma_maxillosus,
                                      mse_general_maxillosus,
                                      mape_general_maxillosus,
                                      mse_ka_maxillosus,
                                      mape_ka_maxillosus))

plot3 <- ggplot2:: ggplot(subset(comparison2,comparison$Kryterium=='MSE'),aes(x=reorder(Metoda,Necrodes),y=Necrodes,fill=Metoda))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ylab("")+
  xlab("Method")+
  ggtitle('Mean Squared Error')+
  theme_bw()+ 
  scale_fill_manual("legend", values = c('constant k' = "#E58700", 'RMA' = "#F8766D", "OLS" = "#00B0F6", 'model'="#6BB100"))+
  scale_x_discrete(labels=c('RMA'='RMA','constant k'='constant k', 'model'=parse(text=TeX("$K_c$ model"))))

plot1 <- plot1 + annotation_custom(
  grob = ggplotGrob(plot3),
  xmin = 18,
  xmax = 22,
  ymin = 475,
  ymax = 550)

ggplot2:: ggplot(subset(comparison2,comparison$Kryterium=='MAPE'),aes(x=reorder(Metoda,Necrodes),y=Necrodes,fill=Metoda))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ylab("MAPE")+
  xlab("Method")+
  ggtitle('Necrodes littoralis L. (Silphidae)')+
  theme_minimal()

plot4 <- ggplot2:: ggplot(subset(comparison2,comparison$Kryterium=='MSE'),aes(x=reorder(Metoda,Maxillosus),y=Maxillosus,fill=Metoda))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ylab("")+
  xlab("Method")+
  ggtitle('Mean Squared Error')+
  theme_bw()+ 
  scale_fill_manual("legend", values = c('constant k' = "#E58700", 'RMA' = "#F8766D", "OLS" = "#00B0F6", 'model'="#6BB100"))+
  scale_x_discrete(labels=c('RMA'='RMA','constant k'='constant k', 'model'=parse(text=TeX("$K_c$ model"))))

plot2 <- plot2 + annotation_custom(
  grob = ggplotGrob(plot4),
  xmin = 20.5,
  xmax = 24,
  ymin = 435,
  ymax = 480)

grid.arrange(plot1, plot2, ncol=2)

ggplot2:: ggplot(subset(comparison2,comparison$Kryterium=='MAPE'),aes(x=reorder(Metoda,Maxillosus),y=Maxillosus,fill=Metoda))+
  geom_bar(stat="identity",show.legend = FALSE)+
  ylab("MAPE")+
  xlab("Method")+
  ggtitle('Creophilus maxillosus L. (Staphylinidae)')+
  theme_minimal()

#Obszar ufności

KaL <- function(x,k,se,lmax,lmin,m)
{
  k-(se*sqrt(3*m)/(lmax-lmin))*(x-0.5*(lmax+lmin))*sqrt((m-1)/qchisq(0.9,df=m-1))
}

KaU <- function(x,k,se,lmax,lmin,m)
{
  k-(se*sqrt(3*m)/(lmax-lmin))*(x-0.5*(lmax+lmin))*sqrt((m-1)/qchisq(0.1,df=m-1))
}

#Przedział ufności 1

UCI <- function(x,k,se,lmax,lmin,m)
{
  Ka(x,k,se,lmax,lmin,m)+qnorm(0.8,0,1)*se
}

DCI <- function(x,k,se,lmax,lmin,m)
{
  Ka(x,k,se,lmax,lmin,m)-qnorm(0.8,0,1)*se
}

Nkal <- UCI(Necrodes2$Length,k=469,se=25,lmax=22,lmin=12,m=8)
Nkau <- DCI(Necrodes2$Length,k=469,se=25,lmax=22,lmin=12,m=8)

Nkal2 <- KaL(Necrodes2$Length,k=469,se=25,lmax=22,lmin=12,m=8)
Nkau2 <- KaU(Necrodes2$Length,k=469,se=25,lmax=22,lmin=12,m=8)

Mkal <- UCI(Maxillosus2$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)
Mkau <- DCI(Maxillosus2$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)

Mkal2 <- KaL(Maxillosus2$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)
Mkau2 <- KaU(Maxillosus2$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)

ggplot2:: ggplot(Necrodes2,aes(Length,mk))+
  geom_smooth(method = "lm",se=F)+
  stat_function(fun = function(x) -17.35671*x+761.1040,size=1,color="red")+
  xlab('Length')+
  ylab('True k')+
  ggtitle('Necrodes littoralis L. (Silphidae)')+
  theme_minimal()+
  scale_x_continuous(breaks = 12:23)+
  stat_function(fun = Ka,args = list(k=469,se=25,lmax=22,lmin=12,m=8),size=1,col="green")+
  geom_ribbon(aes(ymin = Nkal, ymax = Nkau), fill = "grey", alpha = .6)+
  stat_function(fun = function(x) 469,size=1,col="orange")+
  geom_point()

ggplot2:: ggplot(Maxillosus2,aes(Length,mk))+
  geom_smooth(method = "lm",se=F)+
  stat_function(fun = function(x) -10.255145*x+616.0064,size=1,color="red")+
  xlab('Length')+
  ylab('True k')+
  ggtitle('Creophilus maxillosus L. (Staphylinidae)')+
  theme_minimal()+
  scale_x_continuous(breaks = 14:25)+
  stat_function(fun = Ka,args = list(k=405.156,se=14.63,lmax=25,lmin=15,m=7),size=1,col="green")+
  geom_ribbon(aes(ymin = Mkal, ymax = Mkau), fill = "grey", alpha = .6)+
  stat_function(fun = function(x) 405.156,size=1,col="orange")+
  geom_point()

ggplot2:: ggplot(Necrodes2,aes(Length,mk))+
  geom_smooth(method = "lm",se=F)+
  stat_function(fun = function(x) -17.35671*x+761.1040,size=1,color="red")+
  xlab('Length')+
  ylab('True k')+
  ggtitle('Necrodes littoralis L. (Silphidae)')+
  theme_minimal()+
  scale_x_continuous(breaks = 12:23)+
  stat_function(fun = Ka,args = list(k=469,se=25,lmax=22,lmin=12,m=8),size=1,col="green")+
  geom_ribbon(aes(ymin = Nkal2, ymax = Nkau2), fill = "grey", alpha = .6)+
  stat_function(fun = function(x) 469,size=1,col="orange")+
  geom_point()

ggplot2:: ggplot(Maxillosus2,aes(Length,mk))+
  geom_smooth(method = "lm",se=F)+
  stat_function(fun = function(x) -10.255145*x+616.0064,size=1,color="red")+
  xlab('Length')+
  ylab('True k')+
  ggtitle('Creophilus maxillosus L. (Staphylinidae)')+
  theme_minimal()+
  scale_x_continuous(breaks = 14:25)+
  stat_function(fun = Ka,args = list(k=405.156,se=14.63,lmax=25,lmin=15,m=7),size=1,col="green")+
  geom_ribbon(aes(ymin = Mkal2, ymax = Mkau2), fill = "grey", alpha = .6)+
  stat_function(fun = function(x) 405.156,size=1,col="orange")+
  geom_point()

ggplot(Necrodes, aes(x=Length,group=Temperature))+
  geom_histogram(aes(y=..density..))+
  facet_wrap(~Temperature)+
  geom_density(alpha=.2, fill="#FF6666") 

#14:20, 22, 26, 30

ggNecrodes <- subset(Necrodes,Necrodes$Temperature==30)

ggplot(ggNecrodes, aes(x=Length,group=Temperature))+
  stat_bin(bins=10,aes(y=..density..))+
  geom_density(alpha=.2, fill="#FF6666")+
  stat_function(col="green",size=2,fun = function(x) dnorm(x, mean=mean(ggNecrodes$Length),sd=sd(ggNecrodes$Length)))+
  facet_wrap(~Temperature)

#15, 17.5, 20, 22.5, 25, 27.5, 30

ggMaxillosus <- subset(Maxillosus,Maxillosus$Temperature==30)

ggplot(ggMaxillosus, aes(x=k,group=Temperature))+
  stat_bin(bins=5,aes(y=..density..))+
  geom_density(alpha=.2, fill="#FF6666")+
  stat_function(col="green",size=2,fun = function(x) dnorm(x, mean=mean(ggMaxillosus$k),sd=sd(ggMaxillosus$k)))+
  facet_wrap(~Temperature)

#Necrodes 14

tn <- 8.49

testNecrodes14 <- subset(Necrodes,Necrodes$Temperature==14)
set.seed(2317)
NtestS14 <- sample(1:54, 10)
testNecrodes14$Length[NtestS14]
(n14 <- min(testNecrodes14$Length[NtestS14]))

Ka(x=n14,k=469,se=25,lmax=22,lmin=12,m=8)

Ka(x=n14,k=469,se=25,lmax=22,lmin=12,m=8)/(14-tn)
469/(14-tn)
View(testNecrodes14[NtestS14,])

UCI(x=n14,k=469,se=25,lmax=22,lmin=12,m=8)/(14-tn)
DCI(x=n14,k=469,se=25,lmax=22,lmin=12,m=8)/(14-tn)

min(testNecrodes14$k)
max(testNecrodes14$k)

444/(14-tn)
494/(14-tn)

min(testNecrodes14$Time)
max(testNecrodes14$Time)

#Necrodes 19

testNecrodes19 <- subset(Necrodes,Necrodes$Temperature==19)
set.seed(2323)
NtestS19 <- sample(1:100, 10)
testNecrodes19$Length[NtestS19]
(n19 <- min(testNecrodes19$Length[NtestS19]))

Ka(x=n19,k=469,se=25,lmax=22,lmin=12,m=8)/(19-tn)
UCI(x=n19,k=469,se=25,lmax=22,lmin=12,m=8)/(19-tn)
DCI(x=n19,k=469,se=25,lmax=22,lmin=12,m=8)/(19-tn)

min(testNecrodes19$k)
max(testNecrodes19$k)

min(testNecrodes19$Time)
max(testNecrodes19$Time)

View(testNecrodes19[NtestS19,])

469/(19-tn)

#Necrodes 26

testNecrodes26 <- subset(Necrodes,Necrodes$Temperature==26)
set.seed(2321)
NtestS26 <- sample(1:100, 10)
testNecrodes26$Length[NtestS26]
(n26 <- min(testNecrodes26$Length[NtestS26]))

Ka(x=n26,k=469,se=25,lmax=22,lmin=12,m=8)
UCI(x=n26,k=469,se=25,lmax=22,lmin=12,m=8)/(26-8.49)
DCI(x=n26,k=469,se=25,lmax=22,lmin=12,m=8)/(26-8.49)

View(testNecrodes26[NtestS26,])

min(testNecrodes26$k)
max(testNecrodes26$k)

min(testNecrodes26$Time)
max(testNecrodes26$Time)

#Necrodes 30

testNecrodes30 <- subset(Necrodes,Necrodes$Temperature==30)
set.seed(2322)
NtestS30 <- sample(1:100, 10)
testNecrodes30$Length[NtestS30]
(n30 <- min(testNecrodes30$Length[NtestS30]))

Ka(x=n30,k=469,se=25,lmax=22,lmin=12,m=8)/(30-tn)
UCI(x=n30,k=469,se=25,lmax=22,lmin=12,m=8)/(30-tn)
DCI(x=n30,k=469,se=25,lmax=22,lmin=12,m=8)/(30-tn)

View(testNecrodes30[NtestS30,])

min(testNecrodes30$k)
max(testNecrodes30$k)

min(testNecrodes30$Time)
max(testNecrodes30$Time)


#Maxillosus 15

tm <- 11.58

testMaxillosus15 <- subset(Maxillosus,Maxillosus$Temperature==15)
set.seed(2324)
MtestS15 <- sample(1:14, 10)
testMaxillosus15$Length[MtestS15]
(m15 <- min(testMaxillosus15$Length[MtestS15]))

Ka(x=m15,k=405.156,se=14.63,lmax=25,lmin=15,m=7)/(15-tm)
UCI(x=m15,k=405.156,se=14.63,lmax=25,lmin=15,m=7)/(15-tm)
DCI(x=m15,k=405.156,se=14.63,lmax=25,lmin=15,m=7)/(15-tm)

View(testMaxillosus15)

min(testMaxillosus15$k)
max(testMaxillosus15$k)

405.156/(15-tm)
(405.156-14.63)/(15-tm)
(405.156+14.63)/(15-tm)

min(testMaxillosus15$Time)
max(testMaxillosus15$Time)

#Maxillosus 20

testMaxillosus20 <- subset(Maxillosus,Maxillosus$Temperature==20)
set.seed(2326)
MtestS20 <- sample(1:32, 10)
testMaxillosus20$Length[MtestS20]
(m20 <- min(testMaxillosus20$Length[MtestS20]))

Ka(x=m20,k=405.156,se=14.63,lmax=25,lmin=15,m=7)/(20-tm)
UCI(x=m20,k=405.156,se=14.63,lmax=25,lmin=15,m=7)/(20-tm)
DCI(x=m20,k=405.156,se=14.63,lmax=25,lmin=15,m=7)/(20-tm)

View(testMaxillosus20[MtestS20,])

min(testMaxillosus20$k)
max(testMaxillosus20$k)

405.156/(20-tm)
(405.156-14.63)/(20-tm)
(405.156+14.63)/(20-tm)

min(testMaxillosus20$Time)
max(testMaxillosus20$Time)

#Maxillosus 25

testMaxillosus25 <- subset(Maxillosus,Maxillosus$Temperature==25)
set.seed(2329)
MtestS25 <- sample(1:36, 10)
testMaxillosus25$Length[MtestS25]
(m25 <- min(testMaxillosus25$Length[MtestS25]))

Ka(x=m25,k=405.156,se=14.63,lmax=25,lmin=15,m=7)/(25-tm)
UCI(x=m25,k=405.156,se=14.63,lmax=25,lmin=15,m=7)/(25-tm)
DCI(x=m25,k=405.156,se=14.63,lmax=25,lmin=15,m=7)/(25-tm)

View(testMaxillosus25[MtestS25,])

min(testMaxillosus25$k)
max(testMaxillosus25$k)

405.156/(25-tm)
(405.156-14.63)/(25-tm)
(405.156+14.63)/(25-tm)

min(testMaxillosus25$Time)
max(testMaxillosus25$Time)


#Maxillosus 30

testMaxillosus30 <- subset(Maxillosus,Maxillosus$Temperature==30)
set.seed(2330)
MtestS30 <- sample(1:13, 10)
testMaxillosus30$Length[MtestS30]
(m30 <- min(testMaxillosus30$Length[MtestS30]))

Ka(x=m30,k=405.156,se=14.63,lmax=25,lmin=15,m=7)/(30-tm)
UCI(x=m30,k=405.156,se=14.63,lmax=25,lmin=15,m=7)/(30-tm)
DCI(x=m30,k=405.156,se=14.63,lmax=25,lmin=15,m=7)/(30-tm)

View(testMaxillosus30[MtestS30,])

min(testMaxillosus30$k)
max(testMaxillosus30$k)

405.156/(30-tm)
(405.156-14.63)/(30-tm)
(405.156+14.63)/(30-tm)

min(testMaxillosus30$Time)
max(testMaxillosus30$Time)

#plot_comparison<- read_excel("tabela do obrazka.xlsx")

plot_comparison$method <- factor(plot_comparison$method)
plot_comparison$method <- factor(plot_comparison$method,levels(plot_comparison$method)[c(2,1,3)])


ggplot(subset(plot_comparison,plot_comparison$gatunek=='Necrodes'),aes(x=method,y=dd,color=method))+
  geom_line()+
  geom_line(aes(group=type))+
  geom_point()+
  facet_wrap(~temperature, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle('Necrodes littoralis L. (Silphidae)')+
  coord_flip()+
  xlab('')+
  ylab('day-degrees')

ggplot(subset(plot_comparison,plot_comparison$gatunek=='Maxillosus'),aes(x=method,y=dd,color=method))+
  geom_line()+
  geom_line(aes(group=type))+
  geom_point()+
  facet_wrap(~temperature, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle('Creophilus maxillosus L. (Staphylinidae)')+
  coord_flip()+
  xlab('')+
  ylab('day-degrees')

ggplot(subset(plot_comparison,plot_comparison$gatunek=='Necrodes'),aes(x=method,y=time,color=method))+
  geom_line()+
  geom_line(aes(group=type))+
  geom_point()+
  facet_wrap(~temperature, scales = "free")+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle('Necrodes littoralis L. (Silphidae)')+
  coord_flip()+
  xlab('')+
  ylab('time')

ggplot(subset(plot_comparison,plot_comparison$gatunek=='Maxillosus'),aes(x=method,y=time,color=method))+
  geom_line()+
  geom_line(aes(group=type))+
  geom_point()+
  facet_wrap(~temperature, scales = "free",labeller = "label_both")+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle('Creophilus maxillosus L. (Staphylinidae)')+
  coord_flip()+
  xlab('')+
  ylab('time')

#prawdopodobieństwo

ss <- sd(testNecrodes14$Length[NtestS14])
min(testNecrodes14$Length[NtestS14])
max(testNecrodes14$Length[NtestS14])

lb <- Ka(x=n14,k=469,se=25,lmax=22,lmin=12,m=8)

sdl <- (25-15)/4
mll <- (25+15)/2

cov <- seq(from = -sdl*25*sqrt(8), to = 0, by = 0.1)
licznik <- 0

for(i in 1:1768)
{
  licznik <- licznik + pmvnorm(lower=c(-Inf,14),
                               upper=c(lb,17),
                               mean=c(469,mll),
                               sigma=matrix(c(8*25^2,cov[i],cov[i],sdl^2),2))[[1]]
}
licznik <- licznik/1768
mianownik <- pnorm(17,mll,sdl)-pnorm(14,mll,sdl)
(pstwo <- licznik/mianownik)

