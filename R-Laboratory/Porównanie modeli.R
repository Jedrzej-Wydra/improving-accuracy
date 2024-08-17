library(agricolae)
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(MLmetrics)


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

smatr:: sma(mk~Length,Necrodes2)
broom:: tidy(lm(mk~Length,Necrodes2))
smatr:: sma(mk~Length,Maxillosus2)
broom:: tidy(lm(mk~Length,Maxillosus2))
summary(lm(mk~Length,Necrodes2))
summary(lm(mk~Length,Maxillosus2))

Ka <- function(x,k,se,lmax,lmin,m)
{
  k-(se*sqrt(3*m)/(lmax-lmin))*(x-0.5*(lmax+lmin))
}

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
  geom_ribbon(aes(ymin = Nkal, ymax = Nkau), fill = "grey", alpha = .6)+
  geom_smooth(method = "lm",se=F,col="black",linetype="dashed")+
  stat_function(fun = function(x) -17.35671*x+761.1040,size=1,linetype="dotted")+
  xlab('Length')+
  ylab('True k')+
  ggtitle('Necrodes littoralis L. (Silphidae)')+
  theme_bw()+
  scale_x_continuous(breaks = 12:23)+
  stat_function(fun = Ka,args = list(k=469,se=25,lmax=22,lmin=12,m=8),size=1)+
  stat_function(fun = function(x) 469,size=1,linetype="twodash")+
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
  geom_point()+
  geom_point(aes(x=median(Length),y=median(mk)),color="red")

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

#Necrodes

MKadjusted <- MLmetrics:: MAPE(Ka(Necrodes2$Length,k=469,se=25,lmax=22,lmin=12,m=8),Necrodes2$mk)
MKols <- MLmetrics:: MAPE(-15.685*Necrodes2$Length+732.365,Necrodes2$mk)
MKrma <- MLmetrics:: MAPE(-17.35671*Necrodes2$Length+761.1040,Necrodes2$mk)
MKgeneral <- MLmetrics:: MAPE(rep(469,length(Necrodes2$mk)),Necrodes2$mk)

SMKadjusted <- sd(abs(Ka(Necrodes2$Length,k=469,se=25,lmax=22,lmin=12,m=8)-Necrodes2$mk)/Necrodes2$mk)/sqrt(18)
SMKols <- sd(abs(-15.685*Necrodes2$Length+732.365-Necrodes2$mk)/Necrodes2$mk)/sqrt(18)
SMKrma <- sd(abs(-17.35671*Necrodes2$Length+761.1040-Necrodes2$mk)/Necrodes2$mk)/sqrt(18)
SMKgeneral <- sd(abs(rep(469,length(Necrodes2$mk))-Necrodes2$mk)/Necrodes2$mk)/sqrt(18)

Residuals_Necrodes <- data.frame(Residuals=c(MKadjusted,MKols,MKrma,MKgeneral),
                                 lower=c(MKadjusted,MKols,MKrma,MKgeneral)-c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
                                 upper=c(MKadjusted,MKols,MKrma,MKgeneral)+c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
                                 method=c(rep('Ka',length(MKadjusted)),rep('OLS',length(MKols)),rep('RMA',length(MKrma)),rep('General model',length(MKgeneral))))

boxtoplot <- gather(Residuals_Necrodes,type,error,Residuals:upper,factor_key=TRUE)

ggplot(boxtoplot,aes(method,error,color=method))+geom_line()+geom_point()+theme_bw()

kar <- abs(Ka(Necrodes2$Length,k=469,se=25,lmax=22,lmin=12,m=8)-Necrodes2$mk)/Necrodes2$mk
kolsr <- abs(-15.685*Necrodes2$Length+732.365-Necrodes2$mk)/Necrodes2$mk
krmar <- abs(-17.35671*Necrodes2$Length+761.1040-Necrodes2$mk)/Necrodes2$mk
kgener <- abs(rep(469,length(Necrodes2$mk))-Necrodes2$mk)/Necrodes2$mk

anovatest <- data.frame(errors=c(kar,kolsr,krmar,kgener),
                        method=c(rep("Ka",length(kar)),rep("OLS",length(kolsr)),rep("RMA",length(krmar)),rep("General model",length(kgener))))
summary(aov(errors~method,anovatest))

pairwise.t.test(anovatest$errors,anovatest$method)

(HSD.test(aov(errors~method,anovatest), "method")$groups)

Necrodes2 %>% mutate(ka=Ka(Length,k=469,se=25,lmax=22,lmin=12,m=8)) -> Necrodes3

Necrodes3 %>% mutate(diff=abs(mk-ka)/mk) -> Necrodes3

Necrodes3 %>% mutate(diff2=abs(mk-469)/mk) -> Necrodes3

Necrodes3 %>% mutate(diff3=diff2-diff) -> Necrodes3

Necrodes3 %>% mutate(PER=abs(ka-mk)/abs(469-mk)) -> Necrodes3

ggplot(Necrodes3,aes(Length,PER))+geom_line()

ggplot(Necrodes3,aes(Length,diff3,label=Length))+geom_text()+geom_abline(intercept = 0.01,slope=0)+geom_abline(intercept = 0.02,slope=0,col="red")

#Maxillosus

MKadjusted <- MLmetrics:: MAPE(Ka(Maxillosus2$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7),Maxillosus2$mk)
MKols <- MLmetrics:: MAPE(-7.913*Maxillosus2$Length+570.336,Maxillosus2$mk)
MKrma <- MLmetrics:: MAPE(-10.255145*Maxillosus2$Length+616.0064,Maxillosus2$mk)
MKgeneral <- MLmetrics:: MAPE(rep(405.156,length(Maxillosus2$mk)),Maxillosus2$mk)

SMKadjusted <- sd(abs(Ka(Maxillosus2$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)-Maxillosus2$mk)/Maxillosus2$mk)/sqrt(19)
SMKols <- sd(abs(-7.913*Maxillosus2$Length+570.336-Maxillosus2$mk)/Maxillosus2$mk)/sqrt(19)
SMKrma <- sd(abs(-10.255145*Maxillosus2$Length+616.0064-Maxillosus2$mk)/Maxillosus2$mk)/sqrt(19)
SMKgeneral <- sd(abs(rep(405.156,length(Maxillosus2$mk))-Maxillosus2$mk)/Maxillosus2$mk)/sqrt(19)

Residuals_Maxillosus <- data.frame(Residuals=c(MKadjusted,MKols,MKrma,MKgeneral),
                                   lower=c(MKadjusted,MKols,MKrma,MKgeneral)-c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
                                   upper=c(MKadjusted,MKols,MKrma,MKgeneral)+c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
                                   method=c(rep('Ka',length(MKadjusted)),rep('OLS',length(MKols)),rep('RMA',length(MKrma)),rep('General model',length(MKgeneral))))

boxtoplot <- gather(Residuals_Maxillosus,type,error,Residuals:upper,factor_key=TRUE)

ggplot(boxtoplot,aes(method,error,color=method))+geom_line()+geom_point()+theme_bw()

kar <- abs(Ka(Maxillosus2$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)-Maxillosus2$mk)/Maxillosus2$mk
kolsr <- abs(-7.913*Maxillosus2$Length+570.336-Maxillosus2$mk)/Maxillosus2$mk
krmar <- abs(-10.255145*Maxillosus2$Length+616.0064-Maxillosus2$mk)/Maxillosus2$mk
kgener <- abs(rep(405.156,length(Maxillosus2$mk))-Maxillosus2$mk)/Maxillosus2$mk

anovatest <- data.frame(errors=c(kar,kolsr,krmar,kgener),
                        method=c(rep("Ka",length(kar)),rep("OLS",length(kolsr)),rep("RMA",length(krmar)),rep("General model",length(kgener))))
summary(aov(errors~method,anovatest))

pairwise.t.test(anovatest$errors,anovatest$method)

(HSD.test(aov(errors~method,anovatest), "method")$groups)

Maxillosus2 %>% mutate(ka=Ka(Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)) -> Maxillosus3

Maxillosus3 %>% mutate(diff=abs(mk-ka)/mk) -> Maxillosus3

Maxillosus3 %>% mutate(diff2=abs(mk-405.156)/mk) -> Maxillosus3

Maxillosus3 %>% mutate(diff3=diff2-diff) -> Maxillosus3

Maxillosus %>% mutate(ka=Ka(Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)) -> MaxillosusE

MaxillosusE %>% mutate(diff=abs(k-ka)/k) -> MaxillosusE

MaxillosusE %>% mutate(diff2=abs(k-405.156)/k) -> MaxillosusE

MaxillosusE %>% mutate(diff3=diff2-diff) -> MaxillosusE


ggplot(Maxillosus3,aes(Length,diff3,label=Length))+geom_text()+geom_abline(intercept = 0.01,slope=0)+geom_abline(intercept = 0.02,slope=0,col="red")

#Maxillosus restricted

dane <- subset(Maxillosus2,Maxillosus2$Length>21|Maxillosus2$Length<19&Maxillosus2$Length!=16.5&Maxillosus2$Length!=22.5)

MKadjusted <- MLmetrics:: MAPE(Ka(dane$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7),dane$mk)
MKols <- MLmetrics:: MAPE(-7.913*dane$Length+570.336,dane$mk)
MKrma <- MLmetrics:: MAPE(-10.255145*dane$Length+616.0064,dane$mk)
MKgeneral <- MLmetrics:: MAPE(rep(405.156,length(dane$mk)),dane$mk)

SMKadjusted <- sd(abs(Ka(dane$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)-dane$mk)/dane$mk)/sqrt(19)
SMKols <- sd(abs(-7.913*dane$Length+570.336-dane$mk)/dane$mk)/sqrt(19)
SMKrma <- sd(abs(-10.255145*dane$Length+616.0064-dane$mk)/dane$mk)/sqrt(19)
SMKgeneral <- sd(abs(rep(405.156,length(dane$mk))-dane$mk)/dane$mk)/sqrt(19)

Residuals_Maxillosus <- data.frame(Residuals=c(MKadjusted,MKols,MKrma,MKgeneral),
                                   lower=c(MKadjusted,MKols,MKrma,MKgeneral)-c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
                                   upper=c(MKadjusted,MKols,MKrma,MKgeneral)+c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
                                   method=c(rep('Ka',length(MKadjusted)),rep('OLS',length(MKols)),rep('RMA',length(MKrma)),rep('General model',length(MKgeneral))))

boxtoplot <- gather(Residuals_Maxillosus,type,error,Residuals:upper,factor_key=TRUE)

boxtoplot %>% mutate(error=error/0.06240652) -> boxtoplot

boxtoplot <- cbind(boxtoplot, label=c("b","b","b","a",rep("",8)))

#ggplot(boxtoplot,aes(method,error,label=label))+
#  geom_line()+
#  geom_point()+
#  geom_text(nudge_x = 0.06)+
#  ylab('relative error difference')+
#  theme_bw()+
#  theme(legend.position = "none")+
#  ggtitle('Creophilus maxillosus L. (Staphylinidae)')

ggplot(boxtoplot,aes(method,error,label=label,color=method))+
    geom_line()+
    geom_point()+
    geom_text(nudge_x = 0.06,col="black")+
    ylab('relative error difference')+
    theme_minimal()+
    theme(legend.position = "none")+
    ggtitle('Creophilus maxillosus L. (Staphylinidae)')

kar <- abs(Ka(dane$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)-dane$mk)/dane$mk
kolsr <- abs(-7.913*dane$Length+570.336-dane$mk)/dane$mk
krmar <- abs(-10.255145*dane$Length+616.0064-dane$mk)/dane$mk
kgener <- abs(rep(405.156,length(dane$mk))-dane$mk)/dane$mk

anovatest <- data.frame(errors=c(kar,kolsr,krmar,kgener),
                        method=c(rep("Ka",length(kar)),rep("OLS",length(kolsr)),rep("RMA",length(krmar)),rep("General model",length(kgener))))
summary(aov(errors~method,anovatest))

pairwise.t.test(anovatest$errors,anovatest$method)

(HSD.test(aov(errors~method,anovatest), "method")$groups)

#Necrodes restricted

dane <- subset(Necrodes2,Necrodes2$Length<16.5|Necrodes2$Length>19.5&Necrodes2$Length!=12)

MKadjusted <- MLmetrics:: MAPE(Ka(dane$Length,k=469,se=25,lmax=22,lmin=12,m=8),dane$mk)
MKols <- MLmetrics:: MAPE(-15.685*dane$Length+732.365,dane$mk)
MKrma <- MLmetrics:: MAPE(-17.35671*dane$Length+761.1040,dane$mk)
MKgeneral <- MLmetrics:: MAPE(rep(469,length(dane$mk)),dane$mk)

SMKadjusted <- sd(abs(Ka(dane$Length,k=469,se=25,lmax=22,lmin=12,m=8)-dane$mk)/dane$mk)/sqrt(18)
SMKols <- sd(abs(-15.685*dane$Length+732.365-dane$mk)/dane$mk)/sqrt(18)
SMKrma <- sd(abs(-17.35671*dane$Length+761.1040-dane$mk)/dane$mk)/sqrt(18)
SMKgeneral <- sd(abs(rep(469,length(dane$mk))-dane$mk)/dane$mk)/sqrt(18)

Residuals_Maxillosus <- data.frame(Residuals=c(MKadjusted,MKols,MKrma,MKgeneral),
                                   lower=c(MKadjusted,MKols,MKrma,MKgeneral)-c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
                                   upper=c(MKadjusted,MKols,MKrma,MKgeneral)+c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
                                   method=c(rep('Ka',length(MKadjusted)),rep('OLS',length(MKols)),rep('RMA',length(MKrma)),rep('General model',length(MKgeneral))))

boxtoplot <- gather(Residuals_Maxillosus,type,error,Residuals:upper,factor_key=TRUE)

boxtoplot %>% mutate(error=error/0.12536880) -> boxtoplot

boxtoplot <- cbind(boxtoplot, label=c("b","b","b","a",rep("",8)))

#ggplot(boxtoplot,aes(method,error,label=label))+
#  geom_line()+
#  geom_point()+
#  geom_text(nudge_x=0.06)+
#  ylab('relative error difference')+
#  theme_bw()+
#  theme(legend.position = "none")+
#  ggtitle('Necrodes littoralis L. (Silphidae)')

ggplot(boxtoplot,aes(method,error,label=label,color=method))+
  geom_line()+
  geom_point()+
  geom_text(nudge_x = 0.06,col="black")+
  ylab('relative error difference')+
  theme_minimal()+
  theme(legend.position = "none")+
  ggtitle('Necrodes littoralis L. (Silphidae)')

kar <- abs(Ka(dane$Length,k=469,se=25,lmax=22,lmin=12,m=8)-dane$mk)/dane$mk
kolsr <- abs(-15.685*dane$Length+732.365-dane$mk)/dane$mk
krmar <- abs(-17.35671*dane$Length+761.1040-dane$mk)/dane$mk
kgener <- abs(rep(469,length(dane$mk))-dane$mk)/dane$mk

anovatest <- data.frame(errors=c(kar,kolsr,krmar,kgener),
                        method=c(rep("Ka",length(kar)),rep("OLS",length(kolsr)),rep("RMA",length(krmar)),rep("General model",length(kgener))))
summary(aov(errors~method,anovatest))

pairwise.t.test(anovatest$errors,anovatest$method)

(HSD.test(aov(errors~method,anovatest), "method")$groups)

#Necrodes restricted MSE

dane <- subset(Necrodes2,Necrodes2$Length<16.5|Necrodes2$Length>19.5&Necrodes2$Length!=12)

MKadjusted <- MLmetrics:: MSE(Ka(dane$Length,k=469,se=25,lmax=22,lmin=12,m=8),dane$mk)
MKols <- MLmetrics:: MSE(-15.685*dane$Length+732.365,dane$mk)
MKrma <- MLmetrics:: MSE(-17.35671*dane$Length+761.1040,dane$mk)
MKgeneral <- MLmetrics:: MSE(rep(469,length(dane$mk)),dane$mk)

SMKadjusted <- sd((Ka(dane$Length,k=469,se=25,lmax=22,lmin=12,m=8)-dane$mk)^2)/sqrt(18)
SMKols <- sd((-15.685*dane$Length+732.365-dane$mk)^2)/sqrt(18)
SMKrma <- sd((-17.35671*dane$Length+761.1040-dane$mk)^2)/sqrt(18)
SMKgeneral <- sd((rep(469,length(dane$mk))-dane$mk)^2)/sqrt(18)

#Residuals_Maxillosus <- data.frame(Residuals=c(MKadjusted,MKols,MKrma,MKgeneral),
#                                   lower=c(MKadjusted,MKols,MKrma,MKgeneral)-c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
#                                   upper=c(MKadjusted,MKols,MKrma,MKgeneral)+c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
#                                   method=c(rep('Kc',length(MKadjusted)),rep('OLS',length(MKols)),rep('RMA',length(MKrma)),rep('General model',length(MKgeneral))))

Residuals_Maxillosus <- data.frame(Residuals=c(MKadjusted,MKrma,MKgeneral),
                                   lower=c(MKadjusted,MKrma,MKgeneral)-c(SMKadjusted,SMKrma,SMKgeneral),
                                   upper=c(MKadjusted,MKrma,MKgeneral)+c(SMKadjusted,SMKrma,SMKgeneral),
                                   method=c(rep('Kc',length(MKadjusted)),rep('RMA',length(MKrma)),rep('constant k',length(MKgeneral))))

boxtoplot <- gather(Residuals_Maxillosus,type,error,Residuals:upper,factor_key=TRUE)

boxtoplot %>% mutate(error=100*error/3744.7269) -> boxtoplot

boxtoplot <- cbind(boxtoplot, label=c("b","b","a",rep("",6)))

#ggplot(boxtoplot,aes(method,error,label=label))+
#  geom_line()+
#  geom_point()+
#  geom_text(nudge_x=0.06)+
#  ylab('relative error difference')+
#  theme_bw()+
#  theme(legend.position = "none")+
#  ggtitle('Necrodes littoralis L. (Silphidae)')

plot1 <- ggplot(boxtoplot,aes(method,error,label=label,color=method))+
  geom_line(size=2)+
  geom_point(size=5)+
  geom_text(nudge_x = 0.1,col="black",size=6)+
  ylab('Relative error difference [%]')+
  xlab('Method')+
  theme_bw()+
  theme(legend.position = "none", text = element_text(size=20))+
  ggtitle(expression(paste(italic('Necrodes littoralis'))))+
  scale_color_manual("legend", values = c("constant k" = "#E58700", "RMA" = "#F8766D", "OLS" = "#00B0F6", "Kc"="#6BB100"))+
  scale_x_discrete(labels=c('RMA'='RMA','constant k'='constant k', 'Kc'=parse(text=TeX("$K_c$ model"))))

  
kar <- (Ka(dane$Length,k=469,se=25,lmax=22,lmin=12,m=8)-dane$mk)^2
kolsr <- (-15.685*dane$Length+732.365-dane$mk)^2
krmar <- (-17.35671*dane$Length+761.1040-dane$mk)^2
kgener <- (rep(469,length(dane$mk))-dane$mk)^2

anovatest <- data.frame(errors=c(kar,kolsr,krmar,kgener),
                        method=c(rep("Ka",length(kar)),rep("OLS",length(kolsr)),rep("RMA",length(krmar)),rep("General model",length(kgener))))
summary(aov(errors~method,anovatest))

pairwise.t.test(anovatest$errors,anovatest$method)

(LSD.test(aov(errors~method,anovatest), "method", p.adj="BH")$groups)

#Maxillosus restricted MSE

dane <- subset(Maxillosus2,Maxillosus2$Length>21|Maxillosus2$Length<19&Maxillosus2$Length!=16.5&Maxillosus2$Length!=22.5)



MKadjusted <- MLmetrics:: MSE(Ka(dane$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7),dane$mk)
MKols <- MLmetrics:: MSE(-7.913*dane$Length+570.336,dane$mk)
MKrma <- MLmetrics:: MSE(-10.255145*dane$Length+616.0064,dane$mk)
MKgeneral <- MLmetrics:: MSE(rep(405.156,length(dane$mk)),dane$mk)

SMKadjusted <- sd((Ka(dane$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)-dane$mk)^2)/sqrt(19)
SMKols <- sd((-7.913*dane$Length+570.336-dane$mk)^2)/sqrt(19)
SMKrma <- sd((-10.255145*dane$Length+616.0064-dane$mk)^2)/sqrt(19)
SMKgeneral <- sd((rep(405.156,length(dane$mk))-dane$mk)^2)/sqrt(19)

#Residuals_Maxillosus <- data.frame(Residuals=c(MKadjusted,MKols,MKrma,MKgeneral),
#                                   lower=c(MKadjusted,MKols,MKrma,MKgeneral)-c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
#                                   upper=c(MKadjusted,MKols,MKrma,MKgeneral)+c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
#                                   method=c(rep('Kc',length(MKadjusted)),rep('OLS',length(MKols)),rep('RMA',length(MKrma)),rep('General model',length(MKgeneral))))

Residuals_Maxillosus <- data.frame(Residuals=c(MKadjusted,MKrma,MKgeneral),
                                   lower=c(MKadjusted,MKrma,MKgeneral)-c(SMKadjusted,SMKrma,SMKgeneral),
                                   upper=c(MKadjusted,MKrma,MKgeneral)+c(SMKadjusted,SMKrma,SMKgeneral),
                                   method=c(rep('Kc',length(MKadjusted)),rep('RMA',length(MKrma)),rep('constant k',length(MKgeneral))))

boxtoplot <- gather(Residuals_Maxillosus,type,error,Residuals:upper,factor_key=TRUE)

boxtoplot %>% mutate(error=100*error/1052.9536) -> boxtoplot

boxtoplot <- cbind(boxtoplot, label=c("b","b","a",rep("",6)))

#ggplot(boxtoplot,aes(method,error,label=label))+
#  geom_line()+
#  geom_point()+
#  geom_text(nudge_x = 0.06)+
#  ylab('relative error difference')+
#  theme_bw()+
#  theme(legend.position = "none")+
#  ggtitle('Creophilus maxillosus L. (Staphylinidae)')

plot2 <- ggplot(boxtoplot,aes(method,error,label=label,color=method))+
  geom_line(size=2)+
  geom_point(size=5)+
  geom_text(nudge_x = 0.1,col="black",size=6)+
  ylab('')+
  xlab('Method')+
  theme_bw()+
  theme(legend.position = "none")+
  ggtitle(expression(paste(italic('Creophilus maxillosus'))))+
  scale_color_manual("legend", values = c("constant k" = "#E58700", "RMA" = "#F8766D", "OLS" = "#00B0F6", "Kc"="#6BB100"))+
  theme(text = element_text(size=20))+
  scale_x_discrete(labels=c('RMA'='RMA','constant k'='constant k', "Kc"=parse(text=TeX("$K_c$ model"))))

grid.arrange(plot1,plot2,ncol=2)

kar <- (Ka(dane$Length,k=405.156,se=14.63,lmax=24,lmin=15,m=7)-dane$mk)^2
kolsr <- (-7.913*dane$Length+570.336-dane$mk)^2
krmar <- (-10.255145*dane$Length+616.0064-dane$mk)^2
kgener <- (rep(405.156,length(dane$mk))-dane$mk)^2

anovatest <- data.frame(errors=c(kar,kolsr,krmar,kgener),
                        method=c(rep("Ka",length(kar)),rep("OLS",length(kolsr)),rep("RMA",length(krmar)),rep("General model",length(kgener))))
summary(aov(errors~method,anovatest))

pairwise.t.test(anovatest$errors,anovatest$method)

(LSD.test(aov(errors~method,anovatest), "method", p.adj="BH")$groups)

#Necrodes MSE wykluczone

dane <- subset(Necrodes2,Necrodes2$Length>16.5&Necrodes2$Length<19.5&Necrodes2$Length!=12)

#dane <- subset(Necrodes2,Necrodes2$Length!=12)

MKadjusted <- MLmetrics:: MSE(Ka(dane$Length,k=469,se=25,lmax=22,lmin=12,m=8),dane$mk)
MKols <- MLmetrics:: MSE(-15.685*dane$Length+732.365,dane$mk)
MKrma <- MLmetrics:: MSE(-17.35671*dane$Length+761.1040,dane$mk)
MKgeneral <- MLmetrics:: MSE(rep(469,length(dane$mk)),dane$mk)

SMKadjusted <- sd((Ka(dane$Length,k=469,se=25,lmax=22,lmin=12,m=8)-dane$mk)^2)/sqrt(18)
SMKols <- sd((-15.685*dane$Length+732.365-dane$mk)^2)/sqrt(18)
SMKrma <- sd((-17.35671*dane$Length+761.1040-dane$mk)^2)/sqrt(18)
SMKgeneral <- sd((rep(469,length(dane$mk))-dane$mk)^2)/sqrt(18)

Residuals_Maxillosus <- data.frame(Residuals=c(MKadjusted,MKols,MKrma,MKgeneral),
                                   lower=c(MKadjusted,MKols,MKrma,MKgeneral)-c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
                                   upper=c(MKadjusted,MKols,MKrma,MKgeneral)+c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
                                   method=c(rep('Ka',length(MKadjusted)),rep('OLS',length(MKols)),rep('RMA',length(MKrma)),rep('General model',length(MKgeneral))))

boxtoplot <- gather(Residuals_Maxillosus,type,error,Residuals:upper,factor_key=TRUE)

boxtoplot %>% mutate(error=error/178.96412) -> boxtoplot

boxtoplot <- cbind(boxtoplot, label=c("b","b","b","a",rep("",8)))

#ggplot(boxtoplot,aes(method,error,label=label))+
#  geom_line()+
#  geom_point()+
#  geom_text(nudge_x=0.06)+
#  ylab('relative error difference')+
#  theme_bw()+
#  theme(legend.position = "none")+
#  ggtitle('Necrodes littoralis L. (Silphidae)')

ggplot(boxtoplot,aes(method,error,label=label,color=method))+
  geom_line()+
  geom_point()+
  geom_text(nudge_x = 0.06,col="black")+
  ylab('relative error difference')+
  theme_minimal()+
  theme(legend.position = "none")+
  ggtitle('Necrodes littoralis L. (Silphidae)')

kar <- (Ka(dane$Length,k=469,se=25,lmax=22,lmin=12,m=8)-dane$mk)^2
kolsr <- (-15.685*dane$Length+732.365-dane$mk)^2
krmar <- (-17.35671*dane$Length+761.1040-dane$mk)^2
kgener <- (rep(469,length(dane$mk))-dane$mk)^2

anovatest <- data.frame(errors=c(kar,kolsr,krmar,kgener),
                        method=c(rep("Ka",length(kar)),rep("OLS",length(kolsr)),rep("RMA",length(krmar)),rep("General model",length(kgener))))
summary(aov(errors~method,anovatest))

pairwise.t.test(anovatest$errors,anovatest$method)

(LSD.test(aov(errors~method,anovatest), "method", p.adj="BH")$groups)

#Maxillosus MSE wykluczone

dane <- subset(Maxillosus2,Maxillosus2$Length<21&Maxillosus2$Length>19&Maxillosus2$Length!=16.5&Maxillosus2$Length!=22.5)

#dane <- subset(Maxillosus2,Maxillosus2$Length!=16.5&Maxillosus2$Length!=22.5)

MKadjusted <- MLmetrics:: MSE(Ka(dane$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7),dane$mk)
MKols <- MLmetrics:: MSE(-7.913*dane$Length+570.336,dane$mk)
MKrma <- MLmetrics:: MSE(-10.255145*dane$Length+616.0064,dane$mk)
MKgeneral <- MLmetrics:: MSE(rep(405.156,length(dane$mk)),dane$mk)

SMKadjusted <- sd((Ka(dane$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)-dane$mk)^2)/sqrt(19)
SMKols <- sd((-7.913*dane$Length+570.336-dane$mk)^2)/sqrt(19)
SMKrma <- sd((-10.255145*dane$Length+616.0064-dane$mk)^2)/sqrt(19)
SMKgeneral <- sd((rep(405.156,length(dane$mk))-dane$mk)^2)/sqrt(19)

Residuals_Maxillosus <- data.frame(Residuals=c(MKadjusted,MKols,MKrma,MKgeneral),
                                   lower=c(MKadjusted,MKols,MKrma,MKgeneral)-c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
                                   upper=c(MKadjusted,MKols,MKrma,MKgeneral)+c(SMKadjusted,SMKols,SMKrma,SMKgeneral),
                                   method=c(rep('Ka',length(MKadjusted)),rep('OLS',length(MKols)),rep('RMA',length(MKrma)),rep('General model',length(MKgeneral))))

boxtoplot <- gather(Residuals_Maxillosus,type,error,Residuals:upper,factor_key=TRUE)

boxtoplot %>% mutate(error=error/1018.340) -> boxtoplot

boxtoplot <- cbind(boxtoplot, label=c("b","b","b","a",rep("",8)))

#ggplot(boxtoplot,aes(method,error,label=label))+
#  geom_line()+
#  geom_point()+
#  geom_text(nudge_x = 0.06)+
#  ylab('relative error difference')+
#  theme_bw()+
#  theme(legend.position = "none")+
#  ggtitle('Creophilus maxillosus L. (Staphylinidae)')

ggplot(boxtoplot,aes(method,error,label=label,color=method))+
  geom_line()+
  geom_point()+
  geom_text(nudge_x = 0.06,col="black")+
  ylab('relative error difference')+
  theme_minimal()+
  theme(legend.position = "none")+
  ggtitle('Creophilus maxillosus L. (Staphylinidae)')

kar <- (Ka(dane$Length,k=405.156,se=14.63,lmax=25,lmin=15,m=7)-dane$mk)^2
kolsr <- (-7.913*dane$Length+570.336-dane$mk)^2
krmar <- (-10.255145*dane$Length+616.0064-dane$mk)^2
kgener <- (rep(405.156,length(dane$mk))-dane$mk)^2

anovatest <- data.frame(errors=c(kar,kolsr,krmar,kgener),
                        method=c(rep("Ka",length(kar)),rep("OLS",length(kolsr)),rep("RMA",length(krmar)),rep("General model",length(kgener))))
summary(aov(errors~method,anovatest))

pairwise.t.test(anovatest$errors,anovatest$method)

(LSD.test(aov(errors~method,anovatest), "method", p.adj="BH")$groups)

ggplot(Maxillosus,aes(Length,Time,col=factor(Temperature)))+geom_point()+geom_smooth(se=F)+ylab('Time of development')+labs(color='Temperature')+ggtitle('Creophilus maxillosus L. (Staphylinidae)')+theme_minimal()

ggplot(Necrodes,aes(Length,Time,col=factor(Temperature)))+geom_point()+geom_smooth(se=F)+ylab('Time of development')+labs(color='Temperature')+ggtitle('Necrodes littoralis L. (Silphidae)')+theme_minimal()

Necrodes %>% group_by(Length) %>% summarise(mk=median(Time),mt=(median(Temperature))) -> Necrodes2c
Maxillosus %>% group_by(Length) %>% summarise(mk=median(Time),mt=(median(Temperature))) -> Maxillosus2c

ggplot(Maxillosus2c,aes(Length,mk,col=(mt)))+
  geom_point()+geom_line(size=1,aes(y=predict(loess(mk~Length,Maxillosus2c), Maxillosus2c$Length)))+
  ylab('Time of development')+
  labs(color='Temperature')+ggtitle('Creophilus maxillosus L. (Staphylinidae)')+
  theme_bw()+
  scale_colour_gradient(low = "#00FF00", high = "#FF0000",aesthetics = "colour")

ggplot(Necrodes2c,aes(Length,mk,col=mt))+
  geom_point()+geom_line(size=1,aes(y=predict(loess(mk~Length,Necrodes2c), Necrodes2c$Length)))+
  ylab('Time of development')+labs(color='Temperature')+
  ggtitle('Necrodes littoralis L. (Silphidae)')+
  theme_bw()+
  scale_colour_gradient(low = "#00FF00", high = "#FF0000",aesthetics = "colour")

ggplot(Maxillosus2c,aes(y=Length,x=(mt)))+geom_line()+geom_smooth()
ggplot(Necrodes2c,aes(x=Length,y=(mt)))+geom_line()+geom_smooth()

broom:: tidy(lm(mt~poly(Length,4),Necrodes2c))

broom:: tidy(lm(mk~Length,Necrodes2c))

