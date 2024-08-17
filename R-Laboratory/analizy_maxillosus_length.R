CZAS <- Sys.time()

#library(readxl)
#library(dplyr)
#library(ggplot2)
#library(MLmetrics)


C_maxillosus_dane_do_ostatecznych_analiz <- read_excel(path)


dane <- data.frame(X=C_maxillosus_dane_do_ostatecznych_analiz$długość,Y=C_maxillosus_dane_do_ostatecznych_analiz$`k przy 11,58 ogólny`)

P=sample(dane$X,10)
L=sample(P,1)

KM <- 405.156
SM <- 14.63

Kt <- function(k=KM,sek=SM,sel=sd(P),l,ml=mean(P))
{
  return(k-(sek/sel)*(l-ml))
}

PU <- function(k=KM,ml=mean(P),l)
{
  p1=-25*l+k+25*ml
  p2=-0.05*l+k+0.05*ml
  if(l>ml) return(c(p1,p2)) else return(c(p2,p1))
}


Kt(l=L)
PU(l=L)

mean(subset(dane, dane$X==20.5)$Y)

length(subset(dane$X,dane$X==16.5))

samples <- list()
predyktor <- sample(dane$X,1)
for(i in 2:100)
{
  samples[[i-1]] <- c(predyktor,sample(dane$X,i-1))
}
FF <- function(x,y=1) sample(x,size = y)
wielkosc_proby <- 2:100
test <- data.frame(X=wielkosc_proby, SD=sapply(samples, sd), M=sapply(samples, mean), L=rep(predyktor,length(wielkosc_proby)))

test <- test %>% mutate(KT=Kt(sel=SD,ml=M,l=L))
lim <- mean(subset(dane,(dane$X>=floor(predyktor)&dane$X<=floor(predyktor)+1))$Y)
ggplot(test,aes(x=X,y=KT))+geom_line()+geom_point()+geom_abline(slope=0,intercept = mean(subset(dane,(dane$X>=floor(predyktor)&dane$X<=floor(predyktor)+1))$Y))+geom_abline(color="orange",slope=0,intercept = KM)+geom_smooth(formula = y~x,method = "loess",se=F)+geom_vline(xintercept = 25,col="lightblue")+geom_vline(xintercept = 10,col="lightblue")+scale_y_continuous(limits = c(lim-30,lim+30))+theme_minimal()

set.seed(2011)
predyktor_2 <- sample(dane$X,100)
set.seed(2011)
sample_10 <- sample(predyktor_2,10)
model_adjust <- data.frame(length=predyktor_2)
model_adjust_10 <- model_adjust %>% mutate(KT=Kt(sel=sd(sample_10),ml=mean(sample_10),l=predyktor_2))
set.seed(2011)
sample_5 <- sample(predyktor_2,5)
model_adjust_5 <- model_adjust %>% mutate(KT=Kt(sel=sd(sample_5),ml=mean(sample_5),l=predyktor_2))
set.seed(2011)
sample_15 <- sample(predyktor_2,15)
model_adjust_15 <- model_adjust %>% mutate(KT=Kt(sel=sd(sample_15),ml=mean(sample_15),l=predyktor_2))
set.seed(2011)
sample_31 <- sample(predyktor_2,31)
model_adjust_31 <- model_adjust %>% mutate(KT=Kt(sel=sd(sample_31),ml=mean(sample_31),l=predyktor_2))
set.seed(2011)
sample_60 <- sample(predyktor_2,60)
model_adjust_60 <- model_adjust %>% mutate(KT=Kt(sel=sd(sample_60),ml=mean(sample_60),l=predyktor_2))
set.seed(2011)
sample_100 <- sample(predyktor_2,100)
model_adjust_100 <- model_adjust %>% mutate(KT=Kt(sel=sd(sample_100),ml=mean(sample_100),l=predyktor_2))


model_losowy_1 <- rnorm(length(predyktor_2),KM,SM)

True_K <- NULL
for(i in 1:length(predyktor_2))
{
  True_K[i] <- mean(subset(dane,dane$X==predyktor_2[i])$Y)
}

true_model <- data.frame(length=predyktor_2,KT=True_K)

K_adjust_5 <- MSE(model_adjust_5$KT,true_model$KT)
K_adjust_10 <- MSE(model_adjust_10$KT,true_model$KT)
K_adjust_15 <- MSE(model_adjust_15$KT,true_model$KT)
K_adjust_31 <- MSE(model_adjust_31$KT,true_model$KT)
K_adjust_60 <- MSE(model_adjust_60$KT,true_model$KT)
K_adjust_100 <- MSE(model_adjust_100$KT,true_model$KT)
K_general <- MSE(rep(KM,length(model_adjust_10$KT)),true_model$KT)
K_random <- MSE(model_losowy_1,true_model$KT)

comparison <- data.frame(Model=c("K_adjust_5","K_adjust_10","K_adjust_15","K_adjust_31","K_adjust_60","K_adjust_100","K_general","K_random"),MSE=c(K_adjust_5,K_adjust_10,K_adjust_15,K_adjust_31,K_adjust_60,K_adjust_100,K_general,K_random),colour=c("J","J","J","J","J","J","G","R"))

#por?wnanie

comparison

ggplot(comparison,aes(x=reorder(Model,MSE),y=MSE,fill=colour))+geom_bar(stat="identity",show.legend = FALSE)+theme_minimal()+xlab("Model")

modele <- data.frame(x=predyktor_2,
                     model_adjust_5=model_adjust_5$KT,
                     model_adjust_10=model_adjust_10$KT,
                     model_adjust_15=model_adjust_15$KT,
                     model_adjust_31=model_adjust_31$KT,
                     model_adjust_60=model_adjust_60$KT,
                     model_adjust_100=model_adjust_100$KT,
                     general_model=rep(KM,length(model_adjust_10$KT)),
                     random_model=model_losowy_1,
                     true_model=true_model$KT)

ggplot(modele, aes(x=x)) + 
  geom_line(aes(y=model_adjust_5,color="red")) +
  geom_line(aes(y=model_adjust_10,color="green")) + 
  geom_line(aes(y=model_adjust_15,color="blue")) +
  geom_line(aes(y=model_adjust_31,color="peru")) +
  geom_line(aes(y=model_adjust_60,color="cyan")) +
  geom_line(aes(y=model_adjust_100,color="deeppink4")) +
  geom_line(aes(y=general_model,color="orange")) +
  geom_line(aes(y=true_model,color="black")) +
  geom_point(aes(y=model_adjust_5,color="red")) +
  geom_point(aes(y=model_adjust_10,color="green")) + 
  geom_point(aes(y=model_adjust_15,color="blue")) +
  geom_point(aes(y=model_adjust_31,color="peru")) +
  geom_point(aes(y=model_adjust_60,color="cyan")) +
  geom_point(aes(y=model_adjust_100,color="deeppink4")) +
  geom_point(aes(y=general_model,color="orange")) +
  geom_point(aes(y=true_model,color="black")) + theme_minimal() + ylab("Model") + xlab("weight")+
  scale_color_identity(name = "Model fit",
                       breaks = c("red", "green", "blue","peru","cyan","deeppink4","orange","black"),
                       labels = c("model_adjust_5", "model_adjust_10", "model_adjust_15","model_adjust_31","model_adjust_60","model_adjust_100","general_model","partial_mean_model"),
                       guide = "legend")
#View(dane)
ggplot(dane,aes(X,Y))+geom_point()+theme_minimal()
chmura <- dane

model_adjust_5_lm <- lm(KT~length,model_adjust_5)

predict(model_adjust_5_lm,newdata=list(length=dane$X))

model_adjust_10_lm <- lm(KT~length,model_adjust_10)
model_adjust_15_lm <- lm(KT~length,model_adjust_15)
model_adjust_31_lm <- lm(KT~length,model_adjust_31)
model_adjust_60_lm <- lm(KT~length,model_adjust_60)
model_adjust_100_lm <- lm(KT~length,model_adjust_100)

model_adjust_5_pred <- predict(model_adjust_5_lm,newdata=list(length=dane$X))
model_adjust_10_pred <- predict(model_adjust_10_lm,newdata=list(length=dane$X))
model_adjust_15_pred <- predict(model_adjust_15_lm,newdata=list(length=dane$X))
model_adjust_31_pred <- predict(model_adjust_31_lm,newdata=list(length=dane$X))
model_adjust_60_pred <- predict(model_adjust_60_lm,newdata=list(length=dane$X))
model_adjust_100_pred <- predict(model_adjust_100_lm,newdata=list(length=dane$X))
general_model_pred <- rep(KM,length(dane$X))

chmura$model_adjust_5 <- model_adjust_5_pred
chmura$model_adjust_10 <- model_adjust_10_pred
chmura$model_adjust_15 <- model_adjust_15_pred
chmura$model_adjust_31 <- model_adjust_31_pred
chmura$model_adjust_60 <- model_adjust_60_pred
chmura$model_adjust_100 <- model_adjust_100_pred
chmura$general_model <- general_model_pred

ggplot(chmura, aes(X,Y))+
  geom_point() +
  geom_line(aes(y=model_adjust_5,color="red")) +
  geom_line(aes(y=model_adjust_10,color="green")) + 
  geom_line(aes(y=model_adjust_15,color="blue")) +
  geom_line(aes(y=model_adjust_31,color="peru")) +
  geom_line(aes(y=model_adjust_60,color="cyan")) +
  geom_line(aes(y=model_adjust_100,color="deeppink4")) +
  geom_line(aes(y=general_model,color="orange")) +
  theme_minimal() +
  scale_color_identity(name = "Model fit",
                       breaks = c("red", "green", "blue","peru","cyan","deeppink4","orange"),
                       labels = c("model_adjust_5", "model_adjust_10", "model_adjust_15","model_adjust_31","model_adjust_60","model_adjust_100","general_model"),
                       guide = "legend")

K_adjust_5_C <- MSE(chmura$model_adjust_5,chmura$Y)
K_adjust_10_C <- MSE(chmura$model_adjust_10,chmura$Y)
K_adjust_15_C <- MSE(chmura$model_adjust_15,chmura$Y)
K_adjust_31_C <- MSE(chmura$model_adjust_31,chmura$Y)
K_adjust_60_C <- MSE(chmura$model_adjust_60,chmura$Y)
K_adjust_100_C <- MSE(chmura$model_adjust_100,chmura$Y)
K_general_C <- MSE(chmura$general_model,chmura$Y)

comparison_C <- data.frame(Model=c("K_adjust_5_C","K_adjust_10_C","K_adjust_15_C","K_adjust_31_C","K_adjust_60_C","K_adjust_100_C","K_general_C"),MSE=c(K_adjust_5_C,K_adjust_10_C,K_adjust_15_C,K_adjust_31_C,K_adjust_60_C,K_adjust_100_C,K_general_C),colour=c("J","J","J","J","J","J","G"))

comparison_C

ggplot(comparison_C,aes(x=reorder(Model,MSE),y=MSE,fill=colour))+geom_bar(stat="identity",show.legend = FALSE)+theme_minimal()+xlab("Model")

Random_minus <- function(l,m=KM,s=SM,ml)
{
  if(l<ml)
  {
    m+abs(rnorm(1,0,s))
  }
  else
  {
    m-abs(rnorm(1,0,s))
  }
}

mr5 <- NULL
for(i in 1:100)
{
  set.seed(2011)
  mr5[i] <- Random_minus(true_model$length[i],ml=mean(sample_5))
}

mr10 <- NULL
for(i in 1:100)
{
  set.seed(2011)
  mr10[i] <- Random_minus(true_model$length[i],ml=mean(sample_10))
}

mr15 <- NULL
for(i in 1:100)
{
  set.seed(2011)
  mr15[i] <- Random_minus(true_model$length[i],ml=mean(sample_15))
}

mr31 <- NULL
for(i in 1:100)
{
  set.seed(2011)
  mr31[i] <- Random_minus(true_model$length[i],ml=mean(sample_31))
}

mr60 <- NULL
for(i in 1:100)
{
  set.seed(2011)
  mr60[i] <- Random_minus(true_model$length[i],ml=mean(sample_60))
}

mr100 <- NULL
for(i in 1:100)
{
  set.seed(2011)
  mr100[i] <- Random_minus(true_model$length[i],ml=mean(sample_100))
}

model_ols <- lm(Y~X,dane)
K_OLS_m <- predict(model_ols,newdata=list(X=true_model$length))

model_ols_rev <- lm(X~Y,dane)
b <- coef(model_ols_rev)[1]
a <- coef(model_ols_rev)[2]
OLS_REV <- function(x)
{
  return((x-b)/a)
}
K_OLS_m_rev <- predict(model_ols_rev,newdata=list(Y=true_model$KT))

K_r_5 <- MSE(mr5,true_model$KT)
K_r_10 <- MSE(mr10,true_model$KT)
K_r_15 <- MSE(mr15,true_model$KT)
K_r_31 <- MSE(mr31,true_model$KT)
K_r_60 <- MSE(mr60,true_model$KT)
K_r_100 <- MSE(mr100,true_model$KT)
K_OLS <- MSE(K_OLS_m,true_model$KT)


comparison_R <- data.frame(Model=c("K_adjust_5",
                                   "K_r_5",
                                   "K_adjust_10",
                                   "K_r_10",
                                   "K_adjust_15",
                                   "K_r_15",
                                   "K_adjust_31",
                                   "K_r_31",
                                   "K_adjust_60",
                                   "K_r_60",
                                   "K_adjust_100",
                                   "K_r_100",
                                   "K_general",
                                   "K_random",
                                   "K_OLS"),
                           MSE=c(K_adjust_5,
                                 K_r_5,
                                 K_adjust_10,
                                 K_r_10,
                                 K_adjust_15,
                                 K_r_15,
                                 K_adjust_31,
                                 K_r_31,
                                 K_adjust_60,
                                 K_r_60,
                                 K_adjust_100,
                                 K_r_100,
                                 K_general,
                                 K_random,
                                 K_OLS),
                           colour=c("J","R","J","R","J","R","J","R","J","R","J","R","G","R","O"))


comparison_R

ggplot(comparison_R,aes(x=reorder(Model,MSE),y=MSE,fill=colour))+geom_bar(stat="identity",show.legend = FALSE)+theme_minimal()+xlab("Model")

#next comparison

K_r_5_2 <- MAPE(mr5,true_model$KT)
K_r_10_2 <- MAPE(mr10,true_model$KT)
K_r_15_2 <- MAPE(mr15,true_model$KT)
K_r_31_2 <- MAPE(mr31,true_model$KT)
K_r_60_2 <- MAPE(mr60,true_model$KT)
K_r_100_2 <- MAPE(mr100,true_model$KT)
K_OLS_2 <- MAPE(K_OLS_m,true_model$KT)
#K_OLS_REV <- MSE(K_OLS_m_rev,true_model$KT)
K_OLS_REV_2 <- MAPE(K_OLS_m_rev,true_model$length)
K_adjust_5_2 <- MAPE(model_adjust_5$KT,true_model$KT)
K_adjust_10_2 <- MAPE(model_adjust_10$KT,true_model$KT)
K_adjust_15_2 <- MAPE(model_adjust_15$KT,true_model$KT)
K_adjust_31_2 <- MAPE(model_adjust_31$KT,true_model$KT)
K_adjust_60_2 <- MAPE(model_adjust_60$KT,true_model$KT)
K_adjust_100_2 <- MAPE(model_adjust_100$KT,true_model$KT)
K_general_2 <- MAPE(rep(KM,length(model_adjust_10$KT)),true_model$KT)
K_random_2 <- MAPE(model_losowy_1,true_model$KT)


comparison_R_2 <- data.frame(Model=c("K_adjust_5",
                                     "K_r_5",
                                     "K_adjust_10",
                                     "K_r_10",
                                     "K_adjust_15",
                                     "K_r_15",
                                     "K_adjust_31",
                                     "K_r_31",
                                     "K_adjust_60",
                                     "K_r_60",
                                     "K_adjust_100",
                                     "K_r_100",
                                     "K_general",
                                     "K_random",
                                     "K_OLS",
                                     "K_OLS_REV"),
                             MSE=c(K_adjust_5_2,
                                   K_r_5_2,
                                   K_adjust_10_2,
                                   K_r_10_2,
                                   K_adjust_15_2,
                                   K_r_15_2,
                                   K_adjust_31_2,
                                   K_r_31_2,
                                   K_adjust_60_2,
                                   K_r_60_2,
                                   K_adjust_100_2,
                                   K_r_100_2,
                                   K_general_2,
                                   K_random_2,
                                   K_OLS_2,
                                   K_OLS_REV_2),
                             colour=c("J","R","J","R","J","R","J","R","J","R","J","R","G","R","O","V"))


comparison_R_2

ggplot(comparison_R_2,aes(x=reorder(Model,MSE),y=MSE,fill=colour))+geom_bar(stat="identity",show.legend = FALSE)+theme_minimal()+xlab("Model")+ylab("MAPE")

#Many samplings

sequence_matrix <- matrix(rep(0,10000),100,100)

for(i in 2:101)
{
  for(j in 1:100)
  {
    set.seed(i+j)
    m_sample_n <- sample(dane$X,i)
    X1 <- mean(m_sample_n)
    X2 <- sd(m_sample_n)
    Kset <- dane %>% mutate(KT=Kt(sel=X2,ml=X1,l=dane$X))
    sequence_matrix[i-1,j] <- MSE(Kset$KT,dane$Y)
  }
}

sequence <- as.data.frame(sequence_matrix)

if(sum(is.na(sequence))>0)
{
  imp <- mice::mice(sequence)
  sequence <- mice::complete(imp)
}

sequence %>% mutate(m=rowMeans(across())) -> sequence2

MSE_sequence <- sequence2$m

(sequence-sequence2$m)^2 %>% mutate(m=rowMeans(across())) -> sequence3

MSE.dataset <- data.frame(n=2:101,
                          MSE=MSE_sequence,
                          SD=sqrt(sequence3$m))

ggplot(MSE.dataset,aes(n,MSE))+geom_line()+theme_minimal()+geom_point()

ggplot(MSE.dataset,aes(n,SD))+geom_line()+theme_minimal()+geom_point()

testset <- NULL
for(i in 2:100)
{
  testset[i-1] <- round(100*abs(MSE.dataset$MSE[i]-MSE.dataset$MSE[i-1])/MSE.dataset$MSE[i-1],2)
}

testdata <- data.frame(n=1:99,Info=testset)



ggplot(testdata,aes(n,Info))+geom_line()+
  theme_minimal()+
  geom_vline(xintercept = max(subset(testdata,testdata$Info>1)$n),color="red")+geom_point()+ylab("Relative error %")

min(subset(testdata,testdata$Info<1)$n)

set.seed(2011)
sample_10_OPT <- sample(predyktor_2,10)
model_adjust_OPT <- data.frame(length=predyktor_2)
model_adjust_10_OPT <- model_adjust_OPT %>% mutate(KT=Kt(sel=sd(sample_10_OPT),ml=mean(sample_10_OPT),l=predyktor_2))

K_adjust_10_OPT <- MSE(model_adjust_10_OPT$KT,true_model$KT)

mr10_OPT <- NULL
for(i in 1:100)
{
  set.seed(2011)
    mr10_OPT[i] <- Random_minus(true_model$length[i],ml=mean(sample_10_OPT))
}
K_r_10_OPT <- MSE(mr10_OPT,true_model$KT)

comparison_R_3 <- data.frame(Model=c("K_adjust_10_OPT",
                                     "K_r_10_OPT",
                                     "K_general",
                                     "K_random",
                                     "K_OLS"),
                             MSE=c(K_adjust_10_OPT,
                                   K_r_10_OPT,
                                   K_general,
                                   K_random,
                                   K_OLS),
                             colour=c("J","R","G","R","O"))


comparison_R_3

ggplot(comparison_R_3,aes(x=reorder(Model,MSE),y=MSE,fill=colour))+geom_bar(stat="identity",show.legend = FALSE)+theme_minimal()+xlab("Model")+ylab("MAPE")

Sys.time() - CZAS