CZAS <- Sys.time()

#library(readxl)
#library(dplyr)
#library(ggplot2)
#library(MLmetrics)

C_maxillosus_dane_do_ostatecznych_analiz <- read_excel(path)

dane <- data.frame(X=C_maxillosus_dane_do_ostatecznych_analiz$waga,Y=C_maxillosus_dane_do_ostatecznych_analiz$`k przy 11,58 ogólny`)

P=sample(dane$X,10)
L=sample(P,1)

KM <- 405.156
SM <- 14.63

#KM <- mean(dane$Y)
#SM <- sd(dane$Y)

Kt <- function(k=KM,sek=SM,sel=sd(P),l,ml=mean(P))
{
  return(k-0.3*sqrt(10)*(sek/sel)*(l-ml))
}

PU <- function(k=KM,ml=mean(P),l)
{
  p1=-25*l+k+25*ml
  p2=-0.05*l+k+0.05*ml
  if(l>ml) return(c(p1,p2)) else return(c(p2,p1))
}

model_ols <- lm(Y~X,dane)
K_OLS_m <- predict(model_ols,newdata=list(X=dane$X))
K_OLS <- MSE(K_OLS_m,dane$Y)

sma(Y~X,dane)
K_RMA_m <- -1.859622*dane$X+665.8478
K_RMA <- MSE(K_RMA_m,dane$Y)

sequence_matrix <- NULL

for(j in 1:100)
{
  set.seed(10+j)
  m_sample_n <- sample(dane$X,10) 
  X1 <- mean(m_sample_n)
  X2 <- sd(m_sample_n)
  Kset <- dane %>% mutate(KT=Kt(sel=X2,ml=X1,l=dane$X))
  sequence_matrix[j] <- MSE(Kset$KT,dane$Y)
}

sequence <- data.frame(n=1:100,MSE=sequence_matrix)

if(sum(is.na(sequence))>0)
{
  imp <- mice::mice(sequence)
  sequence <- mice::complete(imp)
}

sequence_matrix_OLS <- NULL

for(j in 1:100)
{
  set.seed(10+j)
  m_sample_n <- sample(dane$X,10)
  KsetOLS <- predict(lm(Y~X,data.frame(X=m_sample_n,Y=dane$Y[match(m_sample_n,dane$X)])),newdata = list(X=dane$X))
  sequence_matrix_OLS[j] <- MSE(KsetOLS,dane$Y)
}

sequence_OLS <- data.frame(n=1:100,MSE=sequence_matrix_OLS)

if(sum(is.na(sequence_OLS))>0)
{
  imp <- mice::mice(sequence_OLS)
  sequence_OLS <- mice::complete(imp)
}

K_OLS_sampled <- mean(sequence_OLS$MSE)

K_adjust_10_OPT <- mean(sequence$MSE)

K_general <- MSE(rep(KM,length(dane$X)),dane$Y)

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

mr10 <- NULL
r_10 <- NULL
for(j in 1:100)
{
  for(i in 1:length(dane$X))
  {
    sample_10 <- sample(dane$X,10)
    mr10[i] <- Random_minus(dane$X[i],ml=mean(sample_10))
  }
  r_10[j] <- MSE(mr10,dane$Y)
}
K_r_10 <- mean(r_10)

mse_random <- NULL
for(i in 1:100)
{
  random <- rnorm(length(dane$X),KM,SM)
  mse_random[i] <- MSE(random,dane$Y)
}

K_random <- mean(mse_random)

comparison <- data.frame(
  Model=c("K_OLS",
          "K_adjust_10_OPT",
          "K_general",
          "K_r_10",
          "K_random",
          "K_RMA",
          "K_OLS_sampled"),
  MSE=c(K_OLS,
        K_adjust_10_OPT,
        K_general,
        K_r_10,
        K_random,
        K_RMA,
        K_OLS_sampled),
  colour=c("O",
           "A",
           "G",
           "R",
           "R",
           "A",
           "O")
)
ggplot(comparison,aes(x=reorder(Model,MSE),y=MSE,fill=colour))+
  geom_bar(stat="identity",show.legend = FALSE)+
  theme_minimal()+
  xlab("Model")

model_ols_rev <- lm(X~Y,dane)
K_OLS_m_rev <- predict(model_ols_rev,newdata=list(Y=dane$Y))
K_OLS_rev <- MAPE(K_OLS_m_rev,dane$X)

K_OLS_2 <- MAPE(K_OLS_m,dane$Y)
K_RMA_2 <- MAPE(K_RMA_m,dane$Y)

sequence_matrix_2 <- NULL

for(j in 1:100)
{
  set.seed(10+j)
  m_sample_n_2 <- sample(dane$X,10)
  X1 <- mean(m_sample_n_2)
  X2 <- sd(m_sample_n_2)
  Kset <- dane %>% mutate(KT=Kt(sel=X2,ml=X1,l=dane$X))
  sequence_matrix_2[j] <- MAPE(Kset$KT,dane$Y)
}

sequence_2 <- data.frame(n=1:100,MAPE=sequence_matrix_2)

if(sum(is.na(sequence_2))>0)
{
  imp <- mice::mice(sequence_2)
  sequence_2 <- mice::complete(imp)
}

sequence_matrix_OLS_2 <- NULL
for(j in 1:100)
{
  set.seed(10+j)
  m_sample_n_2 <- sample(dane$X,10)
  KsetOLS_2 <- predict(lm(Y~X,data.frame(X=m_sample_n_2,Y=dane$Y[match(m_sample_n_2,dane$X)])),newdata = list(X=dane$X))
  sequence_matrix_OLS_2[j] <- MAPE(KsetOLS_2,dane$Y)
}

sequence_OLS_2 <- data.frame(n=1:100,MSE=sequence_matrix_OLS_2)

if(sum(is.na(sequence_OLS_2))>0)
{
  imp <- mice::mice(sequence_OLS_2)
  sequence_OLS_2 <- mice::complete(imp)
}

K_OLS_sampled_OLS_2 <- mean(sequence_OLS_2$MSE)

K_adjust_10_OPT_2 <- mean(sequence_2$MAPE)

K_general_2 <- MAPE(rep(KM,length(dane$X)),dane$Y)

mr10_2 <- NULL
r_10_2 <- NULL
for(j in 1:100)
{
  for(i in 1:length(dane$X))
  {
    sample_10 <- sample(dane$X,10)
    mr10_2[i] <- Random_minus(dane$X[i],ml=mean(sample_10))
  }
  r_10_2[j] <- MAPE(mr10_2,dane$Y)
}
K_r_10_2 <- mean(r_10_2)

mse_random_2 <- NULL
for(i in 1:100)
{
  random <- rnorm(length(dane$X),KM,SM)
  mse_random_2[i] <- MAPE(random,dane$Y)
}

K_random_2 <- mean(mse_random_2)

comparison_2 <- data.frame(
  Model=c("K_OLS",
          "K_adjust_10_OPT",
          "K_general",
          "K_r_10",
          "K_random",
          "K_OLS_rev",
          "K_RMA_2",
          "K_OLS_sampled_OLS_2"),
  MAPE=c(K_OLS_2,
         K_adjust_10_OPT_2,
         K_general_2,
         K_r_10_2,
         K_random_2,
         K_OLS_rev,
         K_RMA_2,
         K_OLS_sampled_OLS_2),
  colour=c("O",
           "A",
           "G",
           "R",
           "R",
           "O",
           "A",
           "O")
)
ggplot(comparison_2,aes(x=reorder(Model,MAPE),y=MAPE,fill=colour))+
  geom_bar(stat="identity",show.legend = FALSE)+
  theme_minimal()+
  xlab("Model")

Sys.time() - CZAS
