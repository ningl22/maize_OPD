# Optimal Plant Density (OPD)
# This code is for OPD_RF model training and simulation


## load all the necessary packages 
require(randomForest)
require(rio)
require(plyr)
require(tidyverse)
require(caret)

## step1 : model training & evaluation ===================

### K folder function
CVgroup <- function(k, datasize, seed){  
  cvlist <- list()  
  set.seed(seed)  
  n <- rep(1:k, ceiling(datasize/k))[1:datasize]   
  temp <- sample(n, datasize) 
  x <- 1:k  
  dataseq <- 1: datasize   
  cvlist <- lapply(x, function(x) dataseq[temp==x])  
  return(cvlist)  
}

### import data
df <- import('.../OPD_RF_model.xlsx')

data <-  select(df, SOM, GDD, Radn, Tmax, Tmin, Prec, OPD)

datasize <- nrow(data)
cvlist <- CVgroup(k = 5, datasize = datasize, seed =1234)

outfile <- data.frame()
for (i in 1:5){
  print(i)
  train <- data[-cvlist[[i]], ]
  test <- data[cvlist[[i]], ]
  fit <- randomForest(OPD~., train, importance=TRUE, mtry = 3, ntree=500)
  train.pre <-  predict(fit, test)
  tem <- data.frame(obs = test$OPD, pred = train.pre)
  outfile <- rbind(outfile, tem)
}

rsq <- cor(outfile$obs, outfile$pred)^2
rmse <- RMSE(outfile$obs, outfile$pred)
rrmse <- 100*rmse/mean(outfile$obs)

print(c(rsq, rmse, rrmse))


## step2 : OPD simulation ============

### load data
bs <- import('.../OPD_RF_model.xlsx')
df <- import('.../OPD_simulation.xlsx')

### def function of SOM modifying
lambda <- function(x){ifelse(x<20, 20, x)}

set.seed(1234)
train <- select(bs, SOM, GDD, Radn, Tmax, Tmin, Prec, OPD)  
fit <- randomForest(OPD~., train,  importance=TRUE, mtry = 3, ntree=500) 

### for a specific decade and climate source
df1 <- filter(df, decades == 'historical' & sources == 'CMA')

#### example for historical weather (no SOM improved)
df2 <- select(df1, SOM, GDD, Radn, Tmax, Tmin, Prec)
pre <- predict(fit, df2) # predicted OPD


#### example for historical weather (with SOM improved)
df2 <- select(df1, SOM, GDD, Radn, Tmax, Tmin, Prec)
df2$SOM <- lapply(df2$SOM, lambda)
pre <- predict(fit, df2) # predicted OPD
