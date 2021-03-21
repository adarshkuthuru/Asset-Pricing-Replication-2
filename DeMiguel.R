library(rvest)
library(tidyverse)
library(RCurl)
library(zoo)
library(tseries)
library(stringr)
library(stringi)
library(lubridate)
library(data.table)
library(ggplot2)
library(vars) #VAR
library(expm)
library(Matrix) #for matrices
library(lattice) #for matrices
library(reshape2)
library(xtable) #prints LaTeX tables
library(dplyr)
library(xts)
library(optimr)


setwd("C:/Users/KUTHURU/Downloads/Laptop/Semester 3/Empirical Asset Pricing Pierluigi/Replication-2/DeMiguel Data")



## ============================================================================== 
##                            DeMiguel et al. (2009) Replication 
## ==============================================================================  

#Dataset-1
L=readLines("SPSectors.txt")
for(i in 1:12) L <- sub("\\s+", ",", L)
data1 <- read.csv(text = L)
data1$date=as.Date(format(data1$date),"%Y%m%d")
#create excess returns columns
for(i in 3:13){
  for(j in 1:nrow(data1)){
    data1[j,i+11]=data1[j,i]-data1[j,2]
  }
}
#convert dataframe to timeseries object
data12=data1[ ,c(1,14:24)]
data13 <- xts(data12[,-1], order.by=as.Date(data12[,1], "%m/%d/%Y"))



#Dataset-2
data2=read.delim("Industry.txt")
data2$date=as.Date(format(data2$X.Date),"%Y%m%d")
#create excess returns columns
for(i in 3:12){
  for(j in 1:nrow(data2)){
    data2[j,i+12]=data2[j,i]-data2[j,2]
  }
}
if(i==12){for(j in 1:nrow(data2)){data2[j,25]=data2[j,13]}}
#convert dataframe to timeseries object
data22=data2[ ,c(14:25)]
data23 <- xts(data22[,-1], order.by=as.Date(data22[,1], "%m/%d/%Y"))



#Dataset-3
data3=read.delim("F-F_Research_Data_Factors.txt")
data3$date=as.Date(format(data3$X.Date),"%Y%m%d")
#create excess returns columns
for(i in 3:4){
  for(j in 1:nrow(data3)){
    data3[j,i+4]=data3[j,i]-data3[j,2]
  }
}
if(i==4){for(j in 1:nrow(data3)){data3[j,9]=data3[j,5]}}
#convert dataframe to timeseries object
data32=data3[ ,c(6:9)]
data33 <- xts(data32[,-1], order.by=as.Date(data32[,1], "%m/%d/%Y"))

#Dataset-4
L=readLines("25_Portfolios_5x5_1Factor.txt")
for(i in 1:22) L <- sub("\\s+", ",", L)
data4 <- read.csv(text = L)
data4$date=as.Date(format(data4$Date),"%Y%m%d")
#create excess returns columns
for(i in 3:22){
  for(j in 1:nrow(data4)){
    data4[j,i+22]=data4[j,i]-data4[j,2]
  }
}
if(i==22){for(j in 1:nrow(data4)){data4[j,45]=data4[j,23]}}
#convert dataframe to timeseries object
data42=na.omit(data4[ ,c(24:45)])
data43 <- xts(data42[,-1], order.by=as.Date(data42[,1], "%m/%d/%Y"))

#Dataset-5
L=readLines("25_Portfolios_5x5_MOM.txt")
for(i in 1:25) L <- sub("\\s+", ",", L)
data5 <- read.csv(text = L)
data5$date=as.Date(format(data5$X.Date),"%Y%m%d")
#create excess returns columns
for(i in 3:22){
  for(j in 1:nrow(data5)){
    data5[j,i+25]=data5[j,i]-data5[j,2]
  }
}
for(i in 23:25){for(j in 1:nrow(data5)){data5[j,i+25]=data5[j,i]}}
#convert dataframe to timeseries object
data52=na.omit(data5[ ,c(27:50)])
data53 <- xts(data52[,-1], order.by=as.Date(data52[,1], "%m/%d/%Y"))


## ============================================================================== 
##                                  Create Table-3
## ============================================================================== 



#################################
#           Column-1
#################################

## Strategies
#1/N 
mu <- colMeans(data13); Sigma <- cov(data13); 
ones <- rep(1,ncol(data13))
w <- t(matrix(rep(1/ncol(data13),ncol(data13))))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP1 <- muMVP/sigmaMVP

#Mean Variance portfolio
w <- t(solve(Sigma) %*% mu)/drop(ones %*% solve(Sigma) %*% mu)
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP2 <- muMVP/sigmaMVP

#Minimum Variance portfolio
div.ratio <- function(weight, vol, cov.mat){
  weight <- weight / sum(weight)
  dr <- (t(weight) %*% vol) / (sqrt(t(weight) %*% cov.mat %*% (weight)))  
  return(-dr)
}
vol<-apply(data13,2,sd)
out <- optim(par     = rep(1 / length(vol), length(vol)),  # initial guess
             fn      = div.ratio,
             vol     = vol,
             cov.mat = Sigma,
             method  = "L-BFGS-B",
             lower   = 0,
             upper   = 1)
w <- t(matrix(out$par/sum(out$par)))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP3 <- muMVP/sigmaMVP

#value-weighted portfolio
mu <- colMeans(data13); Sigma <- cov(data13); 
ones <- rep(1,data13$mcap)
w <- t(matrix(rep(1/data13$mcap),ncol(data13)))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP1 <- muMVP/sigmaMVP


#MP-portfolio
var=t(matrix(diag(var(data13))))
Sigma <- mu%*%t(mu) +cov(data13)%*%diag(ncol(data13))
w <- t(solve(Sigma) %*% mu)/drop(ones %*% solve(Sigma) %*% mu)
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP4 <- muMVP/sigmaMVP

res <-rbind(srMVP1,srMVP2,srMVP3,srMVP4)


#################################
#           Column-2
#################################

## Strategies
#1/N 
mu <- colMeans(data23); Sigma <- cov(data23); 
ones <- rep(1,ncol(data23))
w <- t(matrix(rep(1/ncol(data23),ncol(data23))))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP1 <- muMVP/sigmaMVP

#Mean Variance portfolio
w <- t(solve(Sigma) %*% mu)/drop(ones %*% solve(Sigma) %*% mu)
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP2 <- muMVP/sigmaMVP

#Minimum Variance portfolio
div.ratio <- function(weight, vol, cov.mat){
  weight <- weight / sum(weight)
  dr <- (t(weight) %*% vol) / (sqrt(t(weight) %*% cov.mat %*% (weight)))  
  return(-dr)
}
vol<-apply(data13,2,sd)
out <- optim(par     = rep(1 / length(vol), length(vol)),  # initial guess
             fn      = div.ratio,
             vol     = vol,
             cov.mat = Sigma,
             method  = "L-BFGS-B",
             lower   = 0,
             upper   = 1)
w <- t(matrix(out$par/sum(out$par)))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP3 <- muMVP/sigmaMVP

#value-weighted portfolio
mu <- colMeans(data23); Sigma <- cov(data23); 
ones <- rep(1,data23$mcap)
w <- t(matrix(rep(1/data23$mcap),ncol(data23)))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP1 <- muMVP/sigmaMVP

#MP-portfolio
var=t(matrix(diag(var(data23))))
Sigma <- mu%*%t(mu) +cov(data23)%*%diag(ncol(data23))
w <- t(solve(Sigma) %*% mu)/drop(ones %*% solve(Sigma) %*% mu)
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP4 <- muMVP/sigmaMVP

res2 <-rbind(srMVP1,srMVP2,srMVP3,srMVP4)



#################################
#           Column-3
#################################

## Strategies
#1/N 
mu <- colMeans(data33); Sigma <- cov(data33); 
ones <- rep(1,ncol(data33))
w <- t(matrix(rep(1/ncol(data33),ncol(data33))))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP1 <- muMVP/sigmaMVP

#Mean Variance portfolio
w <- t(solve(Sigma) %*% mu)/drop(ones %*% solve(Sigma) %*% mu)
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP2 <- muMVP/sigmaMVP

#Minimum Variance portfolio
div.ratio <- function(weight, vol, cov.mat){
  weight <- weight / sum(weight)
  dr <- (t(weight) %*% vol) / (sqrt(t(weight) %*% cov.mat %*% (weight)))  
  return(-dr)
}
vol<-apply(data33,2,sd)
out <- optim(par     = rep(1 / length(vol), length(vol)),  # initial guess
             fn      = div.ratio,
             vol     = vol,
             cov.mat = Sigma,
             method  = "L-BFGS-B",
             lower   = 0,
             upper   = 1)
w <- t(matrix(out$par/sum(out$par)))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP3 <- muMVP/sigmaMVP

#value-weighted portfolio
mu <- colMeans(data33); Sigma <- cov(data33); 
ones <- rep(1,data33$mcap)
w <- t(matrix(rep(1/data33$mcap),ncol(data33)))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP1 <- muMVP/sigmaMVP

#MP-portfolio
var=t(matrix(diag(var(data33))))
Sigma <- mu%*%t(mu) +cov(data33)%*%diag(ncol(data33))
w <- t(solve(Sigma) %*% mu)/drop(ones %*% solve(Sigma) %*% mu)
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP4 <- muMVP/sigmaMVP

res3 <-rbind(srMVP1,srMVP2,srMVP3,srMVP4)




#################################
#           Column-4
#################################

## Strategies
#1/N 
data43<-na.omit(data43[,-(1:5)])
mu <- colMeans(data43); Sigma <- cov(data43); 
ones <- rep(1,ncol(data43))
w <- t(matrix(rep(1/ncol(data43),ncol(data43))))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP1 <- muMVP/sigmaMVP

#Mean Variance portfolio
w <- t(solve(Sigma) %*% mu)/drop(ones %*% solve(Sigma) %*% mu)
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP2 <- muMVP/sigmaMVP

#Minimum Variance portfolio
div.ratio <- function(weight, vol, cov.mat){
  weight <- weight / sum(weight)
  dr <- (t(weight) %*% vol) / (sqrt(t(weight) %*% cov.mat %*% (weight)))  
  return(-dr)
}
vol<-apply(data43,2,sd)
out <- optim(par     = rep(1 / length(vol), length(vol)),  # initial guess
             fn      = div.ratio,
             vol     = vol,
             cov.mat = Sigma,
             method  = "L-BFGS-B",
             lower   = 0,
             upper   = 1)
w <- t(matrix(out$par/sum(out$par)))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP3 <- muMVP/sigmaMVP

#value-weighted portfolio
mu <- colMeans(data43); Sigma <- cov(data43); 
ones <- rep(1,data43$mcap)
w <- t(matrix(rep(1/data43$mcap),ncol(data43)))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP1 <- muMVP/sigmaMVP

#MP-portfolio
var=t(matrix(diag(var(data43))))
Sigma <- mu%*%t(mu) +cov(data43)%*%diag(ncol(data43))
w <- t(solve(Sigma) %*% mu)/drop(ones %*% solve(Sigma) %*% mu)
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP4 <- muMVP/sigmaMVP

res4 <-rbind(srMVP1,srMVP2,srMVP3,srMVP4)


#################################
#           Column-5
#################################

## Strategies
#1/N 
mu <- colMeans(data53); Sigma <- cov(data53); 
ones <- rep(1,ncol(data53))
w <- t(matrix(rep(1/ncol(data53),ncol(data53))))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP1 <- muMVP/sigmaMVP

#Mean Variance portfolio
w <- t(solve(Sigma) %*% mu)/drop(ones %*% solve(Sigma) %*% mu)
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP2 <- muMVP/sigmaMVP

#Minimum Variance portfolio
div.ratio <- function(weight, vol, cov.mat){
  weight <- weight / sum(weight)
  dr <- (t(weight) %*% vol) / (sqrt(t(weight) %*% cov.mat %*% (weight)))  
  return(-dr)
}
vol<-apply(data53,2,sd)
out <- optim(par     = rep(1 / length(vol), length(vol)),  # initial guess
             fn      = div.ratio,
             vol     = vol,
             cov.mat = Sigma,
             method  = "L-BFGS-B",
             lower   = 0,
             upper   = 1)
w <- t(matrix(out$par/sum(out$par)))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP3 <- muMVP/sigmaMVP

#value-weighted portfolio
mu <- colMeans(data53); Sigma <- cov(data53); 
ones <- rep(1,data53$mcap)
w <- t(matrix(rep(1/data53$mcap),ncol(data53)))
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP1 <- muMVP/sigmaMVP

#MP-portfolio
var=t(matrix(diag(var(data53))))
Sigma <- mu%*%t(mu) +cov(data53)%*%diag(ncol(data53))
w <- t(solve(Sigma) %*% mu)/drop(ones %*% solve(Sigma) %*% mu)
muMVP <- drop(w%*%mu); sigmaMVP <- drop(w %*% Sigma %*% t(w))^0.5
srMVP4 <- muMVP/sigmaMVP

res5 <-rbind(srMVP1,srMVP2,srMVP3,srMVP4)


final <-cbind(res,res2,res3,res4,res5)

final <- as.data.table(final)

#Print tables into LaTeX
print(xtable(final), sanitize.colnames.function=function(x){x},
      sanitize.rownames.function=function(x){x}) #table2 should be a matrix class






