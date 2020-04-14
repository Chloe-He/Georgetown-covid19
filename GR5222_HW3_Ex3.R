library(KernSmooth)
library(ibr)
library(MASS)
library(dplyr)

mse.loclin <- function (df, h, p) {
  smp_siz <- floor(p*nrow(df)) 
  train_ind <-  sample(seq_len(nrow(df)),size = smp_siz)
  trainData <- df[train_ind,] 
  testData <- df[-train_ind,]
  lp <- npregress(x=trainData$x, y=trainData$y, kernel = "e", bandwidth=h)
  predicted <- predict(lp, testData)
  return (sum((predicted-testData$y)^2)/nrow(testData))
}

mse.kfold.loclin <- function (df, h, k) {
  folds <- cut(seq(1,nrow(df)),breaks=k,labels=FALSE)
  total.error <- 0
  for(i in 1:k){
    #Segment the data
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- df[testIndexes, ]
    trainData <- df[-testIndexes, ]
    
    lp <- npregress(x=train$x, y=train$y, kernel = "e", bandwidth=h)
    predicted <- predict(lp, test)
    fold.error <- sum((predicted-trainData)^2)
    total.error <- total.error + fold.error
  }
  return (total.error/k)
}

optim.bandwith <- function(df, k) {
  
  bandwidth.grid <- seq(0, 2, length.out = 50)
  #bandwidth.grid <- 2.^seq(-8, 3, by = 0.1)
  bandwidth.result <- rep(0,length(bandwidth.grid))
  for (i in 1:length(bandwidth.grid)) {
    h <- bandwidth.grid[i]
    bandwidth.result[i] <- mse.kfold.loclin(df, h, k)
  }
  return (bandwidth.grid[which.max(bandiwdth.result)])
}

df <- mcycle %>% rename(x = times, y = accel) #data

set.seed(0)
x <- runif(100)
y <- qnorm(x)+runif(1, min=-3,max=3)
data <- matrix()
lp <- locpol(y ~ x, data = train, bw = h, kernel = EpaK, deg=1)
predict.npregress(lp, test)

##Datasts
n <- 200 # sample size of each dataset
m <- 500 # number of daasets


for (i in 1:m) {
  x <- runif(n)
  eps <- rnorm(n)
  y <- sin(2*x-1)+2*exp(-16*(x-0.5)^2)+eps
}
