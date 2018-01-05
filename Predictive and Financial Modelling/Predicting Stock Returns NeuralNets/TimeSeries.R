#read 15 coefficients out of 20 for training the time series model
coefficients_train = read.csv(file="E:/Google Drive/Fall 17 Sem 2/Academic Projects/Predictive and Financial Modelling/Predicting Stock Returns/coefficients_train.csv")
#removing first dummy column
coefficients_train <- coefficients_train[,c(-1)]
coefficients_train <- coefficients_train[order(coefficients_train$unique_dates_train),]

#read all the 20 coefficients
coefficients_all = read.csv(file="E:/Google Drive/Fall 17 Sem 2/Academic Projects/Predictive and Financial Modelling/Predicting Stock Returns/coefficients_all.csv")
#removing first dummy column
coefficients_all <- coefficients_all[,c(-1)]
coefficients_all <- coefficients_all[order(coefficients_all$unique_dates),]
colnames(coefficients_all)[1] <- "unique_dates_train"

#read reduce_file which contains filtererd tickers with respect to health care domian
#reduce_file has been cleaned for outliers and other data (NA's, Empty, Non conforming data)
reduce_file <- read.csv(file="E:/Google Drive/Fall 17 Sem 2/Academic Projects/Predictive and Financial Modelling/Predicting Stock Returns/reduce_file.csv")


#bestArima gives the output as p,d,q values which will have least aic
bestArima<-function(data,print=TRUE){
  data <- coefficients_train[,2]
  min <- arima(data,order=c(0,0,0))$aic
  p_value <- 0
  d_value <- 0
  q_value <- 0
  d <- 0
  arima_values <- NULL
  for(p in 0:2){
    #for(d in 0:1){
    for(q in 0:2){
      #browser()
      #out <- tryCatch(aic_value <- arima(data,order=c(p,d,q))$aic,  error = function() break)
      aic_value <- arima(data,order=c(p,d,q))$aic
      #arima_values <-  rbind(arima_values,cbind(p,d,q,aic_value))
      if(aic_value<min){
        min = aic_value
        p_value <- p
        d_value <- d
        q_value <- q
      }
      # }
    }
  }
  return (c(p_value,d_value,q_value))
}

#getAutoArima returns values for p.d.q using auto.arima function
getAutoArima<-function(data){
  arima_model <- auto.arima(data)
  return (arimaorder(arima_model))
}

#auto.arima(testing_data)

"
spy_adjclose <- coefficients_train[,2]
plot(spy_adjclose,type='l')
spy_diff <- diff(spy_adjclose)
plot(spy_diff,type='l')
hist(spy_diff)

model <- arima(testing_data,order = c(0,0,0))
hist(model$residuals)
acf(model$residuals)
pacf(model$residuals)
model$aic
pred <- predict(model,n.ahead = 1)
pred$pred
"
#predicted time series rerturns predicted values for next one betas aheaad
#for all betas with given data
predictTimeSeries <- function(data){
  predicted_table <- NULL
  for (i in 2:dim(data)[2]){
    arma_values <- getAutoArima(data[,i])
    model <- arima(data[,i],order = arma_values)
    pred <- predict(model,n.ahead = 1)
    predicted_table <- cbind(predicted_table,pred$pred)
  }
  return (predicted_table)
}

library(forecast)
coefPredicted <- NULL
coefDatesCopy <- coefficients_train
#predicting the betas from 16 to 20
for (i in 16:20){
  timeSeriesPred <- predictTimeSeries(coefDatesCopy)
  #browser()
  temp <- cbind(coefficients_all[i,1],timeSeriesPred)
  colnames(temp) <- (names(coefDatesCopy))
  #colnames(timeSeriesPred) <- data.frame(names(coefDatesCopy))[c(2:22),]
  coefPredicted <- rbind(coefPredicted,temp)
  coefDatesCopy <- rbind(coefDatesCopy,coefficients_all[i,])
  timeSeriesPred <- NULL
  temp <- NULL
}


calculate_returns <- function(data,date){
  data <- ln_returns20141231
  ln_retrurns_actual <- data[,23]
  data[,23] <- NULL
  date <- data[,2]
  ticker <- data[,1]
  data[,1] <- NULL
  data[,1] <- NULL
  intercept <- coefDatesCopy[]
  
}

#reduce_file <- read.csv(file="E:/Google Drive/Fall 17 Sem 2/Academic Projects/Predictive and Financial Modelling/Predicting Stock Returns/reduce_file.csv")
reduce_file <- reduce_file[,2:24]
temp <- reduce_file$ln_returns
reduce_file$ln_returns <- NULL
reduce_file <- cbind(temp,reduce_file)
colnames(reduce_file)[1] <- "ln_returns"


# for(i in 16:20){
#   a <- paste("ln_returns_predicted",coefficients_all[i,1],sep="")
#   assign(a,subset(reduce_file,calendardate==coefficients_all[i,1]))
# }

getPredictedReturns <- function(data,betas){
  ln_returns <- data$ln_returns
  data$ln_returns <- NULL
  ticker <- data$ticker
  data$ticker <- NULL
  calendardate <- data$calendardate
  data$calendardate <- NULL
  unique_dates_train <- betas$unique_dates_train
  betas$unique_dates_train <- NULL
  intercept <- betas$X.Intercept.
  betas$X.Intercept. <- NULL
  predicted_ln_returns <- vector()
  for(i in 1:dim(data)[1]){
    predicted <- data[i,] * betas
    predicted_ln_returns <- rbind(predicted_ln_returns,sum(predicted) + intercept)
  }
  #browser()
  predicted <- data.frame(predicted_ln_returns,ln_returns,ticker)
  #colnames(predicted_ln_returns) <- "predicted_ln_returns"
  #predicted_ln_returns <- cbind(ln_returns,predicted_ln_returns)
  #predicted_ln_returns <- cbind(predicted_ln_returns,ticker)
  return(predicted)
}

createBuckets <- function(data){
  data <- data.frame(data)
  data$ticker <- NULL
  #browser()  
  data <- data[order(data$predicted_ln_returns, decreasing = TRUE),]
  bucket_value <- floor(dim(data)[1] / 5)
  bucket1 <- data[1:bucket_value,]
  bucket1 <- as.data.frame(apply(bucket1,2,mean))
  bucket2 <- data[((bucket_value * 1) + 1) : (bucket_value * 2),]
  bucket2 <- as.data.frame(apply(bucket2,2,mean))
  bucket3 <- data[((bucket_value * 2) + 1): (bucket_value * 3),]
  bucket3 <- as.data.frame(apply(bucket3,2,mean))
  bucket4 <- data[((bucket_value * 3) + 1) : ((bucket_value * 4)),]
  bucket4 <- as.data.frame(apply(bucket4,2,mean))
  bucket5 <- data[((floor(bucket_value) * 4) + 1): (dim(data)[1]),]
  bucket5 <- as.data.frame(apply(bucket5,2,mean))
  bucket <- data.frame(bucket1,bucket2,bucket3,bucket4,bucket5)

  names(bucket) <- c("bucket1","bucket2","bucket3","bucket4","bucket5")
  return(bucket)
}

for(i in coefPredicted[,1]){
  reduce_file_subset <- subset(reduce_file,calendardate==i)
  betas_subset <- subset(as.data.frame(coefPredicted),unique_dates_train==i)
  predicted_returns <- getPredictedReturns(reduce_file_subset,betas_subset)
  predicted_returns_name <- paste("ln_returns_predicted_",i,sep="")
  assign(predicted_returns_name,predicted_returns)
  bucket_name <- paste("bucket_",i,sep = "")
  assign(bucket_name,createBuckets(predicted_returns))
}
