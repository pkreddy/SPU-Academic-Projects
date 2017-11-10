coefficients_train = read.csv(file="E:/Google Drive/Fall 17 Sem 2/Academic Projects/Predictive and Financial Modelling/Predicting Stock Returns/coefficients_train.csv")
coefficients_train <- coefficients_train[,c(-1)]
coefficients_train <- coefficients_train[order(coefficients_train$unique_dates_train),]

coefficients_all = read.csv(file="E:/Google Drive/Fall 17 Sem 2/Academic Projects/Predictive and Financial Modelling/Predicting Stock Returns/coefficients_all.csv")
coefficients_all <- coefficients_all[,c(-1)]
coefficients_all <- coefficients_all[order(coefficients_all$unique_dates),]

reduce_file <- read.csv(file="E:/Google Drive/Fall 17 Sem 2/Academic Projects/Predictive and Financial Modelling/Predicting Stock Returns/reduce_file.csv")


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

getAutoArima<-function(data){
  arima_model <- auto.arima(data)
  return (arimaorder(arima_model))
}
auto.arima(testing_data)

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
predictTimeSeries <- function(data){
  predicted_table <- NULL
  for (i in 2:22){
    #browser()
    arma_values <- getAutoArima(data[,i])
    model <- arima(data[,i],order = arma_values)
    pred <- predict(model,n.ahead = 1)
    predicted_table <- cbind(predicted_table,pred$pred)
  }
  return (predicted_table)
}


coefDatesCopy <- coefficients_train
#predicting the betas from 16 to 20
for (i in 16:20){
  timeSeriesPred <- predictTimeSeries(coefDatesCopy)
  temp <- cbind(coefficients_all[i,1],timeSeriesPred)
  colnames(temp) <- (names(coefDatesCopy))
  #colnames(timeSeriesPred) <- data.frame(names(coefDatesCopy))[c(2:22),]
  coefDatesCopy <- rbind(coefDatesCopy,temp)
  timeSeriesPred <- NULL
  temp <- NULL
}

