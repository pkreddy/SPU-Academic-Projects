test_data <- arima.sim(list(ar=c(.7)),n=500)
plot(test_data)
hist(test_data)
acf(test_data)
pacf(test_data)

test_data <- arima.sim(list(ar=c(.2,.4)),n=500)
plot(test_data)
hist(test_data)
acf(test_data)
pacf(test_data)

#use pacf instead of acf for ar model

#for ma model its acf not the pacf

test_data <- arima.sim(list(ar=c(.3,.5)),n=500)
plot(test_data)
hist(test_data)
acf(test_data)
pacf(test_data)



test_data <- arima.sim(list(ma=c(.2,.4),ar=c(.3,.5)),n=500)
plot(test_data)
hist(test_data)
acf(test_data)
pacf(test_data)



testing_data <- read.csv("E:/Google Drive/Fall 17 Sem 2/640 Predictive Analytics and Financial Modelling/Project-1/SPY.csv")
View(testing_data)
spy_data <- testing_data[order(testing_data$Date),]
View(spy_data)
spy_adjclose <- spy_data[,7]
plot(spy_adjclose,type="l")
spy_diff <- diff(spy_adjclose)
plot(spy_diff,type="l")
plot(spy_diff[1:500],type="l")
hist(spy_diff)
spy_ln <- diff(log(spy_adjclose))
hist(spy_ln,breaks=40)


#use aic in the model
# aic = 2 * k - s * ln(L)

model <- arima(test_data,order = c(1,0,1))
hist(model$residuals)
acf(model$residuals)
pacf(model$residuals)
model$aic


model <- arima(test_data,order = c(2,0,2))
hist(model$residuals)
acf(model$residuals)
pacf(model$residuals)
model$aic
pred <- predict(model,n.ahead = 1)
pred$pred
auto_corr <- acf(model$residuals)
auto_corr$acf

spy_filter <- filter(spy_adjclose,filter = c(rep(1/200,200)))
plot(spy_adjclose[1:1000],type="l")
lines(spy_filter[1:1000],col="red")