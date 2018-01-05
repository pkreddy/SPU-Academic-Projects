createBuckets <- function(data_set){
  data <- data.frame(data_set)
  data$ticker <- NULL
  browser()  
  data_sorted <- data[order(data$predicted, decreasing = TRUE),]
  data <- data.frame(data_sorted[,2])
  bucket_value <- floor(dim(data)[1] / 5)
  #bucket_value <- floor(length(data) / 5)
  bucket1 <- data.frame(data[1:bucket_value,])
  bucket1 <- as.data.frame(apply(bucket1,2,mean))
  bucket2 <- data.frame(data[((bucket_value * 1) + 1) : (bucket_value * 2),])
  bucket2 <- as.data.frame(apply(bucket2,2,mean))
  bucket3 <- data.frame(data[((bucket_value * 2) + 1): (bucket_value * 3),])
  bucket3 <- as.data.frame(apply(bucket3,2,mean))
  bucket4 <- data.frame(data[((bucket_value * 3) + 1) : ((bucket_value * 4)),])
  bucket4 <- as.data.frame(apply(bucket4,2,mean))
  bucket5 <- data.frame(data[((floor(bucket_value) * 4) + 1): (dim(data)[1]),])
  #bucket5 <- data[((floor(bucket_value) * 4) + 1): (length(data)),]
  bucket5 <- as.data.frame(apply(bucket5,2,mean))
  bucket_predicted <- data.frame(bucket1,bucket2,bucket3,bucket4,bucket5)
  names(bucket_predicted) <- c("bucket1","bucket2","bucket3","bucket4","bucket5")
  
  
  data <- data.frame(data_sorted[,1])
  bucket_value <- floor(dim(data)[1] / 5)
  #bucket_value <- floor(length(data) / 5)
  bucket1 <- data.frame(data[1:bucket_value,])
  bucket1 <- as.data.frame(apply(bucket1,2,mean))
  bucket2 <- data.frame(data[((bucket_value * 1) + 1) : (bucket_value * 2),])
  bucket2 <- as.data.frame(apply(bucket2,2,mean))
  bucket3 <- data.frame(data[((bucket_value * 2) + 1): (bucket_value * 3),])
  bucket3 <- as.data.frame(apply(bucket3,2,mean))
  bucket4 <- data.frame(data[((bucket_value * 3) + 1) : ((bucket_value * 4)),])
  bucket4 <- as.data.frame(apply(bucket4,2,mean))
  bucket5 <- data.frame(data[((floor(bucket_value) * 4) + 1): (dim(data)[1]),])
  #bucket5 <- data[((floor(bucket_value) * 4) + 1): (length(data)),]
  bucket5 <- as.data.frame(apply(bucket5,2,mean))
  bucket_actual <- data.frame(bucket1,bucket2,bucket3,bucket4,bucket5)
  
  names(bucket_actual) <- c("bucket1","bucket2","bucket3","bucket4","bucket5")
  
  bucket <- rbind(bucket_predicted,bucket_actual)
  rownames(bucket) <- c("bucket_predicted","bucket_actual")
  return(bucket)
}


###Using Naive Prediction Model for weights##
set.seed(1234)
dates <- c(20140930,unique_dates_test)

for(i in 2:length(dates)-1){
  temp <- subset(reduce_file_tickers_data,calendardate==dates[i])
  temp[c('calendardate','ticker')] <- list(NULL)
  n_factors <- c('revenue','cor','rnd','ebit','netinc','epsdil','ncfo','ncfi','capex','ncfdiv',
                 'intangibles','debt','de','pe','pb','fcfps','assets','netmargin','marketcap','dps')
  # Collapsing them together
  n_factors <- paste(n_factors, collapse = "+")
  formula_ <- as.formula(paste("ln_returns~", n_factors, collapse = "+"))
  
  ln_returns_pred <- vector()
  model <- neuralnet(formula = formula_, temp, c(10,5))
  temp_1 <- subset(reduce_file_tickers_data,calendardate==dates[i+1])
  temp_1[c('calendardate','ticker')] <- list(NULL)
  pred <- compute(model, temp_1[,1:20])
  ln_returns_pred <- cbind(temp_1$ln_returns,pred$net.result)
  colnames(ln_returns_pred) <- c("actual","predicted")
  
  bucket_name <- paste("bucket_",dates[i+1],sep = "")
  assign(bucket_name,createBuckets(ln_returns_pred))
  
}