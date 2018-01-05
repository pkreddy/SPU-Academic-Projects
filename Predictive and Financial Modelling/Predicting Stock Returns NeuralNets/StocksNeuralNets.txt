library(neuralnet)
library(MASS)

factor_data <- read.csv("E:/Google Drive/Fall 17 Sem 2/Academic Projects/Predictive and Financial Modelling/Predicting Stock Returns/final_data.csv")
View(factor_data)

ln_returns <- vector();

for(i in 1:length(factor_data$ticker)){
  if(identical(factor_data$ticker[i+1],factor_data$ticker[i])){
    ln_returns[i] = log(factor_data$price[i+1]/factor_data$price[i]);
  }else{
    ln_returns[i] = -999;
  }
}


ln_returns[1:20]
factor_data <- cbind(ln_returns,factor_data)
# factor_data <- subset(factor_data, ln_returns > -100)
factor_data <- subset(factor_data,ln_returns != Inf & ln_returns != -Inf)
ln_returns <- NULL
ln_returns <- factor_data$ln_returns
data_header <- names(factor_data)
data_header
data_dates <- unique(factor_data$calendardate)
data_dates

data_file_unique <- vector()
for(i in data_dates){
  a <- paste("factor_data_", i, sep="");
  assign(a, subset(factor_data, calendardate==i));
  data_file_unique <- c(data_file_unique, a)
}


NA_data_file_unique <- vector()
for(j in data_file_unique){
  a <- paste("NA_list_", j, sep="");
  NA_data_file_unique <- c(NA_data_file_unique, a);
  NA_list <- vector();
  for(i in data_header){
    z <- is.na(get(j)[[i]]);
    if((sum(z)/length(get(j)[,1])) > .10){
      NA_list <- c(NA_list, i)
    }
  }
  
  assign(a, NA_list)
}

# To obtain columns to be deleted
Na_unique_merge_list <- vector();
for(i in NA_data_file_unique){
  Na_unique_merge_list <- c(Na_unique_merge_list, get(i))
}
Na_unique_merge_list <- unique(Na_unique_merge_list)

reduce_list <- vector("list", length = length(data_dates))

reduce_data_file_unique_date_data_list <- vector()
for(i in 1:length(data_file_unique)){
  data_file_reduced <- get(data_file_unique[i])[-which(names(factor_data) %in% Na_unique_merge_list)]
  reduce_list[[i]] <- data_file_reduced
}

dim(reduce_list)

dim(data_file_reduced)


row_bind=function(x,y){rbind(x,y)}
reduce_file <- Reduce(row_bind, reduce_list)

dim(reduce_file)

reduce_file <- na.omit(reduce_file)
reduce_file <- reduce_file[,-5]
reduce_file <- reduce_file[,-3]
reduce_file <- reduce_file[,-4]
reduce_file <- reduce_file[,-4]
ticker_dates <- reduce_file[,1:3]
#reduce_file <- reduce_file[,(1:3)]
reduce_file <- reduce_file[,-(3:1)]
reduce_file <- apply(reduce_file, 2, scale)
reduce_file <- data.frame(reduce_file)
reduce_file <- cbind(ticker_dates, reduce_file)

View(reduce_file)
l <- ls()
l <- l[l!="reduce_file"]
rm(list=l)
rm(l)

reduce_list_modified <- NULL
unique_tickers <- unique(reduce_file$ticker)
for(u in unique_tickers){
  temp <- subset(reduce_file,ticker==u)
  if(dim(temp)[1]==20){
    reduce_list_modified <- rbind(reduce_list_modified,temp)
  }
  temp<-NULL
}

factors <- c('calendardate','revenue','cor','rnd','ebit','netinc','epsdil','ncfo','ncfi','capex',
             'ncfdiv','intangibles','debt','de','pe','pb','fcfps','assets','netmargin','marketcap','dps','ln_returns')


reduce_file_original <- reduce_file
reduce_file <- reduce_list_modified
#reduce_file <- apply(reduce_file, 2, scale)
reduce_list_modified <- NULL

unique_dates <- unique(reduce_file$calendardate)
unique_dates_train <- unique_dates[1:15]
unique_dates_test <- unique_dates[16:20]


reduce_file_data <- vector()

for(i in factors){
  reduce_file_data <- cbind(reduce_file_data,reduce_file[,i])
}

reduce_file_data <- data.frame(reduce_file_data)
#reduce_file <- apply(reduce_file, 2, scale)
reduce_file_data <- cbind(reduce_file[,'ticker'],reduce_file_data)

colnames(reduce_file_data) <- c('ticker',factors)

coefficients_dates <- vector()

for(j in unique_dates_train){
  temp <- subset(reduce_file_data,calendardate==j)
  temp[c('calendardate','ticker')] <- list(NULL)
  model <- lm(ln_returns ~ . , temp)
  #browser()
  coefficients_dates <- rbind(coefficients_dates,model$coefficients)
  temp <- NULL
  model <- NULL
}

coefficients_dates <- cbind(unique_dates_train,coefficients_dates)

tickers <- read.csv("E:/Google Drive/Fall 17 Sem 2/Academic Projects/Predictive and Financial Modelling/Predicting Stock Returns/tickers.csv",header = TRUE)
t_tickers <- intersect(tickers$Symbol,reduce_file$ticker)
reduce_file_tickers_data <- vector()
for(i in t_tickers){
  reduce_file_tickers_data <- rbind(reduce_file_tickers_data,reduce_file_data[reduce_file_data$ticker==i,])
}

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