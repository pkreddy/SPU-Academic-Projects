coefficients_train = read.csv(file="E:/Google Drive/Fall 17 Sem 2/Academic Projects/Predictive and Financial Modelling/Predicting Stock Returns/coefficients_train.csv")
coefficients_train <- coefficients_train[,c(-1)]
coefficients_train <- coefficients_train[order(coefficients_train$unique_dates_train),]

coefficients_all = read.csv(file="E:/Google Drive/Fall 17 Sem 2/Academic Projects/Predictive and Financial Modelling/Predicting Stock Returns/coefficients_all.csv")
coefficients_all <- coefficients_all[,c(-1)]
coefficients_all <- coefficients_all[order(coefficients_all$unique_dates),]

