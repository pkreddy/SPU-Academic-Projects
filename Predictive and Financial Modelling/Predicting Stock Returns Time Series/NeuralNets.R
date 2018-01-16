###Using Naive Prediction Model for weights##
set.seed(1234)
temp <- subset(reduce_file_tickers_data,calendardate==20140930)
temp[c('calendardate','ticker')] <- list(NULL)
n_factors <- c('revenue','cor','rnd','ebit','netinc','epsdil','ncfo','ncfi','capex','ncfdiv',
               'intangibles','debt','de','pe','pb','fcfps','assets','netmargin','marketcap','dps')
# Collapsing them together
n_factors <- paste(n_factors, collapse = "+")
formula_ <- as.formula(paste("ln_returns~", n_factors, collapse = "+"))

ln_returns_pred <- vector()

model <- neuralnet(formula = formula_, temp, c(10,5))
temp_1 <- subset(reduce_file_tickers_data,calendardate==20141231)
temp_1[c('calendardate','ticker')] <- list(NULL)
pred <- compute(model, temp_1[,1:20])
ln_returns_pred <- cbind(ln_returns_pred,temp$ln_returns,pred$net.result)

