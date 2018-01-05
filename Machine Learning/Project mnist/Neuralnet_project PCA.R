library(neuralnet)
library(caret)
options(max.print=1000000)

# load data
mnist<- read.csv('E:/Google Drive/Fall 17 Sem 2/Academic Projects/Machine Learning/Project 2/mnist.csv')
str(mnist)
mnist[,'X5']<-factor(mnist[,'X5'])
str(mnist)

#create data matrix
model_outputs <- as.data.frame(model.matrix(~X5+0, mnist))
View(model_outputs)

#Create the folds
k=6
num <- mnist[,1]
folds <- createFolds(num, k = 6)

# For PCA
#get features only
mnist_features <-mnist[,-1]
prcomp_model <- prcomp(mnist_features)
summary(prcomp_model)

# Calculate PCA Vals for 95% variance at 154 PCs
new_data<- as.data.frame(prcomp_model$x[, 1:154])
new_modeldata <- as.data.frame(cbind(model_outputs, new_data))

# new formula
n_pca<- names(new_modeldata) # column names
dependent_pca <- paste(n_pca[1:10], collapse = "+")
independent_pca <- paste(n_pca[!n_pca %in% n_pca[1:10]], collapse = "+")
f_pca <- as.formula(paste(dependent_pca, "~", independent_pca))
f_pca

#Model
system.time(
    for (i in 1:k){
      testingFold <- folds[[i]]
      cvTrain_pca <- new_modeldata[-testingFold,]
      cvTrainLabel_pca <- model_outputs[-testingFold,]
      cvTestLabel_pca <- model_outputs[testingFold,]
      cvTest_pca <- new_modeldata[testingFold,]
      nn_model_pca <- neuralnet(f_pca, data = cvTrain_pca, hidden = c(150, 70), stepmax = 5000, linear.output = FALSE)
      nn_predicted_pca <- compute(nn_model_pca, cvTrain_pca[, nn_model_pca$model.list$variables])
      nn_predicted_pca <- nn_predicted_pca$net.result
      original_values_pca <- max.col(cvTrainLabel_pca)
      nn_predicted_2_pca <- max.col(nn_predicted_pca)
      accuracy_pca[i] <- mean(nn_predicted_2_pca == original_values_pca)
    }
)

print(mean(accuracy_pca))