  ###### Setting up Environment ######
  
  data_cancer <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"),header = FALSE)
  colnames(data_cancer)<-c('ID_number','Diagnosis','radius_mean','texture_mean','perimeter_mean','area_mean','smoothness_mean','compactness_mean'
  ,'concavity_mean','concave_points_mean','symmetry_mean','fractal_dimension_mean','radius_SE','texture_SE','perimeter_SE','area_SE',
  'smoothness_SE','compactness_SE','concavity_SE','concave_points_SE','symmetry_SE','fractal_dimension_SE','radius_worst','texture_worst',
  'perimeter_worst','area_worst','smoothness_worst','compactness_worst','concavity_worst','concave_points_worst','symmetry_worst','fractal_dimension_worst')
  
  
  #data_cancer <- data_cancer[order(data_cancer$area_SE),]
  
  #### Randomizing the data ####
  # uncomment below code for randomly selecting the data
  
  set.seed(11)
  random<- rnorm(nrow(data_cancer))
  data_cancer <- cbind(random, data_cancer)
  data_cancer <- data_cancer[order(data_cancer$random),]
  data_cancer$random <- NULL

  # uncomment above code for randomly selecting the data
  
  #temp <- data_cancer$Diagnosis
  #data_cancer$Diagnosis<- NULL
  options(warn=-1)
  data_cancer$ID_number <- NULL
  data_cancer <- na.omit(data_cancer)
  #data_cancer <- apply(data_cancer, 2, scale)
  #data_cancer <- cbind(temp,data_cancer)
  
  
  #data_cancer <- data_cancer[,1:11]
  #data_cancer <- data_cancer[,c(1,2,4,5,8)]
  data_cancer_train <- data_cancer[1:floor(nrow(data_cancer) * 0.75),]
  data_cancer_test <- data_cancer[ceiling(nrow(data_cancer) * 0.75):nrow(data_cancer),]
  
  #### GLM ####
  
  logmodel <- glm(Diagnosis~.,family = binomial(),data=data_cancer_train)
  summary(logmodel)
  
  
  ##testing
  test_glm <- predict.glm(logmodel,data_cancer_test,type='response')
  test_prediction_glm <- ifelse(test_glm > 0.5, "M","B" )
  confusion_matrix_glm <- table(test_prediction_glm,data_cancer_test$Diagnosis,dnn=c('Predicted','Actual'))
  accuracy_glm <- ((confusion_matrix_glm[1,1] + confusion_matrix_glm[2,2])/sum(confusion_matrix_glm))*100
  accuracy_glm
  
  
  #### LDA ####
  #install.packages("MASS")
  library("MASS")
  
  ldamodel <- lda(Diagnosis~.,data=data_cancer_train)
  summary(ldamodel)
  
  pred_lda <- predict(ldamodel,data_cancer_test)
  confusion_matrix_lda <- table(pred_lda$class,data_cancer_test$Diagnosis,dnn=c('Predicted','Actual'))
  accuracy_lda <- ((confusion_matrix_lda[1,1] + confusion_matrix_lda[2,2])/sum(confusion_matrix_lda))*100
  accuracy_lda
  
  #### SVM ####
  #install.packages("e1071")
  library("e1071")
  
  
  model_svm <- svm(Diagnosis ~ . , data= data_cancer_train)
  summary(model_svm)
  pred_svm <- predict(model_svm)
  

  ##testing
  test_svm <- predict(model_svm,data_cancer_test)
  confusion_matrix_svm <- table(test_svm,data_cancer_test$Diagnosis,dnn = c('predicted','actual'))
  accuracy_svm <- ((confusion_matrix_svm[1,1] + confusion_matrix_svm[2,2])/sum(confusion_matrix_svm))*100
  accuracy_svm
  
  #### KNN ####
  
  #install.packages("class")
  library("class")
  data_cancer_copy <- data_cancer
  data_cancer_copy$Diagnosis <- ifelse(data_cancer_copy$Diagnosis == 'M', 1 , 0)
  data_cancer_train_knn <- data_cancer_copy[1:floor(nrow(data_cancer_copy) * 0.75),]
  data_cancer_test_knn <- data_cancer_copy[ceiling(nrow(data_cancer_copy) * 0.75):nrow(data_cancer_copy),]
  
  prev_accuracy_knn <- 0
  k_value <- 1
  
  repeat{
  model_knn <- knn(data_cancer_train_knn, data_cancer_test_knn, data_cancer_train_knn$Diagnosis, k = k_value)
  summary(model_knn) 
  confusion_matrix_knn <- table(model_knn,data_cancer_test$Diagnosis,dnn = c('prediceted','actual'))
  confusion_matrix_knn
  accuracy_knn <- ((confusion_matrix_knn[1,1] + confusion_matrix_knn[2,2])/sum(confusion_matrix_knn)) * 100
  print(accuracy_knn)
  print(paste('At k =',k_value,'accuracy is',accuracy_knn))
  
  if(prev_accuracy_knn >= accuracy_knn){
    accuracy_knn <- prev_accuracy_knn
    k_value <- k_value - 2
    break
  }
  prev_accuracy_knn <- NULL
  prev_accuracy_knn <- accuracy_knn
  k_value <- k_value + 2
  }
  
  accuracy <- data.frame()
  accuracy <- cbind(accuracy_glm,accuracy_lda,accuracy_svm,accuracy_knn)
  colnames(accuracy) <- c("glm","lda","svm","knn")
  print(accuracy)

  