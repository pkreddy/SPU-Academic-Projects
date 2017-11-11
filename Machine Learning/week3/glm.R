data_cancer <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"),header = FALSE)
colnames(data_cancer)<-c('ID_number','Diagnosis','radius_mean','texture_mean','perimeter_mean','area_mean','smoothness_mean','compactness_mean'
,'concavity_mean','concave_points_mean','symmetry_mean','fractal_dimension_mean','radius_SE','texture_SE','perimeter_SE','area_SE',
'smoothness_SE','compactness_SE','concavity_SE','concave_points_SE','symmetry_SE','fractal_dimension_SE','radius_worst','texture_worst',
'perimeter_worst','area_worst','smoothness_worst','compactness_worst','concavity_worst','concave_points_worst','symmetry_worst','fractal_dimension_worst')

#Diagnosis_1 <- factor(data_cancer$Diagnosis,levels=c('B','M'),labels=c(0,1))
#as.integer(as.character(Diagnosis_1))
#data_cancer <- cbind(Diagnosis_1,data_cancer)
#data_cancer$Diagnosis <- NULL

set.seed(16)
random<- rnorm(nrow(data_cancer))
data_cancer <- cbind(random, data_cancer)
data_cancer <- data_cancer[order(data_cancer$random),]
data_cancer$random <- NULL

data_cancer$ID_number <- NULL

data_cancer_train <- data_cancer[1:floor(nrow(data_cancer) * 0.75),]
data_cancer_test <- data_cancer[ceiling(nrow(data_cancer) * 0.75):nrow(data_cancer),]


logmodel <- glm(Diagnosis~.,family = binomial(),data=data_cancer_train)
summary(logmodel)


pred <- predict.glm(logmodel,data_cancer_train,type='response')
prediction <- ifelse(pred > 0.9, 'M', 'B')
#confusion  <- table(prediction, as.logical(logmodel$y))

##testing
test <- predict.glm(logmodel,data_cancer_test,type='response')
#test_prediction <- ifelse(test > 0.5, TRUE, FALSE)
test_prediction <- ifelse(test > 0.5, "M","B" )
#test_actual <- ifelse(data_cancer_test$Diagnosis == 'B', FALSE , TRUE)
#t <- table(test_prediction,as.logical(test_actual))
a <- table(test_prediction,data_cancer_test$Diagnosis,dnn=c('Predicted','Actual'))
a
accuracy <- ((a[1,1] + a[2,2])/sum(a))*100
print(accuracy)