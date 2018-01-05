data_cancer <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"),header = FALSE)
colnames(data_cancer)<-c('ID_number','Diagnosis','radius_mean','texture_mean','perimeter_mean','area_mean','smoothness_mean','compactness_mean'
,'concavity_mean','concave_points_mean','symmetry_mean','fractal_dimension_mean','radius_SE','texture_SE','perimeter_SE','area_SE',
'smoothness_SE','compactness_SE','concavity_SE','concave_points_SE','symmetry_SE','fractal_dimension_SE','radius_worst','texture_worst',
'perimeter_worst','area_worst','smoothness_worst','compactness_worst','concavity_worst','concave_points_worst','symmetry_worst','fractal_dimension_worst')

library("e1071")
#Diagnosis_1 <- factor(data_cancer$Diagnosis,levels=c('B','M'),labels=c(0,1))
#as.integer(as.character(Diagnosis_1))
#data_cancer <- cbind(Diagnosis_1,data_cancer)
#data_cancer$Diagnosis <- NULL
data_cancer$ID_number <- NULL

data_cancer_train <- data_cancer[1:floor(nrow(data_cancer) * 0.75),]
data_cancer_test <- data_cancer[ceiling(nrow(data_cancer) * 0.75):nrow(data_cancer),]

model <- svm(Diagnosis ~ . , data= data_cancer_train)
summary(model)
pred <- predict(model)
table(pred,data_cancer_train$Diagnosis)

plot(cmdscale(dist(data_cancer_train[,-1])),
     col = as.integer(data_cancer_train[,1]),
     pch = c("o","+")[1:426 %in% model$index + 1])

# pred <- predict(model, data_cancer_train[,2:31], decision.values = TRUE)
# attr(pred, "decision.values")[1:4,]

##testing
test <- predict(model,data_cancer_test)
a <- table(test,data_cancer_test$Diagnosis,dnn = c('predicted','actual'))
accuracy <- ((a[1,1] + a[2,2])/sum(a))*100
accuracy