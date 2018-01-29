install.packages("randomForest")
library(datasets)
library(randomForest)

head(iris)
set.seed(100)
iris.split <- sample(2,nrow(iris),replace = TRUE,prob = c(2/3,1/3))

iris.train <- iris[iris.split == 1 ,]
iris.test <- iris[iris.split == 2 ,]

set.seed(1210)
iris.rf <- randomForest(Species ~ . ,
                        data = iris.train,
                        ntree = 500,
                        proximity = TRUE)
print(iris.rf)
plot(iris.rf)
importance(iris.rf)
varImpPlot(iris.rf)


#use the model on testing data

iris.pred <- predict(iris.rf , newdata = iris.test)
table(iris.pred, iris.test$Species)
