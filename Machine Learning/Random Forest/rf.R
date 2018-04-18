install.packages("randomForest")
library(datasets)
library(randomForest)

iris <- read.csv(file = "C:\\Users\\prana\\Desktop\\loadsmart\\flowers.csv", header = TRUE)


head(iris)
set.seed(2)
iris.split <- sample(2,nrow(iris),replace = TRUE,prob = c(2/3,1/3))

iris.train <- iris[iris.split == 1 ,]
iris.test <- iris[iris.split == 2 ,]

set.seed(1)
iris.rf <- randomForest(species ~ data$petal.length+data$petal.width ,
                        data = iris.train,
                        ntree = 0,
                        proximity = TRUE)
print(iris.rf)
plot(iris.rf)
importance(iris.rf)
varImpPlot(iris.rf)

#use the model training data
iris.pred <- predict(iris.rf , newdata = iris.train)
table(iris.pred, iris.train$species)
(sum(iris.pred==iris.train$species)/length(iris.pred))*100


#use the model on testing data
iris.pred <- predict(iris.rf , newdata = iris.test)
table(iris.pred, iris.test$species)
(sum(iris.pred==iris.test$species)/length(iris.pred))*100


#use the model on entire data set
iris.pred <- predict(iris.rf, newdata = iris)
table(iris.pred, iris$species)
(sum(iris.pred==iris$species)/length(iris.pred))*100