install.packages("class")
library(class)
library(datasets)

head(iris)
summary(iris)

set.seed(1212)
iris.split <- sample(2,nrow(iris),
                     replace = TRUE,
                     prob = c(2/3,1/3))

iris.train <- iris[iris.split == 1, 1:4]
iris.test <- iris[iris.split == 2, 1:4]

#create species label
iris.train.labels <- iris[iris.split == 1, 5]
iris.test.labels <- iris[iris.split == 2,5]

iris.pred <- knn(train = iris.train,
                 test = iris.test,
                 cl = iris.train.labels,
                 k=3)

table(iris.pred,iris.test.labels)

