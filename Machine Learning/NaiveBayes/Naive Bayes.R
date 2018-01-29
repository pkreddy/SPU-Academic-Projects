install.packages("e1071")
library(e1071)
library(mlbench)
library(datasets)

data(HouseVotes84 , package = "mlbench")
vote <- HouseVotes84

head(vote)
summary(vote)

set.seed(1231)
vote.split <- sample(2,nrow(vote),
                     replace = TRUE,
                     prob = c(2/3,1/3))

vote.train <- vote[vote.split == 1 ,]
vote.test <- vote[vote.split == 2, ]


nbc <- naiveBayes(Class ~ . , data=vote.train)
nbc

table(predict(nbc,vote.train[,-1]),vote.train[,1])
round(prop.table(table(predict(nbc,vote.train[,-1]),
                       vote.train[,1]),
                 1),2) * 100

table(predict(nbc,vote.test[,-1]),vote.test[,1])
round(prop.table(table(predict(nbc,vote.test[,-1]),
                       vote.test[,1]),
                 1),2) * 100
