library(datasets)
library(party)
head(iris)
summary(iris)
iris.ct <- ctree(Species ~ . , data = iris)
iris.ct
plot(iris.ct)
table(predict(iris.ct),iris$Species)
