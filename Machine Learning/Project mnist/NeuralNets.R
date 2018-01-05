View(iris)
library("neuralnet")
data_iris <- iris[,-5]
set.seed(1)
model_outputs <- model.matrix(~Species+0,iris)
data_iris <- cbind(data_iris, model_outputs)
n <- names(data_iris)
dependednt <- paste(n[5:7],collapse = "+")
independent <- paste(n[!(n %in% n[5:7])],collapse = "+")
f <- as.formula(paste(dependednt," ~ ", independent))

nn_model <- neuralnet(f,data = data_iris,hidden = c(3,2,3), linear.output = FALSE)
plot(nn_model)
nn_predicted <- compute(nn_model,data_iris[,1:4])

