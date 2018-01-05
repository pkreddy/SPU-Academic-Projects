mnist_data <- read.csv("E:\\Google Drive\\Fall 17 Sem 2\\Academic Projects\\Machine Learning\\week4\\mnist.csv")
head(mnist_data)


# numberzero <- as.matrix(mnist_data[1,])
# numberzero <- numberzero[,-1]
# View(numberzero)
# length(numberzero)
# zeromatrix <- matrix(numberzero,nrow=28,ncol=28)
# image(zeromatrix)


#remove first column
mnist_features <-mnist_data[,-1]

#Question 1
prcomp_model <- prcomp(mnist_features)
summary(prcomp_model)

#261 proportional components has 98.014% of variance of original dataset

#Question 2
prop <- as.data.frame(summary(prcomp_model)$importance)
proportion_variance <- prop[2,(1:261)]


#Question 3
loadings <- as.data.frame(prcomp_model$rotation[,(1:261)])
loadings <- abs(loadings)
max_loading <- row.names(loadings)[(max.col(loadings))]
count_loading <- as.data.frame(table(max_loading))

#Question 4
#Pixel 80 is the one with the most presence with 20 occurences of having the most significant loading
head(count_loading[rev(order(count_loading$Freq)),])

#BONUS
projected <- scale(mnist_features, prcomp_model$center, prcomp_model$scale) %*% prcomp_model$rotation

pc_vals<- as.data.frame(projected[,1:261]%*%t(prcomp_model$rotation)[1:261,])

##Plotting 10 PCs observations
for (i in 1:10)
{
  PC <- as.matrix(pc_vals[i,])
  PCmatrix <- matrix(PC, ncol = 28, nrow = 28)
  image(PCmatrix)
  title(paste("PC", as.character(i)))
}