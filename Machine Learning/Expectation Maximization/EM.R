k <- 2
data <- read.csv('E:\\Google Drive\\Fall 17 Sem 2\\630 Machine Learning\\Project-1\\cluster.csv',header = FALSE,numerals = "no.loss")
data <- data[,2:71]
#View(data)
data <- na.omit(data)
cluster <- kmeans(as.matrix(data),k,iter.max = 10)

center <- cluster$centers
data_cluster <- cbind(cluster$cluster,data)
names(data_cluster)[1]<-'CLUSTER'




gamma_Numerator <- function(Kdata,k,kth_Value){
  pi_k <- 1/k
  data_subset <- Kdata[which(Kdata$CLUSTER==kth_Value),]
  data_subset <- data_subset[,2:71]
  mean <- apply(data_subset,2,mean)
  variance <- apply(data_subset,2,var)
  a <- 1/sqrt(2*pi*(apply(data_subset,2,var)))
  exp_coeffs <- NULL
  
  for(i in 1:(dim(data_subset)[1])){
    b <- exp(-(((data_subset[i,]-mean)^2)/2*variance))
    exp_coeffs <- rbind(exp_coeffs,b)
  }
  numerator <- NULL
  
  for(j in 1:length(data_subset)){
    numerator <- cbind(numerator,exp_coeffs[,j]*a[j])
  }
  return (pi_k * numerator)
}

#Kdata - data_cluster, k - total k vlaue, kth_value

gammaK<-function(Kdata,k,kth_Value){
  numerator <- gamma_Numerator(Kdata,k,kth_Value)
  
}

B = matrix( 
     c(2, 4, 3, 1, 5, 7), 
     nrow=3, 
     ncol=2) 