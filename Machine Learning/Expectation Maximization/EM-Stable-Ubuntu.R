library(MASS)
library(mvtnorm)
#convergence limit calculated in this abs((newlikelihood - LogsumLoglikelihood_old )/LogsumLoglikelihood_old) * 100
#percentage decrease with each iteration
#if the percentage limit is less than 0.01 with any iteration EM exits with convergence limit
convergence_limit <- 0.001

max_iterations <- 20 
#K value to start with
start_k_value <- 9 
#(1 is default)

#No of K's to run for
end_k_value <- 25

#reading csv 
data_file <- read.csv('cluster.csv',header = FALSE,numerals = "no.loss")

#########STARTING DATA EXPLORATION##########
#View(data_file)
### ALL FEATURES ARE NUMERIC NO CLASS VARIABLE OR LABEL, FIRST COLUMN IS THE ID 

### STEP1 EXPLOR THE DATA #############

summary(data_file)

##NOTICING THE PRESENCE OF OUTLIERS AND NA'S########

x_col = data_file[,-1] #REMOVING THE FIRST COLUMN
head(x_col)
########### removing NA'S#########
x_col <- na.omit(x_col)
x_col <- x_col[complete.cases(x_col),]
dim(x_col)


#removing outliers
x_col <- subset(x_col, x_col$V6 >=2 & x_col$V6<=5.5)
dim(x_col)
x_col <- subset(x_col, x_col$V9 >=1.5 & x_col$V9<=5)
dim(x_col)
x_col <- subset(x_col, x_col$V11 >=0.8 & x_col$V11<=2)
dim(x_col)
x_col <- subset(x_col, x_col$V12<=1.5)
dim(x_col)

#View(x_col)


###################kmeans##############
########## function to scale our data. we defined it and put it into a variable called standardize###################
standardize <- function(x) {
  return ((x - mean(x)) / sd(x))  }
###We only scale the domain and not the range.after removing the first variable id,now our first variable in prc
#data is diagnosis_result we skeepit and go tothe secong variable till 31 excludind the last 2 column as well
#that is diagnosis and diag_Num
x_col <- as.data.frame(lapply(x_col[1:70],standardize))
summary(x_col$V5) #checking
set.seed(1234)

# finding cluster covariance during initialization
cluster_covariance<-function(datak,meank){
  sumk<-0
  #data <-data[,2:71]
  #mean <-mean[2:71]
  for(i in 1:(dim(datak)[1])){
    sumk <- sumk + (datak[i,]-meank) * (t(datak[i,])-meank)
  }
  return (sumk/dim(datak)[1])
}

#log_likelihood during initialization
log_likelhood <- function(datak,pik,meank,covariancek){
  sum <- 0
  #browser()
  for(i in 1:dim(datak)[1]){
    normk <- dmvnorm(as.matrix(datak[i,]),mean = as.matrix(meank[]),sigma = diag(covariancek))
    sum <- sum + normk * pik
  }
  #browser()
  return (sum)
}


#numerator of the gamma during initialization
gamma_numerator <- function(datak,pik,meank,covariancek){
  norm_numerator <- NULL
  for(i in 1:dim(datak)[1]){
    normk <- dmvnorm(as.matrix(datak[i,]),mean = as.matrix(meank[]),sigma = diag(covariancek))
    normk <- normk * pik
    norm_numerator <- rbind(norm_numerator,normk)
  }
  return (norm_numerator)
}

#denominator of gamma initialization
gamma_denominator <- function(datak,pik,meank,covariancek){
  norm_denominator <- NULL
  sumk <- 0
  for(i in 1:dim(datak)[1]){
    normk <- dmvnorm(as.matrix(datak[i,]),mean = as.matrix(meank[]),sigma = diag(covariancek))
    normk <- normk * pik
    #norm_denominator <- rbind(norm_denominator,normk)
    sumk <- sumk + normk
  }
  return (sumk)
}


init_likelihood <- function(){
  sumk <- sumlog <- 0
  #data_ll <- newdf[,2:71]
  #browser()
  for(j in 1:dim(newdf)[1]){
    #take individual data points
    data_ll <- newdf[j,2:71]
    sumk <- 0
    for(i in 1:k){
      mean_ll <- get(paste("mean",i,sep = ""))
      cov_ll <- get(paste("cov",i,sep = ""))
      pi_ll <- get(paste("pi",i,sep = ""))
      
      sumk <- sumk + pi_ll  * dmvnorm(as.matrix(data_ll),mean = as.matrix(mean_ll[]),sigma = diag(cov_ll))
    }
      sumlog <- sumlog + log(sumk)
  }
    return(sumlog)
}


#function to find new_means for a given cluster after initialization
find_new_means <- function(data,gammak){
  sumk <- 0
  for(i in 1:dim(data)[1]){
    sumk <- sumk + data[i,] * gammak[i]
  }
  return (sumk)
}
#function to find new covariance for a given cluster after initialization
find_new_covariance <- function(gammak,datak,meank,Nk){
  sumk <- 0
  for(i in 1:dim(datak)[1]){
    sumk <- sumk + gammak[i] * (datak[i,]-meank) * (t(datak[i,]-meank))
  } 
  return (sumk/Nk)
}

#function to find new Nk's for a given cluster after initialization
find_new_Nk <- function(gammak){
  sumk <- 0
  for(i in 1:k){
    sumk <- sumk + gammak[i]
  }
  return(sumk)
}

#function to find new pi's for a given cluster after initialization
new_pi <- function(datak,data){
  return(dim(datak)[1]/dim(data)[1])
}

find_gamma <- function(i){
  numerator <- get(paste("numerator",i,sep = ""))
  gammak <- NULL
  #browser()
  for(j in 1:dim(newdf)[1]){
    sum <- 0
    for(m in 1:k){
      temp <- get(paste("numerator",m,sep = ""))
      sum <- sum + temp[j]
    }
    gammak <- rbind(gammak,numerator[j]/sum)
  }
  return (gammak)
}


#Initialization starts here
sumLoglikelihood <- 0

#function to find new gamma for a given cluster after initialization
find_new_gamma <- function(){
  for(i in 1:k){
    numerator_name <- paste("numerator_new",i,sep = "")
    pik <- get(paste("new_pi",i,sep = ""))
    meank <- get(paste("new_mean",i,sep = ""))
    covariancek <- get(paste("new_covariance",i,sep = ""))
    clusterk <- get(paste("cluster",i,sep = ""))
    numerator <- gamma_numerator(newdf[,2:71],pik,meank,covariancek)
    assign(numerator_name,numerator,envir = .GlobalEnv)
  }
  for(i in 1:k){
    numerator <- get(paste("numerator_new",i,sep = ""))
    gamma_new_name <- paste("gamma_new",i,sep = "")
    gammak <- NULL
    for(j in 1:dim(newdf)[1]){
      sum <- 0
      for(m in 1:k){
        temp <- get(paste("numerator_new",m,sep = ""))
        sum <- sum + temp[j]
      }
      gammak <- rbind(gammak,numerator[j]/sum)
    }
    assign(gamma_new_name,gammak,envir = .GlobalEnv)
  }    
}

#clears all the new values that have to computed
clearValues <- function(){
  for(i in 1:k){
    pik <- (paste("new_pi",i,sep = ""))
    meank <- (paste("new_mean",i,sep = ""))
    covariancek <- (paste("new_covariance",i,sep = ""))
    
    assign(pik,NULL,envir = .GlobalEnv)
    assign(meank,NULL,envir = .GlobalEnv)
    assign(covariancek,NULL,envir = .GlobalEnv)
  }
}

#function to find new_values for a given cluster after initialization
compute_new_values <- function(){
  for(i in 1:k){
  gamma_value <- get(paste("gamma_new",i,sep = ""))
  
  #getting Nk's 
  N_names <- paste("N",i,sep = "")
  #N_value <- gamma_value * dim(newdf)[1]
  N_value <- sum(gamma_value)
  assign(N_names,N_value,envir = .GlobalEnv)
  
  #new means
  new_means_names <- paste("new_mean",i,sep = "")
  means_value <- find_new_means(newdf[,2:71],gamma_value)/get(N_names)
  assign(new_means_names,means_value,envir = .GlobalEnv)
  
  #new covariance
  new_covariance_names <- paste("new_covariance",i,sep = "")
  assign(new_covariance_names,find_new_covariance(gamma_value,newdf[,2:71],means_value,N_value),envir = .GlobalEnv)
  
  #new pis
  new_pi_names <- paste("new_pi",i,sep = "")
  pi_value <- N_value/dim(newdf)[1]
  assign(new_pi_names,pi_value,envir = .GlobalEnv)
  }
}

#function to find new likelihood with new means, covarinace, pi's
new_likelihood <- function(){
  sumk <- sumlog <- 0
  #data_ll <- newdf[,2:71]
  #browser()
  for(j in 1:dim(newdf)[1]){
    #take individual data points
    data_ll <- newdf[j,2:71]
    sumk <- 0
    for(i in 1:k){
      mean_ll <- get(paste("new_mean",i,sep = ""))
      cov_ll <- get(paste("new_covariance",i,sep = ""))
      pi_ll <- get(paste("new_pi",i,sep = ""))
      
      sumk <- sumk + pi_ll  * dmvnorm(as.matrix(data_ll),mean = as.matrix(mean_ll[]),sigma = diag(cov_ll))
    }
    sumlog <- sumlog + log(sumk)
  }
  return(sumlog)
}

#Iterating through given K values

for(iteration in start_k_value:end_k_value){

k <- iteration

print(paste("running K-Means with K value -",k))

clust <- kmeans(x_col,centers = k,iter.max = 10)
#clust
################8888888##############################
newdf<-cbind(clust$cluster,x_col)
#View(newdf)

#Execution starts from here

for(i in 1:k){
  #create k clusters
  cluster_name <- paste("cluster",i,sep = "")
  a <- subset(newdf,clust$cluster==i)
  a <- a[,2:71]
  assign(cluster_name,a)
  #create k data_frames with each mean of Kth cluster in it
  mean_name <- paste("mean",i,sep = "")
  b <- apply(a, 2, mean)
  assign(mean_name,b)
  #create k data_frames with each pi value of Kth cluster in it
  pi_name <- paste("pi",i,sep = "")
  c <- dim(a)[1]/dim(newdf)[1]
  assign(pi_name,c)
  #create k data_frames with each co variance of Kth cluster in it
  cov_name <- paste("cov",i,sep = "")
  #a is data in kth cluster and b is mean of the kth cluster
  d <- cluster_covariance(a,b)
  assign(cov_name,d)
  #sumLoglikelihood <- sumLoglikelihood + log_likelhood(newdf[,2:71],c,b,d)
  #sumLoglikelihood <- sumLoglikelihood + log_likelhood(a,c,b,d)
  numerator_name <- paste("numerator",i,sep = "")
  #assign(numerator_name,gamma_numerator(newdf[,2:71],c,b,d))
  assign(numerator_name,gamma_numerator(newdf[,2:71],c,b,d))
  #calculate gamma denominators for each k value
  #denominator_name <- paste("denominator",i,sep = "")
  #assign(denominator_name , gamma_denominator(newdf[,2:71],c,b,d))
}

#LogsumLogLikelihood_old contains likelihood of initilization
LogsumLoglikelihood_old <- init_likelihood()

for(i in 1:k){
  #getting up gamma values
  #browser()
  #find gammas for the initialized values
  gamma_names <- paste("gamma",i,sep = "")
  nums <- paste("numerator",i,sep = "")
  denoms <- paste("denominator",i,sep = "")
  #gamma_value <- get(nums)/get(denoms)
  gamma_value <- find_gamma(i)
  assign(gamma_names,gamma_value)
  
  #getting Nk's 
  N_names <- paste("N",i,sep = "")
  #N_value <- gamma_value * dim(newdf)[1]
  #N_value <- find_new_Nk(gamma_value)
  N_value <- sum(gamma_value)
  assign(N_names,N_value)
  cluster_value <- get(paste("cluster",i,sep = ""))
  #new means
  new_means_names <- paste("new_mean",i,sep = "")
  #means_value <- find_new_means(newdf[,2:71],gamma_value)/get(N_names)
  means_value <- find_new_means(cluster_value,gamma_value)/get(N_names)
  assign(new_means_names,means_value)
  
  #new covariance
  new_covariance_names <- paste("new_covariance",i,sep = "")
  assign(new_covariance_names,find_new_covariance(gamma_value,newdf[,2:71],means_value,N_value))

  #new pis
  new_pi_names <- paste("new_pi",i,sep = "")
  pi_value <- N_value/dim(newdf)[1]
  assign(new_pi_names,pi_value)
}


newlikelihood <- 0
count <- 0

#setting gamma_new to null
for(i in 1:k){
  gamma_new_name <- paste("gamma_new",i,sep = "")
  assign(gamma_new_name,NULL)
}

#EM starts here
repeat{
  #Evaluate log likelihood
  newlikelihood <- new_likelihood()
  
  print(paste("new log likelihood for iteration",count + 1,"is",newlikelihood))
  print(paste("old log likelihood is",LogsumLoglikelihood_old))
  
  #convergence <- abs(newlikelihood / LogsumLoglikelihood_old)
    convergence <- abs((newlikelihood - LogsumLoglikelihood_old )/LogsumLoglikelihood_old) * 100
  if(convergence <= convergence_limit || max_iterations < count) {
    if(max_iterations < count){
      print("max iterations reached")  
    }else{
      print("convergence limit reached")
      print(paste("percentage decreased value",abs((newlikelihood - LogsumLoglikelihood_old )/LogsumLoglikelihood_old) * 100))
    }
    break
  }
  
  print(paste("abs((newlikelihood - LogsumLoglikelihood_old )/LogsumLoglikelihood_old) * 100 =",
              abs((newlikelihood - LogsumLoglikelihood_old )/LogsumLoglikelihood_old) * 100,
              "is greater than convergence limit",convergence_limit))
  count <- count + 1

  #Expectation Step	
  #finds_new_gammas using new mean, covariance, pi
  find_new_gamma()
  
  clearValues()
  
  #Maximization Step
  # find new mean, covariance, pi and Nk
  compute_new_values()
  
  #copying the new likelihood to old likelihood
  LogsumLoglikelihood_old <- newlikelihood
}
}
