############## Problem 1 ################
problem1 <- function(){
  K = 100
  r = .015
  C = 0
  mu = 0.05
  sigma = sqrt(.15)
  N = 10000
  T = 0.5
  S = vector()
  ln_S_T = 0
  S_0 = 110
  
  for (i in 1:10000){
    ln_S_T = rnorm(1,mean = (log(S_0) + (mu - (sigma*sigma/2)) * T ), sd = sigma*sqrt(T))
    S_T = exp(ln_S_T)
    C = C + max(K - S_T, 0)
  }
  return(exp(-r*T)*C/N)
}

data <- data.frame()
for(i in 1:100){
  set.seed(i)
  t <- system.time(s <- problem1())[3]
  data <- rbind(data,c(i,t,s))
}
colnames(data) <- c("seed","Time","Premium")
write.csv(data, file = "problem1.csv")