################  problem 2  ######################
problem2 <- function(seed){
  count = 0
  K = 100
  r = .015
  C = 0
  mu = 0.15
  sigma = sqrt(.15)
  N = 10000
  T = 0.5
  S = vector()
  S[1] = 110
  dt = 0.5/126
  for(j in 1:N){
    for (i in 2:126){
      S[i] = S[i-1] + mu * S[i-1] * dt + sigma * S[i-1] * rnorm(1) * sqrt(dt)
     
      if(S[i] < 80){
        #S[i] == 0
        flag = 0
        break
      }else{
        flag = 1
      }
    }
    if(flag == 1){C = C + max(K - S[126], 0)
    count = count + 1}
  }
  #print(C/N)
  
  plot(S,type="l")
  dev.copy(jpeg,filename = paste("C:/Users/prana/Desktop/",seed,".jpeg" , sep = "") );
  dev.off();
  print(exp(-r*T)*C/N)
  print(exp(-r*T)*C/(count))
  return(exp(-r*T)*C/N)
}

data1 <- data.frame()
for(i in 1:2){
  set.seed(i)
  t <- system.time(d <- problem2(i))
  data1 <- rbind(data1,c(i,t[3],d))
}
colnames(data1) <- c("seed","t","Premium")
write.csv(data1,file="Problem_2.csv")
