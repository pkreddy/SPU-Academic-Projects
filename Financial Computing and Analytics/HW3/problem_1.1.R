################  problem 2  #######################
problem <- function(){
K = 100
r = 0.015
C = 0
mu = 0.05
sigma = sqrt(0.15)
N = 10000
T = 0.5
S = vector()
S[1] = 110
dt = 0.5/600
#set.seed(1)
for(j in 1:N){
  for (i in 2:600){
    S[i] = S[i-1] + mu * S[i-1] * dt + sigma * S[i-1] * rnorm(1) * sqrt(dt)
  }
  C = C + max(K - S[600], 0)
}

print(exp(-r*T)*C/N)

}

system.time(problem())
