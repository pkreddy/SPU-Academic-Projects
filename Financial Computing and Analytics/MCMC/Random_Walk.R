X <- vector()
t <- 10^6
X[1] = 0
for(i in 2:t){
  X[i] <- X[i-1] + rnorm(1,0,1)
}

plot(X,type="l")

#Helps to see whether randomwalk is converging
plot(cumsum(X),type="l")

# A stationary probability distribution exists by construction
# for those chains; that is, there exists a probability
# distribution f such that if X(t) ∼ f , then X(t + 1) ∼ f

#Analyzing the last values of X we can determine whether stationary probability
#exists for random walk

tail(X)

#By looking at the last values of X we can determine that 
#random walk is not a stationary distribution

# In a limiting distribution X(t) is f for almost any initial value X(0)
# But in our current randomwalk simulation
# we reach different X(t) value with the same initial value X(0)


#By looking at the last values of X(t) we can determine that
#random walk is not a limiting distribution

