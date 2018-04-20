#6.10 a

N= 5000
X=rep(rnorm(1,0,1), N) #initialize the chain
n = 4
for(i in 2:N){
  Y=rnorm(1, 0, 1) # sample from Y
  rho=(dt(Y,df=n)/dt(X[i-1], df= n))*(dnorm(Y, 0,1)/dnorm(X[i-1],0,1)) # this is f(y)/f(x) and because q is Uniform 0,1 the q portion is = 1/1 - probability of accepting
  X[i]=X[i-1] + (Y-X[i-1])*(runif(1)<rho) # if true then equal to 1 and =Y, if false then 0 and = to last value in chain
}

plot(X,type = "l") # samping from X, which is y-axis 0 to 1
#does it look like a t density?
hist(X,breaks = 40, probability = TRUE) # and it does
#superimpose the target density Beta(2.7,6.3) to verify how good
x<-seq(-3,3, by=.01)
y<-dt(x,df=4)
lines(x,y)
mean(X)
#Mean of t-distibution is 0, oTherwise undefined. When calculating mean of all X, or accepted values 
#with candidate density N(0,1) for a target t-distribution with df = 4 mean is 0.007

#6.10b
N= 5000
X=rep(rt(1,2), N) #initialize the chain
n = 4
for(i in 2:N){
  Y=rt(1,2) # sample from Y
  rho=(dt(Y,df=n)/dt(X[i-1], df= n))*(dt(Y,2)/dt(X[i-1],2))# this is f(y)/f(x) and because q is Uniform 0,1 the q portion is = 1/1 - probability of accepting
  X[i]=X[i-1] + (Y-X[i-1])*(runif(1)<rho) # if true then equal to 1 and =Y, if false then 0 and = to last value in chain
}

plot(X,type = "l") # samping from X, which is y-axis 0 to 1
#does it look like a t density?
hist(X,breaks = 40, probability = TRUE, xlim = c(-4,4)) # and it does
#superimpose the target density Beta(2.7,6.3) to verify how good
x<-seq(-3,3, by=.01)
y<-dt(x,df=4)
lines(x,y)
mean(X)
#Mean of t-distibution is 0, otherwise undefined. When calculating mean of all X, or accepted values 
#with candidate denstity t and df = 2 for target t distribution and df = 4 mean is 0.117