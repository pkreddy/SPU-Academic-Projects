# from slides
y=geneq(x[t])# assume this is geenrator for q(y|x)
if(runif(1)<f(y)*q(y,x[t])/f(x[t])*q(x[t],y)){
  x[t+1]=y} # accept
else{
  x[t+1]=x[t] #reject
}

#example 6.1 from book
#initial values
a=2.7
b=6.3
c=2.669
#use uniform 0,1 because beta runs on same domain 0,1. so helps 

N= 5000
X=rep(runif(1), N) #initialize the chain
for(i in 2:N){
  Y=runif(1) # sample from Y
  rho=dbeta(Y,a,b)/dbeta(X[i-1], a,b) # this is f(y)/f(x) and because q is Uniform 0,1 the q portion is = 1/1 - probability of accepting
  X[i]=X[i-1] + (Y-X[i-1])*(runif(1)<rho) # if true then equal to 1 and =Y, if false then 0 and = to last value in chain
}

plot(X,type = "l") # samping from X, which is y-axis 0 to 1
plot(X[2971:3171], type = "l") # zoom in to 200 samples
# we see multiple flats where the chain rejected
#does it look like a beta density?
hist(X,breaks = 40, probability = TRUE) # and it does
#superimpose the target density Beta(2.7,6.3) to verify how good
x<-seq(0,1, by=.01)
y<-dbeta(x,a,b)
lines(x,y)
