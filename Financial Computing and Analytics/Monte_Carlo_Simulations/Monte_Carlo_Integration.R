h=function(x){(cos(50*x)+sin(20*x))^2}
par(mar=c(2,2,2,1),mfrow=c(2,1))
curve(h,xlab="Function",ylab="",lwd=2)
integrate(h,0,1)
#0.965201 with absolute error < 1.9e-10
x=h(runif(10^4))
estint=cumsum(x)/(1:10^4)
esterr=sqrt(cumsum((x-estint)^2))/(1:10^4)
plot(estint, xlab="Mean and error range",type="l",lwd=
         + 2,ylim=mean(x)+20*c(-esterr[10^4],esterr[10^4]),ylab="")
lines(estint+2*esterr,col="gold",lwd=2)
lines(estint-2*esterr,col="gold",lwd=2)

a <- function(x){(theta/1 + (theta)^2) * exp(-((x-theta)^2)/2)}

############## 3.1 a ################

a0 <- function(x){return ((x/(1 + (x)^2)) * exp(-(-x)^2/2))}
a2 <- function(x){return ((x/(1 + (x)^2)) * exp(-(2-x)^2/2))}
a4 <- function(x){return ((x/(1 + (x)^2)) * exp(-(4-x)^2/2))}

b0 <- function(x){return ((1/(1 + (x)^2)) * exp(-(-x)^2/2))}
b2 <- function(x){return ((1/(1 + (x)^2)) * exp(-(2-x)^2/2))}
b4 <- function(x){return ((1/(1 + (x)^2)) * exp(-(4-x)^2/2))}

par(mfrow = c(3,1))

plot(a0,xlim=c(-3,3),ylim=c(-1,1), main = "X = 0")
plot(b0,xlim=c(-3,3),add=TRUE,ylim=c(-1,1))

plot(a2,xlim=c(-3,5),ylim=c(-0.5,0.5),main = "X = 2")
plot(b2,xlim=c(-3,5),add=TRUE,ylim=c(-0.5,0.5))

plot(a4,xlim=c(-3,7),ylim=c(-0.25,0.25),main = "X = 4")
plot(b4,xlim=c(-3,7),add=TRUE,ylim=c(-0.25,0.25))

par(mfrow = c(1,1))

plot(rcauchy(10^4))

set.seed(123)

par(mfrow = c(3,3))
for(r in c(0,2,4)){
  Nsim <- 10^4
  cauch <- rcauchy(Nsim)
  cumsum_num <- cumsum(cauch * dnorm(cauch,mean = r))/(1:10^4)
  cumsum_denom <- cumsum(dnorm(cauch,mean = r))/(1:10^4)
  plot(cumsum_num,type = "l",main = paste("numerator with x",r))
  plot(cumsum_denom, type = "l",main = paste("denominator with x",r))
  print(cumsum_num[length(cumsum_num)]/cumsum_denom[length(cumsum_denom)])
}

# [1] 0.0006678213
# [1] 1.276915
# [1] 3.408064


###### 3.1 b ######

par(mfrow = c(3,3))
for(r in c(0,2,4)){
  Nsim <- 10^4
  cauch <- rcauchy(Nsim)
  cumsum_num_err <- sqrt(cumsum((cauch * dnorm(cauch,mean = r))^2))/(1:10^4)
  cumsum_denom_err <- (cumsum((dnorm(cauch,mean = r))^2))/(1:10^4)
  plot(cumsum_num_err,type = "l",main = paste("numerator with x",r))
  plot(cumsum_denom_err, type = "l",main = paste("denominator with x",r))
  print(paste("standerror with 0.95 probabilty for x value",r,"is",
    +cumsum_num_err[length(cumsum_num_err)]/cumsum_denom_err[length(cumsum_denom_err)]))
}

# [1] "standerror with 0.95 probabilty for x value 0 is 0.0215398394549276"
# [1] "standerror with 0.95 probabilty for x value 2 is 0.115831689937375"
# [1] "standerror with 0.95 probabilty for x value 4 is 0.488633976461582"

##### 3.1 c #######


set.seed(123)

par(mfrow = c(3,3))
for(r in c(0,2,4)){
  Nsim <- 10^4
  #cauch <- rcauchy(Nsim)
  cauch <- rnorm(Nsim,mean = r)
  cumsum_num <- cumsum(cauch * dcauchy(cauch))/(1:10^4)
  cumsum_denom <- cumsum(dcauchy(cauch))/(1:10^4)
  plot(cumsum_num,type = "l",main = paste("numerator with x",r))
  plot(cumsum_denom, type = "l",main = paste("denominator with x",r))
  print(cumsum_num[length(cumsum_num)]/cumsum_denom[length(cumsum_denom)])
}

# [1] -0.001183145
# [1] 1.273159
# [1] 3.43772

esterr <- sqrt(cumsum((x-estimate_fun)^2))/(1:10^4)

max(var(dnorm(co,m=x))*10^6,var(co*dnorm(co,m=x)))

ab0 <- integrate(a0,-Inf,Inf)$value / integrate(b0,-Inf,Inf)$value
ab2 <- integrate(a2,-Inf,Inf)$value / integrate(b2,-Inf,Inf)$value
ab4 <- integrate(a4,-Inf,Inf)$value / integrate(b4,-Inf,Inf)$value

abf0 <- function(x){return (a0(x)/ b0(x))}
abf2 <- function(x){return (a2(x)/ b2(x))}
abf4 <- function(x){return (a4(x)/ b4(x))}

Niter=10^4
co=rcauchy(Niter)
I=mean(co*dcauchy(co,mean=x))/mean(dnorm(co,mean=x))
#We thus get
x=0
mean(co*dcauchy(co,mean=x))/mean(dnorm(co,mean=x))
#0.01724
x=2
mean(co*dnorm(co,mean=x))/mean(dnorm(co,mean=x))
#[1] 1.295652
x=4
mean(co*dnorm(co,mean=x))/mean(dnorm(co,mean=x))
#[1] 3.107256


set.seed(123)
sim <- rcauchy(100)
x <- abf0(sim)
estimate_fun <- cumsum(x) /(1:100)
plot(estimate_fun,type="l",xlim = c(1,100),ylim = c(-10,5))
estimate_fun[length(estimate_fun)]
esterr <- sqrt(cumsum((x-estimate_fun)^2))/(1:100)
lines(estimate_fun + 2*esterr, col = "red")
lines(estimate_fun - 2*esterr, col = "red")

set.seed(111)
sim <- rcauchy(100)
x <- abf2(sim)
estimate_fun <- cumsum(x) /(1:100)
plot(estimate_fun,type="l",xlim = c(1,100),ylim = c(-10,5))
estimate_fun[length(estimate_fun)]
esterr <- sqrt(cumsum((x-estimate_fun)^2))/(1:100)
lines(estimate_fun + 2*esterr, col = "red")
lines(estimate_fun - 2*esterr, col = "red")

set.seed(123)
sim <- rcauchy(100)
x <- abf4(sim)
estimate_fun <- cumsum(x) /(1:100)
plot(estimate_fun,type="l",xlim = c(1,100),ylim = c(-10,5))
estimate_fun[length(estimate_fun)]
esterr <- sqrt(cumsum((x-estimate_fun)^2))/(1:100)
lines(estimate_fun + 2*esterr, col = "red")
lines(estimate_fun - 2*esterr, col = "red")

a=abf0(rcauchy(10^4))
estint=cumsum(a)/(1:10^4)
plot(estint,type="l")

curve(abf0,xlab="Function",ylab="",lwd=2)
curve(abf2,xlab="Function",ylab="",lwd=2)
curve(abf4,xlab="Function",ylab="",lwd=2)

f1=function(t){ t/(1+t*t)*exp(-(-t)^2/2)}
f2=function(t){ 1/(1+t*t)*exp(-(-t)^2/2)}
plot(f1,xlim=c(-3,3),ylim=c(-1,1))
plot(f2,xlim=c(-3,3),add=TRUE,ylim=c(-1,1))
legend("topright", c("f1=t.f2","f2"), lty=1,col=1 :2)

f1=function(x){ x/(1+x*x)*exp(-(-x)^2/2)}
f2=function(x){ 1/(1+x*x)*exp(-(-x)^2/2)}

##################### 3.3 ##########################
par(mfrow=c(1,1))
f33 <- function(x){ 
  return  (1/(x*x*sqrt(2*pi)*exp(1/(2*x*x))))
  }

curve(f33,from = 0, to = 1)
curve(f33,from = 0, to = 1/20)
points(runif(1000,0,0.05),runif(1000,0,2.5e-85))

X <- runif(10^4,0,1/20)
estimate_f33 <- cumsum(X) / (1:10^4)
esterr_f33 <- sqrt(cumsum((X - estimate_f33)^2))/(1:10^4)
plot(estimate_f33,type = "l",ylim = c(0.015,0.030))
lines(estimate_f33 + 2 * esterr_f33, col = "blue")
lines(estimate_f33 - 2 * esterr_f33, col = "blue")

estimate_f33[length(estimate_f33)]

(estimate_f33 + 2 * esterr_f33)[10^4]
(estimate_f33 - 2 * esterr_f33)[10^4]

#Using Integrate function
integrate(f33,0,1/20)

#Using accept reject method
set.seed(12)
ptunder <- sum(f33(runif(10^6,0,1/20)) >  runif(10^6,0,2.5e-85))
(ptunder / 10^6) * 0.05 * 2.56e-85

############## 3.4 ################

h <- function(x){
  return (exp((-(x-3)^2)/2) + exp((-(x-6)^2)/2))
}
curve(h, from = -10, to = 10)
set.seed(123)
x <- h(rnorm(10^3))
estimate_fun <- cumsum(x) /(1:10^3)
plot(estimate_fun,type="l",ylim = c(-0.1,0.3))
estimate_fun[length(estimate_fun)]
esterr <- sqrt(cumsum((x-estimate_fun)^2))/(1:10^3)
lines(estimate_fun + 2*esterr, col = "red")
lines(estimate_fun - 2*esterr, col = "red")

(estimate_fun + 2*esterr)[1000]
(estimate_fun - 2*esterr)[1000]

(exp(-9/4)+exp(-9))/sqrt(2)