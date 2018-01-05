#import data
xdata <- read.csv("E:\\Google Drive\\Fall 17 Sem 2\\630 Machine Learning\\week1\\xdata.txt",header = FALSE, sep=",")
ydata <- read.csv("E:\\Google Drive\\Fall 17 Sem 2\\630 Machine Learning\\week1\\ydata.txt",header = FALSE, sep=",")
wdata <- read.csv("E:\\Google Drive\\Fall 17 Sem 2\\630 Machine Learning\\week1\\wdata.txt",header = FALSE, sep=",")
wdata[2] <- NULL

View(xdata)
xdata <- xdata[,-4]
View(ydata)

xdata <- c(xdata,ydata)
names(xdata)[4] <- "V4"
model1 <- lm(V4 ~ .,xdata)
model1_res <- residuals(model1)
summary(model1)
sum(abs(model1_res))

h<-hist(model1_res,breaks=20,prob=T,xlab="residuals")
x <- seq(-10,10,length = 300)
y <- dnorm(x,mean(model1_res),sd(model1_res))
lines(x,y,col="red")
#y = m1*x1 + m2*x2 + m3*x3 + c


# summary(model1)
# 
# Call:
#   lm(formula = V4 ~ ., data = xdata)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.0475 -0.9138  0.0688  0.9695  7.2001 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   32.6359     0.7502   43.50   <2e-16 ***
#   V1            54.5686     0.7029   77.63   <2e-16 ***
#   V2          -128.0465     0.7540 -169.83   <2e-16 ***
#   V3             9.6541     0.7820   12.35   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 2.096 on 96 degrees of freedom
# Multiple R-squared:  0.9975,	Adjusted R-squared:  0.9974 
# F-statistic: 1.272e+04 on 3 and 96 DF,  p-value: < 2.2e-16

model2 <- lm(data.matrix(V4) ~ .,data = (xdata) , weights = data.matrix(wdata))
model2_res <- model2$residuals
h<-hist(model2_res,breaks=20,prob=T,xlab="residuals")
x <- seq(-10,10,length = 300)
y <- dnorm(x,mean(model2_res),sd(model2_res))
lines(x,y,col="red")