############ Completely Randomised Block Design ############
## Yesterday was Completly randomized Design (No blocking)
### Input data by treatment1 then treatment2, treatment 3... for ONE block
### We want a homogenity in each block


##Example 1
table = matrix(c(7.62,8.14,7.76,7.17,7.46,8.00,8.15,7.73,7.57,7.68,7.93,
	7.87,7.74,7.80,7.21), nrow=3, ncol=5, byrow=TRUE)
table
data = c(t(table)) 
data
T = c("36", "54", "72", "108", "144")
a = 5  ## a is the number of Treatment groups
b = 3  ## b is the number of blocks
N = a*b

### trt <- rep(T, b)
### trt <- as.factor(rep(T, b))

trt = gl(a, 1, N, factor(T)) ## will be the same once set up properly. 
trt
blk = gl(b, a, N)  ## will be the same once set up properly. 
blk

crbd = aov(data~trt + blk)
summary(crbd)
qf(0.95,4,8)


# There is a treatment value for each block, this will minimize the blocking effect


##Example 2
table = matrix(c(709,713,660,668,722,692,659,666,678,698,704,686),
	nrow=4, ncol=3, byrow=TRUE)
data = c(table)
data # within each block (Writing Surface, you have each treatment)
T = c("BP1", "BP2", "BP3", "BP4")
a = 4
b = 3
N = a*b
trt = gl(a, 1, N, factor(T))
blk = gl(b, a, N)
crbd = aov(data~trt + blk)
summary(crbd)
qf(0.95,3,6)

### We we accept Ho for treatments (same mean)
### But reject Ho for blocks (blocking is significant)
## Big nuisance ????


#ALT: can also directly enter the data as a vector but in the order of blocks
data = c(709,668,659,698,713,722,666,704,660,692,678,686)
T = c("BP1", "BP2", "BP3", "BP4")
Brand = gl(4, 1, 12, factor(T))
Surface = gl(3, 4, 12)
crbd = aov(data~Brand + Surface)
summary(crbd)


##Example 3
VADeaths
data = c(VADeaths)
data
T = c("50-54","55-59","60-64","65-69","70-74")
a = 5
b = 4
N = a*b
trt = gl(a,1,N,factor(T))
blk = gl(b,a,N)
crbd = aov(data~trt + blk)
summary(crbd)
qf(0.99,4,12)



############ Simple Linear Regression ############
### Stochastic: The RV have random distributions ####


##Example 1
x = c(1,2,3,4,5,6,7,8,9)
y = c(6.8,6.6,6.6,6.4,6.1,5.7,5.5,5.2,4.9)
reg=lm(y~x)
summary(reg)
anova.reg=aov(reg)
summary(anova.reg)
qf(0.90,1,7)

## adjusted R^2, modifies the R^2 value subject to the number of predictors in the model ###
## R^2 coefficient of variation
## R^2 will always increase or stay the same. 
## r is the correlation coefficient

confint(reg,level=0.90)

plot(x,y,xlab="Day",ylab="pH",main="Relationship between milk pH and day")
abline(reg,col="pink")

plot(x,y,xlim=c(0,9),ylim=c(4.8,8.0),xlab="Day",ylab="pH",
	main="Relationship between milk pH and day")
abline(reg,col="black")

#X11() #windows() # manually adjust the window, if there an a margin error!
X11(width=10, height=10)
split.screen(c(2,2))
screen(1)
hist(reg$residuals, main = "Histogram of Residuals")

screen(2)
qqnorm(reg$residuals, pch = 20) ## reg is the regression model that contains residuals
qqline(reg$residuals)

screen(3)
plot(reg$fitted.values, reg$residuals, main = "Residuals versus Fitted",pch = 20)
abline(h=0, lty=2)

screen(4)
plot(1:9,reg$residuals, main = "Residuals versus time order", pch = 20)
abline(h=0, lty=2)




##Example 2
library(readxl)
regdata = read_excel("slr.xlsx")
regdata
reg=lm(regdata$Y ~ regdata$X)
summary(reg)

dim(regdata)
qt(0.025,60,lower.tail=F,log.p=F)


## Extra, follow example above
X11(width=10, height=10) #windows()
split.screen(c(2,2))
screen(1)
hist(reg$residuals, main = "Histogram of Residuals")
screen(2)
qqnorm(reg$residuals, pch = 20)
qqline(reg$residuals)
screen(3)
plot(reg$fitted.values, reg$residuals, main = "Residuals versus Fitted",pch = 20)
abline(h=0, lty=2)
screen(4)
plot(1:62,reg$residuals, main = "Residuals versus time order", pch = 20)
abline(h=0, lty=2)



######################## END ########################



