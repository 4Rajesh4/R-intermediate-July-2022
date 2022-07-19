############ Estimating Probability Distributions ############
######## Simulation of Data ######

## r is random value from that distribution
## d is density, 
## 
## x is the number of times you get a success out of n trials

X ~ Bin(125,0.04)
pop.mean = 125 * 0.04
pop.mean
pop.var = 125 * 0.04 * (1-0.04)


set.seed(25) ### keeps the data fixed, reproducibility ####
x=rbinom(10000,125,0.04)
mean(x)
var(x)

dbinom(5,125,0.04,log=F)   ## y axis value on distribution
pbinom(1,125,0.04,lower.tail=TRUE,log.p=F) ## cummulative probability distribution lower tail

#Y ~ Poisson(5)
## Cars
## no of calls in ah hour
## no of chocolates chips in a cookie
## no of something in a fixed interval: time or area

set.seed(12)
y=rpois(10000,5)
mean(y)
dpois(2, 5, log=F)
ppois(2, 5, lower.tail=T, log.p=F)

par(mfrow=c(1,2))  #par generates sub blots, 1 row, 2 columns, mfcol 
hist(x, main="Histogram of Binomial(125,0.04)", col="lightblue") ## n>30, p<0.1
hist(y, main="Histogram of Poisson(5)", col="lightblue")
#### Probability of a defect is 0.04,
#### 125 items of the same thing manufactured in a day
#### Approximated to having a Po(5) 

#### YouTube Video for more info: https://www.youtube.com/watch?v=eexQyHj6hEA 


############ Covariance and Correlation ############
### mutually exclusive events. 
## The event of getting a diamond is Mutually is exclusive to getting a heart,
## when you pull a heart.

## Independent Events: The occurrence of one does not affect the occurrence of the other
cov(x,y)
cor(x,y)

x = c(106,125,42,51,64,76,72,84,40,171,180,210,101,41,70)
y = c(10,44,0,2,8,14,21,18,24,17,26,52,16,11,37)
cor(x,y)
plot(x, y, type="p", main = "Scatterplot")
pairs(y~x)							#scatter plot matrix


############ Hypothesis Testing - One sample ############

#Example 1
weight = c(68, 63, 66, 81, 61, 73, 65, 77)
t.test(weight, mu=60, alternative="greater", conf.level=0.90)  
qt(0.10,df=7,lower.tail=F,log.p=F)  ## qt gives t, such that P(T>=t) = alpha 

## significance level alpha

#Example 2
diameter = c(1.01,0.97,1.03,1.04,0.99,0.98,0.99,1.02,1.01,1.03)
t.test(diameter, mu=1, conf.level=0.99)  ### the default is two tail test
qt(0.01/2,df=9,lower.tail=F,log.p=F) ### qt gives t score
qt(0.01/2,df=9,lower.tail=T,log.p=F) ### qt gives t score

#Example 3
rate=c(479.3,471.8,458.6,431.9,404.5,387.1,387.8,369.1,361.6,373.7,400.5,398.2)
t.test(rate, mu=420, alternative="less")
qt(0.05,df=11,lower.tail=T,log.p=F)


########################## END ###################################


