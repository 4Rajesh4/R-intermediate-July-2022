############ Bonus , Final Exercises ############


######################## Question 1 #############################

#H0: p1 >= p2 vs H1: p1 < p2

prop.test(x=c(2,4), n=c(48,52), p=NULL,
	alternative="less", conf.level=0.94, correct=TRUE)
## Why correction ? 
## When you go from a discrete to continuous distribution


#CI contains 0 and pvalue > 0.06 so fail to reject H0
#insufficient evidence to conclude prop in factory 1 < prop in factory 2



######################## Question 2 #############################
### Two way independence Test
## H0: independent/no relationship vs 
## H1: dependent/relationship

library(MASS)
two.way = matrix(c(56,80,23,41), nrow=2, ncol=2, byrow=TRUE)
colnames(two.way) = c("Absent", "Not Absent")
rownames(two.way) = c("Male", "Female")
two.way

chisq.test(two.way, correct=F) ## T if one (or more) of the enteries is < 5.
qchisq(0.95, df=1)

#test stat < critical and pvalue > 0.05 so fail to reject H0
#no relationship between gender and absenteeism



######################## Question 3 ########################
################### Day 5 in Lecture notes ########
## need to load the MASS package first

attach(wtloss)
#H0: normal vs H1: not normal
shapiro.test(Days)
shapiro.test(Weight)
#Days pvalue > 0.02 and Weight pvalue < 0.02
#Days variable normal but Weight variable not normal

#H0: rho = 0 vs H1: rho /= 0
cor.test(Days, Weight, method = "spearman", exact=F)
## p-value < 0.02 so reject H0
## significant relationship between weight and time



######################## Question 4 ########################

#H0: mu1 = mu2 = mu3 vs H1: At least one mu_i different
scores = c(643, 655, 702, 469, 427, 525, 484, 456, 402)
pitch = c(rep("P1",3), rep("P2",3), rep("P3",3))
pitch
kruskal.test(scores~pitch)
qchisq(0.90, df=2)
#test stat > critical and pvalue < 0.10 so reject H0
#At least one pitch has a different mean score

TukeyHSD(aov(scores~pitch), conf.level=0.90)
#Comparison1: CI does not include 0 and pvalue < 0.10 P1 and P2 different
#Comparison2: CI does not include 0 and pvalue < 0.10 P1 and P3 different
#Comparison3: CI includes 0 and pvalue > 0.10 P2 and P3 same



######################## Question 5 ########################

#H0: mu1 = mu2 = mu3 vs H1: At least one mu_i different
sugar = read.table("sugar.txt", header=TRUE)
sugar
#We need to exclude the first column and convert the data to a matrix
sugardata = as.matrix(sugar[,2:6])
data = c(sugardata)
data
T = c("50lb","75lb","100lb")
a = 3
b = 5
N = a*b
trt = gl(a,1,N,factor(T))
blk = gl(b,a,N)
crbd = aov(data ~ trt + blk)
summary(crbd)
qf(0.99,2,8)
## test stat < critical and p-value > 0.01 so do not reject H0
## note at the  3% or higher it will be significant 
## Mean sugar content is the same for all 3 treatments i.e. does not affect



######################## Question 6 ########################

##i
x = c(7.01,7.11,7.12,7.24,7.94,7.94,8.04,8.05,8.07,8.90)
y = c(60,67,66,52,50,45,52,48,40,23)
reg=lm(y~x)
summary(reg)
#the fitted regression model is yhat = 202.502 - 19.659x


##ii
#T-test: Method 1
qt(0.025,8,lower.tail=F,log.p=F)  ## df is (n-2)
## Calculated value is -6.497, take the positive value 
#|test stat| in (i) > critical value so reject H0

# Using p-value Method 2
#pvalue < 0.05 so reject H0

#T-test: Method 3
confint(reg,level=0.95)
#CI for beta1 does not include 0 so reject H0

#Conclusion: the slope coefficient and hence regression is significant


##iii
anova.reg=aov(reg)
summary(anova.reg)
### If significant, the overall model is significant

##iv
### Diagnostic Test (perform test first then do the analysis)

windows()
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
plot(1:10,reg$residuals, main = "Residuals versus time order", pch = 20)
abline(h=0, lty=2)

#histogram is symmetric and points equally distributed above and below the qqline
#Residuals vs Fitted plot: points equally split in the band (-5,5)
#Residuals vs Time plot: no pattern in the spread of data points.
#All 3 assumptions hold



############################## END ##############################




