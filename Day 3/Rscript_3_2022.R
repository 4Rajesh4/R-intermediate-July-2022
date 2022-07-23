############ Hypothesis Testing - two samples ############
# Paired t-test example, link with the diet example in lecture 1,
# now instead of plotting pie charts, we saying with certainty that
# the diet worked (with some uncertainity)

# What will this do?? 
# THis will make another line 

before=c(73.1,43.8,91.3,70.0,88.3,111.4,93.0,78.4,61.2,85.9,63.2)
after=c(80.3,50.2,96.3,69.3,84.3,101.4,96.4,83.2,60.1,89.7,70.4)
t.test(before,after,alternative="two.sided",conf.level=0.90,paired=T)
qt(0.05,df=10,lower.tail=F,log.p=F)


## Two sample t-test for equal variance example 1
## Use the pooled T-Test (academic stuff)
## How can you tell if they have equal variances
## F test?? 

loc1 = c(25.4,22.0,24.3,25.2,28.0,23.9,23.6,24.8,
	23.1,27.2,27.0,26.5,23.2,26.7,22.2,25.5)
loc2 = c(24.8,24.3,22.0,20.9,21.5,22.6,25.0,20.8,
	20.4,21.6,22.3,24.4,24.0,20.9,21.1,24.0)
t.test(loc1, loc2, alternative="greater", conf.level=0.95, var.equal=T)
qt(0.05,df=30,lower.tail=F,log.p=F)



# Example 2 on slide 11, day 3
# Two sample t-test for unequal variance example 2
# Unequal Variance we use Welch Two Sample t-test (academic stuff)
sleep
attach(sleep)
drug1 = sleep[1:10,1] # if data
drug1
drug2 = sleep[11:20,1]
drug2
t.test(drug1, drug2, alternative="two.sided", conf.level=0.90, var.equal=F)
qt(0.05,df=18,lower.tail=F,log.p=F)

# a = which(group ==1)
# a
# length(a)
# sleep[c(a)]
# drug111 = sleep[which(group == 1),]
# drug111
# sleep[c(1:9),]




# Example 3 on slide 14, day 3
# Show click on data in Menu pane
#Two sample t-test for equal variance example 3
butterfat=read.table("Butterfat.txt",header=T)
attach(butterfat)
t.test(Brand1, Brand2, alternative="less", conf.level=0.99, var.equal=T)
qt(0.01,df=46,lower.tail=F,log.p=F)



############ Normal Distribution ############
# Example of slide 20, day 3
# set.seed(123)
x = rnorm(5000,80,10) # Note 10 and not 100
mean(x)
var(x)
#different values or same ?? why??

dnorm(100, 80, 10, log=F)
pnorm(100,80,10,lower.tail=T,log.p=F) -  pnorm(65,80,10,lower.tail=T,log.p=F)



############ Hypothesis Testing - proportions ############


#One sample examples
prop.test(45, 120, p = 0.40, alternative="greater", conf.level=0.95, correct=T)

## Example 2 (newspaper)
prop.test(50, 200, p = 0.32, alternative ="less", conf.level = 0.90, correct=T)


# Exercises tomorrow for you to try (uploaded already)
# R Mark down
  # Exercise: Add color to the course outline, make it look better. 
# Projects
# Extra: https://code.visualstudio.com/ for editing code 
# R studio website
