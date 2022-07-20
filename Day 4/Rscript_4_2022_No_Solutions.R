############ Hypothesis Testing - two sample proportions ############


#Example 1
prop.test(x=c(90,125), n=c(200,275), p=NULL,
	alternative="two.sided", conf.level=0.97, correct=T)

#Example 2
attach(mtcars)
library(data.table)
x1 = which(gear == 4)
length(x1)
x2 = which(gear == 5)
length(x2)

prop.test(x = c(12,5), n=c(32,32), p = NULL,
	alternative="greater", conf.level=0.95, correct=T)



############ Exercises ############
#Question 4
# You can copy and Paste the data instead of typing it out...
m1=c(0.70,0.65,0.96,0.23,0.51,0.63,0.88,0.12,0.36,0.92,0.47,0.09)
m2=c(0.70,0.68,0.84,0.44,0.49,0.70,0.89,0.18,0.56,0.92,0.72,0.12)