############ Hypothesis Testing - two sample proportions ############


#Example 1
## Note NOT 90 with the 200, when inputting the data into R.
## It is the successes in one variable and the number of elements in the other
## variable. 

prop.test(x=c(90,125), n=c(200,275), p=NULL,
	alternative="two.sided", conf.level=0.97, correct=T)



#Example 2
attach(mtcars)
mtcars
str(mtcars)
library(data.table)
x1 = which(gear == 4)
length(x1)

x2 = which(gear == 5)
length(x2)

prop.test(x = c(12,5), n=c(32,32), p = NULL,
	alternative="greater", conf.level=0.95, correct=T)



############ Exercises ############

############################    #Question 1   ##########################
### Question 1 (longest method)
# Method 1
iris
str(iris)
attach(iris)
library(data.table)
iris = as.data.table(iris)
s1 = iris[which(Species == "setosa")]  ## subsets the data for the setosa only
s1
row1 = colMeans(s1[,1:4]) ## Cannot have the last column, as it has text 

s2 = iris[which(Species == "versicolor")]
row2 = colMeans(s2[,1:4])
s3 = iris[which(Species == "virginica")]
row3 = colMeans(s3[,1:4])

table = rbind(row1, row2, row3)
rownames(table) = c("setosa","versicolor","virginica")
table









### Alternative Using sapply (middle) ####
# Method 2
iris
attach(iris)
library(data.table)
iris = as.data.table(iris)
s1 = iris[which(Species == "setosa")]
row1 = sapply(s1[ , 1:4], FUN = mean) ## because the last column is text
row1
s2 = iris[which(Species == "versicolor")]
row2 = sapply(s2[,1:4], FUN = mean)
s3 = iris[which(Species == "virginica")]
row3 = sapply(s3[,1:4], FUN = mean)

table = rbind(row1, row2, row3)
rownames(table) = c("setosa","versicolor","virginica")
table


### Alternative Using aggregate (fastest) ###
# Method 3
aggdata <- aggregate(iris[ , 1:4], by=list(iris$Species), FUN= mean) 
aggdata
## list groups the data by the factor that is in the list
## parameter for which you want to group the data by. 
### Alternative is tapply ##


# Method 4
T1 <- tapply(iris$Sepal.Length, iris$Species, FUN = mean)
T2 <- tapply(iris$Sepal.Width, iris$Species, FUN = mean)
T3 <- tapply(iris$Petal.Length, iris$Species, FUN = mean)
T4 <- tapply(iris$Petal.Width, iris$Species, FUN = mean)

Table_t <- t(rbind(T1, T2, T3, T4))
Table_t
## label columns










############################    #Question 2   ##########################
pbinom(3,2000,0.001,lower.tail=TRUE,log.p=F) #p is for cummulative for both

dpois(4,2,log=F) #d is for exactly equal in discrete
# Y<=4, 
# ppois(4,2,lower.tail = TRUE)

set.seed(8)
x=rbinom(50000,2000,0.001)
# set.seed(8) 
y=rpois(50000,2)
par(mfrow=c(1,2)) #opens a plot window, row by column
hist(x, main="Binomial(2000,0.001)")		
hist(y, main="Poisson(2)")	
Cov <- cov(x,y)
Cov
Cor <- cor(x,y)
Cor
Scatterplot <- plot(x,y)
# Little to no relationship and relationship between X and Y


z=rbinom(50000,2000,0.001)
Cov <- cov(x,z)
Cov
Cor <- cor(x,z)
Cor
Scatterplot <- plot(x,z)




############################    #Question 3   ##########################
Orange
attach(Orange)
#H0: mu = 110 vs H1: mu /= 110
t.test(circumference, mu=110, alternative="two.sided", conf.level=0.95)
qt(0.05/2,df=34,lower.tail=F,log.p=F) # T Critical Value
#CI includes 110, t < 2.032 and p-value > 0.05
#We do not reject H0 so there is no significant difference













############################    #Question 4   ##########################
m1=c(0.70,0.65,0.96,0.23,0.51,0.63,0.88,0.12,0.36,0.92,0.47,0.09)
m2=c(0.70,0.68,0.84,0.44,0.49,0.70,0.89,0.18,0.56,0.92,0.72,0.12)
# A = mean(m1)-mean(m2)
# A
diff=m1-m2
diff
dbar=mean(diff)
dbar
#H0: mu_D <= 0 vs H1: mu_D > 0
t.test(m1,m2,alternative="greater",conf.level=0.90,paired=T)
qt(0.10,df=11,lower.tail=T,log.p=F)

#mean difference = dbar = -0.06
#CI does not contain 0, |t| > 1.363 and p-value < 0.10
#We can reject H0 concluding that employee performance decreased
#from the 1st month to the 2nd 

### Calculation of the Test Statistic itself 
D_sd <-sd(diff) ## sample standard deviation of the differences
T_cal <- ((dbar - 0) / (D_sd/sqrt(length(diff))))
T_cal

################################   End   ####################################
