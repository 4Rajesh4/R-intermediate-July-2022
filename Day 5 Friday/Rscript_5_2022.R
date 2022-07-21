############ Chi-squared Independence Test ############


install.packages("MASS")
library(MASS)

## One-way example 1
## Whether there is a relationship between the sport and the number of concussions
## Goodness of fit with the uniform distribution
##
conc = c(11, 26, 45, 68)
chisq.test(conc)
qchisq(0.90, df=3) # chi-squared critical vale 

##One-way example 2
carb = table(mtcars$carb) ### Table is used to get the frequency of categories in one column
carb
chisq.test(carb)
qchisq(0.98, df=5)


##Two-way example 1

## Test for independence between Categorical Variables
## (Row/grand) * (col/Grand) *(Grand) Simplifies to (Row*col)/(Grand)
## P(A male exercising in the morning) (6/65)
## is equal to the P(Male) * P(Someone exercise in the morning)
## (36/65)* (19/65)
## Sum: (O-E)^2 / E

#Option 1 : Manually enter the data in R
Male = c(6, 21, 9)
Female = c(13, 10, 6)
table = rbind(Male,Female)
table
colnames(table) = c("Morning", "Evening", "Night")
table
chisq.test(table)
qchisq(0.95, df=2)
### Reject Ho and conclude that there is a relationship between exercise preference time and gender


#Matrix option for entering data
table1 = matrix(c(6,21,9,13,10,6), nrow=2, ncol=3, byrow=T)
colnames(table1) = c("Morning", "Evening", "Night")
rownames(table1) = c("Male", "Female")
table1


#Option 2 : Creating Excel data file
library(readxl)
data = read_excel("Exercise.xlsx")
data
new.table = data[,2:4]
new.table
chisq.test(new.table)	
qchisq(0.95, df=2)


##Two-way example 2
data2= read.csv("https://goo.gl/j6lRXD", header=T)
data2

## A <- table(data2$treatment, data2$improvement)
## chisq.test(A, correct=FALSE)


chisq.test(data2$treatment, data2$improvement, correct=FALSE)
qchisq(0.99, df=1)
### At the 10% we reject
## At the 5% S.L we reject
## At the 1% we Do not reject
## Remember reject Ho if p-cal is less than alpha (significance level)

#ALT
table = table(data2[,2:3])
table
chisq.test(table, correct=F) ## Fisher exact test ###



############ Correlation tests ############


##Example 1

malaria = read.csv("malaria.csv", header=T)
library(data.table)
data = as.data.table(malaria)
exposed = data[which(data$malaria == 1)]
attach(exposed)

shapiro.test(age)
shapiro.test(antibody)
cor.test(age, antibody, method = "spearman", exact=F)
cor.test(age, antibody, method = "spearman")


##Example 2
swiss
shapiro.test(swiss$Fertility)
shapiro.test(swiss$Agriculture)
cor.test(swiss$Fertility, swiss$Agriculture, conf.level=0.90, method="pearson")



######################### END #########################



