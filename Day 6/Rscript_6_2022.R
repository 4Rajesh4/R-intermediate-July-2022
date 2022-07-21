############ One Way ANOVA ############
### Extension of 2 Sample Independent T test.
### Methods of Learning
### ONE way is completely randomized Design
### Two way is Completely Randomiezed Blocking Design
### Different Classes examples with Blocking:

### Treatment (treatment group)
### Face to face Learning (25): Marks:  5, 6, 7, 6, 5, 7, 9, 8. MEAN around <10
### Online Only  (25): Marks: 30, 35, 36, 38, 40.... MEAN around 30-40
### Blended Learning: Both Face to Face and Online Learning (25): Marks: 70, 80, 85, 87, 90 Mean: >70


### Treatment groups and Replicates (Data)
### Treatments is among the groups
### Residuals is within the groups 


##Example 1
price=c(30,34,36,38,40,30,35,37,38,40,40,41,43,44,50)
year=c(rep("1996",5), rep("1997",5), rep("1998",5))
year
anova=aov(price~year)
## anova=aov(price~year,conf.level = 0.90) ## there is no CL for ANOVA, because of the F distribution and what the test is calculating, the ratio of MSgroup/MSreplicate
anova
summary(anova)
qf(0.99,2,12) ##q gives the value on the x axis
### 



##Example 2: Hindu, Christian, Muslim, Other
persons=c(16,20,18,20,21,25,17,15,19,3,5,10)
religion=c(rep("h",3),rep("c",3),rep("m",3),rep("o",3))	
anova=aov(persons~religion)
summary(anova)							
qf(0.95,3,8)


TukeyHSD(anova, conf.level=0.95)  ### Honest Significant Difference



############ Kruskal Wallis ###########

##Example 1
chickwts
attach(chickwts)
kruskal.test(weight~feed, conf.level=0.95)
kruskal.test(weight~feed)
qchisq(0.90, df=5)

### to find where the difference is...
TukeyHSD(aov(weight~feed), conf.level=0.90)

##Example 2

#Option 1: Manually
### shapiro.test(c(59,60,59,55,50))


## Data must line up under each other. 
hours = c(59,60,59,55,50,57,57,46,54,51,45,53,49,51,46,44,45,39,58,44)
hospital = c(rep("1",5), rep("2",5), rep("3",5), rep("4",5))
hospital
kruskal.test(hours~hospital)

###summary(aov(hours~hospital))
qchisq(0.99, df=3)

#Option 2: Data file
icu = read.csv("ICU_patients.csv", header=T)
attach(icu)
kruskal.test(hours~hospital)
qchisq(0.99, df=3)

##Example 3
cereal = read.csv("cereal.csv", header=T)
cereal
cost = cereal[,2]
cost
location = cereal[,9]
location
kruskal.test(cost~location)
qchisq(0.95, df=3)


##### Just to check for yourself
TukeyHSD(aov(cost~location), conf.level=0.90)
location <- as.factor(location)
## cost <- as.factor(cost)  , Note cost must be a numeric in nature, this changes cost to a factor, the Tukey test will not run like this
## cost <- as.numeric(cost), this changes it back to numeric
TukeyHSD(aov(cost~location), conf.level=0.90) ## The code will run now

######################### END #########################
