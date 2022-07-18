############ Introduction to R ############
# Control + Shift + C forces a line to be a comment, in R studio
### Clt + L to clear ###

1+2 
1-2
5*2
4/10
2^4

x = c(1,2,3,4,5,6) ### c is a concatenate function (join or going together)
x
y = t(x)
y
z1 = matrix(x, nrow=2, ncol=3, byrow=T)
z1
z2 = matrix(x, nrow=2, ncol=3, byrow=F)
z2

row_1 = c(1,2,3,4,5)
row_1
row_2 = c(6,7,8,9,10)
row_2

table1 = rbind(row_1, row_2)			#rbind = combine by rows
table1
table2 = cbind(row_1, row_2)			#cbind = combine by columns
table2

colnames(table1) = c(A, "B", "C", "D", "E")
table1

rownames(table2) = c("A", "B", "C", "D", "E")
table2


table3 = table1
table3

rownames(table3) = c("Row_P", "Row_Q")
table3

#########
 # Solution to First Exercise on slide 10 of Lecture 1
colnames(table3) <- c("Hi","When","Five","Zoo","Final")
table3
rownames(table3) = c("Trinidad", "Tobago")
table3
########

#Change Directory before reading data
Data4 = read.csv("Diet.csv", header = T)
Data4



data1=read.table("Butterfat.txt",header=T)
data1

data2 <- read.csv("Diet.csv",header = F)
data2

data2.5 <- read.csv("Diet.csv",header = T)
data2.5



install.packages("readxl")
library(readxl)
data3=read_excel("slr.xlsx")
data3
View(data3)


##### Built in Data Sets #######
data()

############ Data Manipulation ############

women
dim(women)
str(women)
?women
attach(women) # Attach Set of R Objects to Search Path
height
women$height
women[ , 1]

women[1:5,]
women[8,2]

mean(height)
var(height)
sd(height)
sqrt(var(height))

### What does this do:  var(women$height)


hist(weight,main = "Histogram Showing Women's weight", col = "lightblue")
#Can label the axis of a histogram using xlab = "label", ylab = "label" 

max(height)
min(height)
median(height)

weight
order(weight) #gives the positions of the values in the data, NOT the actual data 
weight[order(weight)] ## This will give the actual values in the particular order ###A <- mtcars[order(mtcars$cyl),] ###
sort(weight, decreasing=T)  ### double check
sort(weight, decreasing=F)


install.packages("data.table")
library(data.table)
women = as.data.table(women) ## so you can call the columns, this is redundant for this data set though

which(height == 59) ## which gives the index
women[which(height == 59)]  ### this gives the actual values, any other method to do this?
women[which(weight > 150)]
women[which(weight < 120)]
women[2]


diet = read.csv("Diet.csv", header = T)
diet

# diet2 = read.csv("Diet.csv", header = F)
# diet2

attach(diet)
dim(diet)
diff = pre.weight - weight6weeks
weightgain = which(diff < 0)  ## gives row positions
length(weightgain)  ### number of elements in the data
(4/78)*100

A = (length(weightgain)/78)*100
A

######"Percentage of individuals who gaining weigh is =" A

dietnew = cbind(diet, diff) 
dietnew
dim(dietnew)
dietnew = as.data.table(dietnew)
no.gain = dietnew[which(diff >= 0)]  ## weight gain or loss???
dim(no.gain)
no.gain
attach(no.gain)

diet1 = length(which(Diet == 1))/length(Diet)  ### "subset-ing" the data by diet type
diet2 = length(which(Diet == 2))/length(Diet)  ### Length counts the number of rows for which *something* occurs
diet3 = length(which(Diet == 3))/length(Diet)
diet3

pct = c(diet1, diet2, diet3) * 100
pct
percent = paste(round(pct), "%", sep="")  ### paste concatenation of strings and numeric values
percent



#Pie Chart
Diet = table(Diet)  ## gives a frequency table, counts no of observaiton for each category, maybe str(no.gain)
Diet
diet.label = c("Diet1", "Diet2", "Diet3")
labels = paste(diet.label, percent)
labels
pie(Diet, labels, main = "Persons who did not gain weight")



### Alt percentage
### Diet = table(no.gain$Diet)
### Diet
### pct = (Diet/sum(Diet))*100
### pct


#Change colours
install.packages("RColorBrewer")
library(RColorBrewer)

#http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

colours = c("forestgreen","deepskyblue3","darkred")
pie(Diet, labels, main = "Persons who did not gain weight", col=colours)


#Legend
pie(Diet,labels=percent,col=colours,main="Persons who did not gain weight")
legend("topright",legend=diet.label,title="Diet Type",cex=1.5,fill=colours)  ## cex is font size for the legend

pie(Diet,labels=c("","",""),col=colours,main="Persons who did not gain weight")
legend("bottomleft",legend=diet.label,title="Diet Type",cex=0.8,lty=c(1,1),col=colours)


### End

