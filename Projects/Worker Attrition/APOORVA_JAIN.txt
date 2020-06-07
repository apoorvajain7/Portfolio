#done by Apoorva Jain and Vaibhav Arvind Sonar

Mydata0 = read.csv("hw2.csv")
str(Mydata0)

set.seed(44)
#1.

#1.a.i.  #Table showing the average attrition rate depending on business travel 
table1=tapply(Mydata0$Attrition, Mydata0$BusinessTravel, mean)
table(table1)

#1.a.ii.  Table showing the average attrition rate depending on department
table2=tapply(Mydata0$Attrition, Mydata0$Department, mean)
table(table2)


#1.a.iii.  Table showing the average attrition rate depending on education field
table3=tapply(Mydata0$Attrition, Mydata0$EducationField, mean)
table(table3)

#1.a.iv.  Table showing the average attrition rate depending on geneder
table4=tapply(Mydata0$Attrition,Mydata0$Gender, mean)
table(table4)

#1.a.v.  Table showing the average attrition rate depending on job role
table5=tapply(Mydata0$Attrition, Mydata0$JobRole, mean)
table(table5)

#1.a.vi.  Table showing the average attrition rate depending on marital status
table6=tapply(Mydata0$Attrition, Mydata0$MaritalStatus, mean)
table(table6)

#1.a.vii.  Table showing the average attrition rate depending on overtime
table7=tapply(Mydata0$Attrition, Mydata0$OverTime, mean)
table(table7)



#2.a.	average Age depending on whether Attrition = 0 or 1.
table8=tapply(Mydata0$Age, Mydata0$Attrition, mean)
table(table8)

#2.i. Monthly income
table9=tapply(Mydata0$MonthlyIncome, Mydata0$Attrition, mean)
table(table9)

#2.ii Job satisfaction
table10=tapply(Mydata0$JobSatisfaction, Mydata0$Attrition, mean)
table(table10)

#2.iii.	YearsSinceLastPromotion
table11=tapply(Mydata0$YearsSinceLastPromotion, Mydata0$Attrition, mean)
table(table11)

#2.iv.	YearsInCurrentRole
table12=tapply(Mydata0$YearsInCurrentRole, Mydata0$Attrition, mean)
table(table12)

#2.v.  Total working years
table13=tapply(Mydata0$TotalWorkingYears, Mydata0$Attrition, mean)
table(table13)

#2.vi  Distance from home
table14=tapply(Mydata0$DistanceFromHome, Mydata0$Attrition, mean)
table(table14)

# 3.
nums = unlist(lapply(Mydata0,is.numeric))  
MyDataNum = Mydata0[ , nums]
mycor= cor(MyDataNum)
View(mycor)

# 4. ggplot2, MonthlyIncome as a function of Age, color coded by Attrition
library(ggplot2)
ggplot(Mydata0, aes(x= Age, y= MonthlyIncome, color= Attrition))+geom_point() + ggtitle("Monthly Income vs Age")

#5.Plot TotalWorkingYears as a function of MonthlyIncome color-coded by NumCompaniesWorked
ggplot(Mydata0, aes(x= MonthlyIncome, y= TotalWorkingYears, color= NumCompaniesWorked))+geom_point(shape=8) + ggtitle("Monthly Income vs Total Working Years")



#6.Creating subset and ploting histogram
MyData1 <- subset(Mydata0, Attrition=="1")
ggplot(MyData1, aes(x= MonthlyIncome))+geom_histogram(bins=30) + ggtitle("Monthly Income for Attrition 1")

MyData0 <- subset(Mydata0, Attrition=="0")
ggplot(MyData0, aes(x= MonthlyIncome))+geom_histogram(bins=30) + ggtitle("Monthly Income for Attrition 0")


#7.Comparing Graphs for commenting role of MonthlyIncome in attrition

ggplot(Mydata0,aes(x=MonthlyIncome, y=..density..)) + geom_freqpoly(data=MyData1,color = "red") + geom_freqpoly(data=MyData0,color = "blue")

#8.Comparing Graphs for commenting role of TotalWorkingYears

ggplot(Mydata0,aes(x=TotalWorkingYears, y=..density..)) + geom_freqpoly(data=MyData1,color = "red") + geom_freqpoly(data=MyData0,color = "blue")


# Part 2
# 2.1 Separate the data frame into a training set (65% of data) and a testing set (35%), assigning instances randomly to either set
library(caTools)

set.seed(73)
mydata=read.csv("Attrition.csv")
str(mydata)
nrow(mydata)
#answer part 2 question 1 split into 35 and 65 
spl=sample.split(mydata$Attrition,SplitRatio=0.35)
datatest=subset(mydata,spl==TRUE)
spl=sample.split(mydata$Attrition,SplitRatio=0.65)
datatrain=subset(mydata,spl==TRUE)
str(datatest)
str(datatrain)
table(datatrain$Attrition)

#answer part 2 question 2 accuracy base line threshold= 0.5
pred=predict(attritionmodel18,type="response", newdata= datatrain)
pred
table(datatrain$Attrition,pred>0.5)

#part 2 answer 3 logistic regression training
attritionmodel=glm(Attrition~., data=datatrain , family = binomial)
summary(attritionmodel)
#part 2 nswer 4 refining training
attritionmodel=glm(Attrition~.-Department , data=datatest , family = binomial)
summary(attritionmodel)
attritionmodel18=glm(Attrition~.-JobLevel-YearsWithCurrManager-Gender-Age-HourlyRate-PercentSalaryHike-WorkLifeBalance-MaritalStatus-DailyRate-DistanceFromHome-MonthlyRate-Department-JobRole-MonthlyIncome-EducationField-NumCompaniesWorked-PerformanceRating-Education-YearsAtCompany, data=datatest , family = binomial)
summary(attritionmodel18)
#answer part 2 question 5
library(ROCR)
rocprediction= prediction(pred,datatest$Attrition)
roccurve=performance(rocprediction,'tpr','fpr')
plot(roccurve,colorize=TRUE,print.cutoff.at=seq(0,1,0.1),text.adj=c(-0.2,0.7))
as.numeric(performance(rocprediction,"auc")@y.values)
#part 2 question 6
pred=predict(attritionmodel18,type="response", newdata= datatest)
pred
table(datatest$Attrition,pred>0.3)
library(ROCR)
rocprediction= prediction(pred,datatest$Attrition)
roccurve=performance(rocprediction,'tpr','fpr')
plot(roccurve,colorize=TRUE,print.cutoff.at=seq(0,1,0.1),text.adj=c(-0.2,0.7))
as.numeric(performance(rocprediction,"auc")@y.values)
