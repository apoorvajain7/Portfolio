#PROJECT 2 
#APOORVA JAIN AND VAIBHAV SONAR
#reading the data
set.seed(12)
socio=read.csv("Surveys 2016-2019.csv",header=TRUE)
sales=read.csv("Sales Data & Stats 2016 - 2019.csv",header=TRUE)
pay=read.csv("Payment Calendars 2016 - 2019.csv", header=TRUE)

#merging the data "socio","sales","pay"
sociosales=merge(x=socio,y=sales,by="Id.de.contacto")
mydata=merge(x=sociosales, y=pay, by="Id..de.la.oportunidad")
na.omit(mydata)
str(mydata)
write.csv(mydata,file="datafile1.csv")

#installing the packages needed
install.packages("caTools")
install.packages("dplyr")
install.packages("ROCR")
install.packages("lubridate")
install.packages("PerformanceAnalytics")
install.packages("rpart.plot")
#reading the data needed (used small sets as we talked about it )
data1= read.csv("datafile1.csv", nrows=30000)
library(dplyr)
refine=select(data1,-10:-37)
na.omit(refine)
str(refine)
#question 1 
#performed in TABLEAU
#question 2 analysing the logistic regression and performing it on training sets
library(caTools)
spl=sample.split(refine$Paid, SplitRatio = 0.65)
datatrain=subset(refine,spl==TRUE)
datatest=subset(refine,spl==FALSE)
refinelog=glm(Paid ~ Sexo+total.balance+paid.balance+credit.term.months+months.passed+Rating.Interno.Credito+
              months.passed,data=datatrain,family=binomial)
summary(refinelog)

#predict on training
PredictTrain = predict(refinelog, type="response") 
PredictTrain
table(datatrain$Paid, PredictTrain > 0.4)
library(ROCR)
ROCRpred = prediction(PredictTrain, datatrain$Paid) 
ROCCurve = performance(ROCRpred, "tpr", "fpr") 
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)

#predict on testing sets for question 3 prediction and evaluation 
refinelog1=glm(Paid ~ Sexo+total.balance+paid.balance+credit.term.months+months.passed+Rating.Interno.Credito+
                months.passed,data=datatest,family=binomial)
summary(refinelog1)
PredictTest1 = predict(refinelog1, type="response")
PredictTest1
table(datatest$Paid, PredictTest1 > 0.4)
library(ROCR)
ROCRpred1 = prediction(PredictTest1, datatest$Paid) 
ROCCurve = performance(ROCRpred1, "tpr", "fpr") 
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred1, "auc")@y.values)

#correlation question 5
library(lubridate)
as.numeric(data1$Scheduled.Date)
install.packages("PerformanceAnalytics")
install.packages("dplyr")
library(dplyr)
datacor=cbind(data1$Scheduled.Date,data1$total.balance,data1$paid.balance,data1$Paid,data1$credit.term.months,data1$late.balance,data1$Rating.Interno.Credito,data1$Situacion.del.Cliente)
colnames(datacor) <- c("scheduled", "totalbalance","paidbal","paidone","creditterm","latebal","creditrating","clientsituation")
datacor1=data.frame(datacor)
library("PerformanceAnalytics")
chart.Correlation(datacor1)
#BETTER VISUALIZATION CORRELATION CHART
install.packages("RColorBrewer")
install.packages("corrplot")
library(corrplot)
na.omit(datacor1)
x <- cor(datacor1)
library(RColorBrewer)
corrplot(x, type="upper",method = "square",col=brewer.pal(n=8, name="PuOr"))

# cluster analysis(clients) answer 6
# Normalization
clientcluster=data1[c("Codigo.de.Cliente","Estado.x","age","Locality","Temporalidad.de.Ingresos","Situacion.del.Cliente","Fuente.de.Iluminacion","Aparatos.que.comprara.con.luz","Rating.Interno.Credito")]
na.omit(clientcluster)
install.packages("caret")
library(caret)  
preproc = preProcess(clientcluster) 
#preparation of the transformation of the data (scaling, centering)
clientclusternorm = predict(preproc, clientcluster) 
na.omit(clientclusternorm)
#applying the transformation to a data set
# Hierarchical Clustering
distances = dist(clientclusternorm, method="euclidean")
Hierclustering = hclust(distances, method="ward.D2")
plot(Hierclustering,hang = -1,)
clusterGroups = cutree(Hierclustering, k = 10)
rect.hclust(Hierclustering,k=10, border="green")
table(clusterGroups)
#cluster groups shown above
