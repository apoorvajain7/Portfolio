data=read.csv("Sentiment.csv")
head(data)
library(tidyverse)
datas=data%>%select(text,sentiment)
head(datas)
round(prop.table(table(datas$sentiment)),2)
library(tm)
library(NLP)
library(SnowballC)
corpus = VCorpus(VectorSource(datas$text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)
as.character(corpus[[1]])
dtm = DocumentTermMatrix(corpus)
dtm
dim(dtm)
dtm = removeSparseTerms(dtm, 0.999)
dim(dtm)
freq<- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
findFreqTerms(dtm, lowfreq=100)
library(ggplot2)
wf= data.frame(word=names(freq), freq=freq)
head(wf)
library("wordcloud")
library("RColorBrewer")
par("mar")
par(mar=c(0.1,0.1,0.1,0.1))
positive= subset(datas,sentiment=="Positive")
wordcloud(positive$text, max.words = 100, scale = c(1,0.5), colors = "blue")
negative = subset(datas,sentiment=="Negative")
wordcloud(negative$text, max.words = 100, scale = c(1,0.5), colors = "purple")
neutral =  subset(datas,sentiment=="Neutral")
wordcloud(neutral$text, max.words = 100, scale = c(1,0.5), colors = "turquoise")
library("wordcloud")

## Loading required package: RColorBrewer
library("RColorBrewer")
set.seed(74)
par("mar")
par(mar=c(.1,.1,.1,.1))
display.brewer.all(6)
wordcloud(words = wf$word, freq = wf$freq, min.freq = 10,
          max.words=300, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(11, "Spectral"))
##As naive bayes algorithm excepts binary 
convert <- function(x) {
y <- ifelse(x > 0, 1,0)
y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
y
    }  
    
    datanaive = apply(dtm, 2, convert)
    
    dataset = as.data.frame(as.matrix(datanaive))    
    dataset$Class = datas$sentiment
    str(dataset$Class)
    
    head(dataset)
    dim(dataset)
    
# data splitting
    set.seed(31)
    split = sample(2,nrow(dataset),prob = c(0.75,0.25),replace = TRUE)
    train_set = dataset[split == 1,]
    test_set = dataset[split == 2,] 
    
    prop.table(table(train_set$Class))
    prop.table(table(test_set$Class)) 
# naive bayes
    install.packages("e1071")
    library(e1071)
    library(caret)
    control= trainControl(method="repeatedcv", number=10, repeats=3)
    system.time( classifier_nb <- naiveBayes(train_set, train_set$Class, laplace = 1,
                                             trControl = control,tuneLength = 7) )
# model evaluation
    
    nb_pred = predict(classifier_nb, type = 'class', newdata = test_set)
    confusionMatrix(nb_pred,test_set$Class)
    