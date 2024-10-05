###import the data
library(readr)
dataset=read.delim("train.tsv")


###cleaning the texts
library(tm)
library(SnowballC)
library(dplyr)
colnames(dataset) <- c("ID", "rating", "comments")

View(dataset)
data<-dataset
data<-na.omit(data)
data$rating <- as.factor(data$rating)

str(data$rating)
data



##NLP


Corpus=VCorpus(VectorSource(data$comments))
Corpus=tm_map(Corpus,content_transformer(tolower))
Corpus=tm_map(Corpus,removeNumbers)
Corpus=tm_map(Corpus,removePunctuation)
Corpus=tm_map(Corpus,removeWords,stopwords())
Corpus=tm_map(Corpus,stemDocument)
Corpus=tm_map(Corpus,stripWhitespace)
as.character(Corpus[[1]])






dtm=DocumentTermMatrix(Corpus)
dtm=removeSparseTerms(dtm,0.99)

data1=as.data.frame(as.matrix(dtm))
data1$ratings=data$rating



str(data1$ratings)



###random forest


library(caTools)

set.seed(123)
split=sample.split(data1$ratings,SplitRatio = 0.8)
training_set=subset(data1,split==TRUE)
test_set=subset(data1,split==FALSE)




library(randomForest)

classifier=randomForest(x=training_set[-193],
                        y=training_set$ratings,
                   ntree=20)

##prediction

y_pred=predict(classifier,newdata = test_set[-193])

##confusion matrix

cm=table(table_set[,193],y_pred)
                


varImpPlot(classifier,main = "variable importance")
