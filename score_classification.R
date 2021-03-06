
library(tm)
library(stringr)
library(dplyr)
library(e1071)
 
load(file = "informacion.Rdata")

load( file = "questions_2015.Rdata")

# We are going to apply preprocesing text mining functions 

questions_corpus <- Corpus(VectorSource(questions$body))


remove_lat <- function(x){gsub("<p>","",x)}
questions_corpus = tm_map(questions_corpus, remove_lat)
questions_corpus = tm_map(questions_corpus, removeNumbers)
questions_corpus = tm_map(questions_corpus, removePunctuation)
questions_corpus = tm_map(questions_corpus, removeWords, c( "function","for",stopwords("english")))
questions_corpus =  tm_map(questions_corpus, stripWhitespace)

questions_dtm <- DocumentTermMatrix(questions_corpus,control = list(weighting = weightTfIdf))
questions_dtm = removeSparseTerms(questions_dtm, 0.95)

inspect(questions_dtm)


# We want to discretize the score variable: 

for(i in 1:nrow(questions)){
  if(questions[i,6] < 0){
    questions[i,6] = -1
  }
  if(questions[i,6] > 0){
    questions[i,6] = 1
  }
}




# We build the models

questions_mining <- cbind(questions$score,as.matrix(questions_dtm))
questions_mining <- as.data.frame(questions_mining)
colnames(questions_mining)[1] <- "score"

questions_mining$score <- as.factor(questions_mining$score)
set.seed(314)
train <- sample(1:10000,7000)
test <- sample(c(1:10000)[-train],3000)

questions.training <- questions_mining[train,]
question.testing <- questions_mining[test,]

# We train the svm model and predict the classes

model.svm <- svm(questions.training$score ~. ,data = questions.training,kernel = "polynomial",class.weights = c("-1" = 0.0235,"0" = 0.2018,"1" = 0.7747), cost = 1000, degree = 10)


pred.svm = predict(model.svm, question.testing)

# The confussion matrix

table(question.testing$score,pred.svm,dnn=c("Obs","Pred"))


# we train a naive bayes model

naive.questions <- naiveBayes(x= as.matrix(questions_dtm)[train,], y = questions_mining$score[train])

pp <- predict(naive.questions,question.testing) 

# Confussion matrix

table(pp, questions_mining$score[test]) 










