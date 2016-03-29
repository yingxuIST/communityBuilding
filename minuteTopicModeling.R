### set a working directory
getwd()
setwd("/Users/YingXu/Dropbox/2016Zaatari/minuteTopicModeling")

### install packages

install.packages("tm") #text mining: preprocess data
install.packages("lda") #latent Dirichlet allocation

#1. before installing topicmodels package, insall homebrew from terminal, 
#ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
#2. and then install gsl in terminal by using: brew install gsl
#3. find directory of brew installed gsl
#4. copy respective files from gsl into -I/opt/local/Library/Frameworks/R.framework/Resources/include,
# as error messages showed.

install.packages("topicmodels")

library(topicmodels)
library(lda)
library(tm)

######################################################
### read data

minutes <- read.csv("template.csv")
names(minutes)
head(minutes)


######################################################

###check on the discrepancies between attendance and concerned NGOs



######################################################
####### Topic Modleing with Minutes Details ##########

###pre-process data into matrices

#create minutes detail corpus
minutes.detail = Corpus(VectorSource(minutes$Details.incidents))

#transform to lower case
#minutes.detail = tm_map(minutes.detail, content_transformer(tolower))

#remove potentially problematic symbols
#toSpace = content_transformer(function(x,pattern) {return (gsub(pattern," ",x))})
#minutes.detail = tm_map(minutes.detail, toSpace, "-")
#minutes.detail = tm_map(minutes.detail, toSpace, "'")

#remove punctuation
#minutes.detail = tm_map(minutes.detail, removePunctuation)

#remove stopwords
minutes.detail = tm_map(minutes.detail, removeWords, stopwords("SMART"))

#remove whitespace
#minutes.detail= tm_map(minutes.detail, stripWhitespace)

#write lines to check the process
writeLines(as.character(minutes.detail[2]))
Document

#create document-term matrices
minutes.detail.matrix = DocumentTermMatrix(minutes.detail,
                        control = list(removePunctuation = TRUE, removeNumber = TRUE, stopwords = TRUE))
minutes.detail.matrix


###topic modeling with "topicmodels"

#set parameters for Gibbs sampling
burnin = 4000
iter = 2000
thin = 50
seed = list(2003, 5, 63, 100001, 765)
nstart = 5
best = TRUE

# set number of topics
k = 5

# run lda using Gibbs samplign
minute.detail.lda = LDA(minutes.detail.matrix, k, method = "Gibbs",control = list(nstart = nstart, 
                    seed = seed, best = best, burnin = burnin, iter = iter, thin = thin))

# turning lda results to topics
minutes.detail.topics = as.matrix(topics(minute.detail.lda))
minutes.detail.topics
write.csv(minutes.detail.topics, file = paste("LDAGibbs", k, "DocsToTopics.csv"))

#topic 6 terms in each topic
minutes.detail.terms = as.matrix (terms(minute.detail.lda,6))
minutes.detail.terms
write.csv(minutes.detail.terms, file=paste("LDAGibss", k, "TopicstoTerms.csv"))

#probabilities associated with each topic
minutes.detail.topics.probabilities = as.data.frame(minute.detail.lda@gamma)
write.csv(minutes.detail.topics.probabilities, file=paste("LDAGibbs", k, "TopicProbabilites.csv"))

#find relative importance of top 2 topics
topic1AndTopic2 = lapply(1:nrow(minutes.detail.matrix), function(x)
  sort(minutes.detail.topics.probabilities[x,]) [k]/sort(minutes.detail.topics.probabilities[x,])[k-1])

#find relative importance of the second and the third most important topics
topic2AndTopic3 = lapply(1:nrow(minutes.detail.matrix), function(x)
  sort(minutes.detail.topics.probabilities[x,]) [k-1]/sort(minutes.detail.topics.probabilities[x,])[k-2])

#write
write.csv(topic1AndTopic2, file=paste("LDAGibbs",k,"Topic1andTopic2.csv"))
write.csv(topic2AndTopic3, file=paste("LDAGibbs",k,"Topic2andTopic3.csv"))



### Within District / Cross Topic


### Cross District / Within Topic


######################################################
######## Topic Modleing with NGOs Response ###########

###pre-process data into matrices

#create minutes detail corpus
minutes.detail = Corpus(VectorSource(minutes$Details.incidents))

#transform to lower case
#minutes.detail = tm_map(minutes.detail, content_transformer(tolower))

#remove potentially problematic symbols
#toSpace = content_transformer(function(x,pattern) {return (gsub(pattern," ",x))})
#minutes.detail = tm_map(minutes.detail, toSpace, "-")
#minutes.detail = tm_map(minutes.detail, toSpace, "'")

#remove punctuation
#minutes.detail = tm_map(minutes.detail, removePunctuation)

#remove stopwords
minutes.detail = tm_map(minutes.detail, removeWords, stopwords("SMART"))

#remove whitespace
#minutes.detail= tm_map(minutes.detail, stripWhitespace)

#write lines to check the process
writeLines(as.character(minutes.detail[2]))
Document

#create document-term matrices
minutes.detail.matrix = DocumentTermMatrix(minutes.detail,
                                           control = list(removePunctuation = TRUE, removeNumber = TRUE, stopwords = TRUE))
minutes.detail.matrix


###topic modeling with "topicmodels"

#set parameters for Gibbs sampling
burnin = 4000
iter = 2000
thin = 50
seed = list(2003, 5, 63, 100001, 765)
nstart = 5
best = TRUE

# set number of topics
k = 5

# run lda using Gibbs samplign
minute.detail.lda = LDA(minutes.detail.matrix, k, method = "Gibbs",control = list(nstart = nstart, 
                                                                                  seed = seed, best = best, burnin = burnin, iter = iter, thin = thin))

# turning lda results to topics
minutes.detail.topics = as.matrix(topics(minute.detail.lda))
minutes.detail.topics
write.csv(minutes.detail.topics, file = paste("LDAGibbs", k, "DocsToTopics.csv"))

#topic 6 terms in each topic
minutes.detail.terms = as.matrix (terms(minute.detail.lda,6))
minutes.detail.terms
write.csv(minutes.detail.terms, file=paste("LDAGibss", k, "TopicstoTerms.csv"))

#probabilities associated with each topic
minutes.detail.topics.probabilities = as.data.frame(minute.detail.lda@gamma)
write.csv(minutes.detail.topics.probabilities, file=paste("LDAGibbs", k, "TopicProbabilites.csv"))

#find relative importance of top 2 topics
topic1AndTopic2 = lapply(1:nrow(minutes.detail.matrix), function(x)
  sort(minutes.detail.topics.probabilities[x,]) [k]/sort(minutes.detail.topics.probabilities[x,])[k-1])

#find relative importance of the second and the third most important topics
topic2AndTopic3 = lapply(1:nrow(minutes.detail.matrix), function(x)
  sort(minutes.detail.topics.probabilities[x,]) [k-1]/sort(minutes.detail.topics.probabilities[x,])[k-2])

#write
write.csv(topic1AndTopic2, file=paste("LDAGibbs",k,"Topic1andTopic2.csv"))
write.csv(topic2AndTopic3, file=paste("LDAGibbs",k,"Topic2andTopic3.csv"))



### Within District / Cross Topic


### Cross District / Within Topic
