### set a working directory
getwd()
setwd("/Users/YingXu/Dropbox/2016Zaatari/minuteTopicModeling/data")

### install packages

install.packages("tm") #text mining: preprocess data
install.packages("lda") #latent Dirichlet allocation

#1. before installing topicmodels package, insall homebrew from terminal, 
#ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
#2. and then install gsl in terminal by using: brew install gsl
#3. find directory of brew installed gsl
#4. copy respective files from gsl into -I/opt/local/Library/Frameworks/R.framework/Resources/include,
# as error messages showed.

install.packages("topicmodels") #topicmodels
install.packages("LDAvis") #LDA visualization
install.packages('servr') 

library(topicmodels)
library(lda)
library(tm)
library(LDAvis)
library(servr)

######################################################
### read data

minutes_men <- read.csv("MinutesMan.csv")
minutes_women <- read.csv("MinutesWoman.csv")
names(minutes_men)
names(minutes_women)

######################################################
###check on the discrepancies between attendance and concerned NGOs

# if concerned ngo is TRUE, check whether they are in the name of participation

######################################################
####### Topic Modleing with Minutes Details ##########

#get minutes details
minutes_men$detail.full = paste(minutes_men$Details.incidents,minutes_men$Causes.Gaps,
                                minutes_men$Proposed_Solutions)
minutes.detail = minutes_men$detail.full

minutes_men$response.full = paste(minutes_men$Sector_response, minutes_men$Agreements_Action_Points)
minutes.detail = minutes_men$response.full

########## main function ##########
#replace or delete stop words and theme-related words
stop_words = stopwords("SMART")
minutes.detail=gsub("'","", minutes.detail)
minutes.detail = gsub("[[:punct:]]"," ", minutes.detail)
minutes.detail = gsub("[[:cntrl:]]", " ",minutes.detail)
minutes.detail = gsub("don\xfc\xbe\x8c\x83\xa4\xbct", "", minutes.detail)
minutes.detail = gsub("can\xfc\xbe\x8c\x83\xa4\xbct", "", minutes.detail)
minutes.detail = gsub("it\xfc\xbe\x8c\x83\xa4\xbcs", "", minutes.detail)
minutes.detail = gsub("aren\xfc\xbe\x8c\x83\xa4\xbct", "", minutes.detail)
minutes.detail = gsub("s\xfc\xbe\x8c\x83\xa4\xbc", "", minutes.detail)
minutes.detail = gsub("that\xfc\xbe\x8c\x83\xa4\xbcs", "", minutes.detail)
minutes.detail = gsub("there\xfc\xbe\x8c\x83\xa4\xbcs", "", minutes.detail)
minutes.detail = gsub("doesn\xfc\xbe\x8c\x83\xa4\xbct", "", minutes.detail)
minutes.detail = gsub("ngo\xfc\xbe\x8c\x83\xa4\xbcs", "", minutes.detail)

minutes.detail = gsub('[[:digit:]]+', " ", minutes.detail)
minutes.detail = tolower(minutes.detail)

minutes.detail = gsub("refugee","", minutes.detail)
minutes.detail = gsub("camp","", minutes.detail)
minutes.detail = gsub("pm","", minutes.detail)
minutes.detail = gsub("dont","", minutes.detail)
minutes.detail = gsub("person","", minutes.detail)
minutes.detail = gsub("people","", minutes.detail)
minutes.detail = gsub("st","", minutes.detail)
minutes.detail = gsub("days","", minutes.detail)
minutes.detail = gsub("proper","", minutes.detail)
#minutes.detail = gsub("na","", minutes.detail)


#tokenize on space and output a a list
minutes.detail.list = strsplit(minutes.detail, "[[:space:]]+")

#compute the table of terms
minutes.detail.term = table(unlist(minutes.detail.list))
minutes.detail.term = sort(minutes.detail.term, decreasing = TRUE)

#remove terms hat are stop words or ocur fewer than 5 times:
delete = names(minutes.detail.term) %in% stop_words | minutes.detail.term < 5
minutes.detail.term = minutes.detail.term [!delete]
vocab = names (minutes.detail.term)

# put the documents into the format required by lda
minutes.detail.getting.term = function (x) {
  index = match(x, vocab)
  index = index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
minutes.detail.document = lapply(minutes.detail.list, minutes.detail.getting.term)

d = length(minutes.detail.document) 
w = length(vocab) 
doc.length = sapply(minutes.detail.document, function(x) sum(x[2,]))
N = sum(doc.length) 
term.frequency = as.integer(minutes.detail.term)

##lda modeling
# MCMC and Model tuning parameters:
k = 10
G = 5000
alpha = 0.02
eta = 0.02

#fit the model:
set.seed(27)
minutes.detail.lda.fit = lda.collapsed.gibbs.sampler(documents = minutes.detail.document, K = k, vocab = vocab, 
                                                     num.iterations = G, alpha = alpha, eta = eta, initial = NULL,
                                                     burnin = 0, compute.log.likelihood = TRUE)

##visualizing with LDAvis
theta = t(apply(minutes.detail.lda.fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi = t(apply(t(minutes.detail.lda.fit$topics) + eta, 2, function(x) x/sum(x)))

minutesDetail.men = list(phi = phi, theta = theta, doc.length = doc.length, vocab = vocab, term.frequency = term.frequency)

# create JSON file
minutes.detail.men.json = createJSON(phi = minutesDetail.men$phi,   #topic*vocab
                                     theta = minutesDetail.men$theta, #document*topic
                                     doc.length = minutesDetail.men$doc.length, #length of each document
                                     vocab = minutesDetail.men$vocab, 
                                     term.frequency = minutesDetail.men$term.frequency,
                                     plot.opts = list(xlab = "Principle Component 1", ylab = "Principle Component 2"))

#generate interactive visualization
serVis(minutes.detail.men.json, out.dir = 'vis_responses', open.brower = FALSE)




### Within District / Cross Topic


### Cross District / Within Topic


######################################################
######## Topic Modleing with NGOs Response ###########
