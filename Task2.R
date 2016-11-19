setwd("/coursera/task2")

#require(plyr)               

rm(list = ls())

x <- readLines("Gluten-Free.txt")  # read data with readLines
nx <- !nchar(x)            # locate lines with only empty strings
# create final data.frame
out <- rbind.fill(lapply(split(x[!nx], cumsum(nx)[!nx]),function(x) data.frame(t(x))))               
out2<- do.call(paste, c(out[c(1:ncol(out))], sep = " "))

#library(tm)

## Text prep
##
vectexto <- out2

texto <- vectexto[ !is.na( vectexto ) ]

myCorpus <- Corpus(VectorSource(texto))

#myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove stopwords
myStopwords <- c(stopwords('english'))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myStopwords <- c('can', 'say','one','way','use',
                 'also','howev','tell','will',
                 'much','need','take','tend','even',
                 'like','particular','rather','said',
                 'get','well','make','ask','come','end',
                 'first','two','help','often','may',
                 'might','see','someth','thing','point',
                 'post','look','right','now','think','ve',
                 're ','anoth','put','set','new','good',
                 'want','sure','kind','larg','yes,','day','etc',
                 'quit','sinc','attempt','lack','seen','awar',
                 'littl','ever','moreov','though','found','abl',
                 'enough','far','earli','away','achiev','draw',
                 'last','never','brief','bit','entir','brief',
                 'great','lot','tri','ive','say','went','locat',
                 'want','dont','got','even','didnt','way','thing')
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
                 
myStopwords <- c('use','mani','next','still','sinc','appet','two','noth',
                 'that','chevi','overal','owner','year','minut','run','walk',
                 'let','must','hit','insid','now','bring','els','guy','theyr',
                 'dri','ate','min','soooo','abl','ami','someon','arent','thru',
                 'soo','<U+63A8><U+8350><U+76AE><U+86CB><U+7626><U+8089><U+7CA5><U+3002><U+3002>',
                 'waaaay','rang','abbi','tir','kaow','kee','TRUE','guin','ing','asu','ach',
                 'soooooooo','itd','sooooo','lit','yous','cuz','mcds','waaayyy','phx','wwwdeepfriedkimcheecom',
                 'Iwe','mabo','uni','fin','alo','byblo','annnnnnnnnnnnd','von','httpwwwyelpcombizbibwgorznnlmlbpnxg',
                 'httpwwwyelpcombizifldhmppmrcxhuda','thankyoumayihaveanother','sighusu',
                 'ces','ehid','leinenkugel','wil','xeo','bruddah','gfs','ref','ossam','tok','beij',
                 'ssoosososoooo','googl','ucsd','httpwwwyelpcombizdelfriscosdoubleeaglesteakhouselasvegashridczikerqueyeflqga',
                 'brrrraaaiiiinnnni','maaaaaiiinnnnnssss','na')
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, stemDocument)
myCorpus <- tm_map(myCorpus, stripWhitespace)
#inspect(myCorpus)
#writeLines(as.character(myCorpus[[3]]))

# TF                
# Create Doc Term Matrix
myDtm <- DocumentTermMatrix(myCorpus, control = c(weight = weightTf))
myDtm
#dim(myDtm)
# Collapses Term Matrix
cDtm <- removeSparseTerms(myDtm, 0.99)
cDtm

freq <- colSums(as.matrix(cDtm))   
length(freq)   
ord <- order(freq)
fileTF <- freq[tail(ord,100)] 
fileTF <- as.data.frame(freq[tail(ord,100)])
lapply(rownames(fileTF), write, "fileTF.txt", append=TRUE, ncolumns=1000)
  
#  IDF
# Create Doc Term Matrix
myDtmIDF <- DocumentTermMatrix(myCorpus,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =
                                                           FALSE),
                                         stopwords = TRUE))
myDtmIDF
#dim(myDtm)
# Collapses Term Matrix
cDtmIDF <- removeSparseTerms(myDtmIDF, 0.99)
cDtmIDF
          
freq <- colSums(as.matrix(cDtmIDF))   
length(freq)   
ord <- order(freq)
freq[tail(ord,100)] 
fileIDF <- as.data.frame(freq[tail(ord,100)])
fileIDF
lapply(rownames(fileIDF), write, "fileIDF.txt", append=TRUE, ncolumns=1000)

   
# LDA Modeling
#
#require(lda)
#require(topicmodels)
text <- dtm2ldaformat(cDtm, omit_empty = FALSE)
ldaModel=lda.collapsed.gibbs.sampler(text$documents,K=10,vocab=text$vocab,burnin=9999,num.iterations=1000,alpha=1,eta=0.1)
top.words <- top.topic.words(ldaModel$topics, 10, by.score=TRUE)
print(top.words)
fileLDA <- as.list(top.words)
fileLDA
lapply(fileLDA, write, "fileLDA.txt", append=TRUE, ncolumns=1000)
                 
