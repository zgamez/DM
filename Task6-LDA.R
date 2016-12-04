setwd("/coursera/task6")


rm(list = ls())

x <- readLines("hygiene.dat") 
reviews <- as.data.frame(x)

rm(x)

library(tm)

missing <- c(6, 90, 117, 132, 170, 176, 210, 218, 220, 232, 275, 336, 348, 350, 351, 353, 370, 378, 381, 383, 467, 488, 495, 506, 512)

for (index in missing){
print(index)
## Text prep
##
vectexto <- reviews[index,]

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
#writeLines(as.character(myCorpus[[1]]))

# TF                
# Create Doc Term Matrix
myDtm <- DocumentTermMatrix(myCorpus, control = c(weight = weightTf))
#myDtm


# LDA Modeling
#
require(lda)
require(topicmodels)
text <- dtm2ldaformat(myDtm, omit_empty = FALSE)
ldaModel=lda.collapsed.gibbs.sampler(text$documents,K=3,vocab=text$vocab,burnin=9999,num.iterations=1000,alpha=1,eta=0.1)
top.words <- top.topic.words(ldaModel$topics, 1, by.score=TRUE)
print(top.words)
reviews[index,2] <- top.words[[1]]
reviews[index,3] <- top.words[[2]]
reviews[index,4] <- top.words[[3]]
}

write.csv(reviews, "reviews.csv")
