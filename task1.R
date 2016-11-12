#install.packages("SnowballC")
#install.packages("lda")
#install.packages("LDAvis")
#install.packages("stringi")

setwd("/coursera")

library(jsonlite)
library(tidyjson)
library(dplyr) 


# read in individual JSON lines
json_file <- "yelp_academic_dataset_business.json"

# turn it into a proper array by separating each object with a "," and
# wrapping that up in an array with "[]"'s.

datB <- fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=","))) 
dim(datB)
str(datB)


json_file <- "yelp_academic_dataset_review.json"
datR <- fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))
dim(datR)
str(datR)

saveRDS(datB, "datB.rds")
saveRDS(datR, "datR.rds")

datB <- readRDS("datB.rds")
datR <- readRDS("datR.rds")

table(datB$state)

# Selects main cities to compare
datB_s <- subset(datB, state == "AZ")# | state == "NV") #  
datR_s <- subset(datR, business_id %in% datB_s$business_id) #  

# Merge Reviews with Business
datRB <- merge(datR_s, datB_s, by = "business_id", all.x = TRUE)

rm(datB)
rm(datR)
rm(datB_s)
rm(datR_s)

# Characterizes categories for Restaurants (c for categ) #668k
datRB$categ <- grepl ( "^(.*[Rr]estaurant.*)",  datRB$categories)
datRBr_US <- subset(datRB, datRB$categ == TRUE)  


rm(datRB)

# 1% Sample
set.seed(1)
datRBr_sam <- datRBr_US[sample(1:nrow(datRBr_US), 0.01*nrow(datRBr_US)), ]

rm(datRBr_US)

library(tm)

## Text prep
##
vectexto <- datRBr_sam$text

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
                 'post','look','right','now','think',''ve ',
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
                 'brrrraaaiiiinnnni','maaaaaiiinnnnnssss')
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, stemDocument)
myCorpus <- tm_map(myCorpus, stripWhitespace)
#inspect(myCorpus)
#writeLines(as.character(myCorpus[[1]]))

# Create Doc Term Matrix
myDtm <- DocumentTermMatrix(myCorpus, control = c(weight = weightTf))
myDtm
#dim(myDtm)

# Collapses Term Matrix
cDtm <- removeSparseTerms(myDtm, 0.99)
cDtm

rm(datRBr_sam)
rm(datRBr_US)


# LDA Modeling
#
require(lda)

text <- dtm2ldaformat(cDtm, omit_empty = FALSE)

ldaModel=lda.collapsed.gibbs.sampler(text$documents,K=10,vocab=text$vocab,burnin=9999,num.iterations=1000,alpha=1,eta=0.1)
top.words <- top.topic.words(ldaModel$topics, 10, by.score=TRUE)
print(top.words)


# Visualization
#

# Top words per Topics for Graph
forg <- as.table(top.words)
forg <- as.data.frame(forg)

library(igraph)

df <- data.frame(forg$Var2,forg$Freq)
df.g <- graph.data.frame(d = df, directed = FALSE)

l <- layout.kamada.kawai(df.g)

plot(df.g, vertex.shape="none", vertex.label=V(df.g)$name, 
     vertex.label.font=2, vertex.label.color="gray40", 
     vertex.label.cex=.7, edge.color="gray85")

# For interactive Plot
#tkplot(df.g, vertex.shape="none", vertex.label=V(df.g)$name, 
 #      vertex.label.font=2, vertex.label.color="red", 
  #     vertex.label.cex=.7, edge.color="gray85")


# Hubs and Authorities
hs <- hub_score(df.g, weights=NA)$vector
as <- authority_score(df.g, weights=NA)$vector
plot(df.g, vertex.size=as*30, main="Authorities")
plot(df.g, vertex.size=hs*50, main="Hubs")


#Find neighborhood of vertices
#gn <- graph.neighborhood(df.g, order = 1)
#plot(gn[[10]])


