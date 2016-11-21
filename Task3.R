setwd("/coursera/task3")

library(tm)
require(plyr)   
require(text2vec)

rm(list = ls())

x <- readLines("italian.txt")  # read data with readLines
nx <- !nchar(x)            # locate lines with only empty strings
# create final data.frame
out <- rbind.fill(lapply(split(x[!nx], cumsum(nx)[!nx]),function(x) data.frame(t(x))))               
out2<- do.call(paste, c(out[c(1:ncol(out))], sep = " "))


## Text prep
##
vectexto <- out2

texto <- vectexto[ !is.na( vectexto ) ]

# Clean corpus
myCorpus <- Corpus(VectorSource(texto))
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
myCorpus <- tm_map(myCorpus, stripWhitespace)

dataframe<-data.frame(text=unlist(sapply(myCorpus, `[`, "content")), stringsAsFactors=F)

inputtext <- paste(dataframe[,1])

# Create iterator over tokens
tokens <- space_tokenizer(inputtext)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)

top_terms <- arrange(vocab$vocab, desc(terms_counts))[1:20,]
bottom_terms <- arrange(vocab$vocab, terms_counts)[1:20,]

# Min recurrence 20
vocab <- prune_vocabulary(vocab, term_count_min = 20L, doc_proportion_max = 0.5, doc_proportion_min = 0.001)


# Use our filtered vocabulary
vectorizer <- vocab_vectorizer(vocab, 
                               # don't vectorize input
                               grow_dtm = FALSE, 
                               # use window of 5 for context words
                               skip_grams_window = 5L)
# term-co-occurence matrix (TCM).
tcm <- create_tcm(it, vectorizer)

#  factorize it via the GloVe algorithm
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)

# fit model
glove$fit(tcm, n_iter = 20)

word_vectors <- glove$get_word_vectors()

italian_vec.df <- as.data.frame(word_vectors)

top_terms <- arrange(vocab$vocab, desc(terms_counts))[1:20,]

NewWord <- word_vectors["minestrone", , drop = FALSE] + 
  word_vectors["pomodoro", , drop = FALSE] + 
  word_vectors["pesto", , drop = FALSE]


cos_sim = sim2(x = word_vectors, y = NewWord, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)



