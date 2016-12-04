setwd("/coursera/task6")


rm(list = ls())

x <- readLines("hygiene.dat") 
reviews <- as.data.frame(x)

rm(x)

###########################################################################
# Sentiment analysis for 'text'
###########################################################################
#  

library(sentiment)

fortext <- reviews[351:546,]

vectexto <- fortext

#texto <- vectexto[ !is.na( vectexto ) ]

some_txt <- vectexto

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)

# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"


# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df_4= data.frame(text=some_txt, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)

sent <- rbind(sent_df, sent_df_2, sent_df_3, sent_df_4)

write.csv(sent, "revsent.csv")

#saveRDS(sent_df, file = "sent.rds")


#datRUBcuas_PHX_full <- cbind(datRUBcua_PHX, sent)
#saveRDS(datRUBcuas_PHX_full, file = "datRUBcuas_PHX-full.rds")
#datRUBcuas_PHX_full <- readRDS("datRUBcuas_PHX-full.rds")
