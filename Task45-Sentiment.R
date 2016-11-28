setwd("/coursera/task4")

## Phoenix Exploratory
# Subset by City = Phoenix

# datB_PHX <- subset(datB, city == "Phoenix") # 8410 obs
# datT_PHX <- subset(datT, business_id %in% datB_PHX$business_id) # 73k obs
# datR_PHX <- subset(datR, business_id %in% datB_PHX$business_id) # 219k obs
# datU_PHX <- subset(datU, user_id %in% datR_PHX$user_id) # 65k

datB_PHX <- readRDS("datB_PHX.rds") # 8410 obs
datT_PHX <- readRDS("datT_PHX.rds") # 73k obs
datR_PHX <- readRDS("datR_PHX.rds") # 219k obs
datU_PHX <- readRDS("datU_PHX.rds") # 65k

# Selects users with fans
fanscount <- subset(datU_PHX, fans >= 0)
#fanscount[is.na(fanscount)] <- 0
 
# Select all users 
datRU_PHX <- subset(datR_PHX, user_id %in% fanscount$user_id) # 219k obs

# add User info
datRU_PHX <- merge(datRU_PHX, fanscount, by = "user_id", all.x = TRUE)

# add business info
datRUB_PHX <- merge(datRU_PHX, datB_PHX, by = "business_id", all.x = TRUE)

rm(datRU_PHX)
rm(fanscount)
rm(datB_PHX)
rm(datR_PHX)
rm(datT_PHX)
rm(datU_PHX)

# Remove irrelevant columns
myvars <- names(datRUB_PHX) %in% c("yelping_since", "friends", "type.y",
                                   "compliments", "elite", "full_address",
                                   "city", "neighborhoods", "longitude",
                                   "state", "latitude", "type.x", "name.x",
                                   "open", "type", "stars.y", "review_count.y",
                                   "votes.y")
datRUB_PHX <- datRUB_PHX[!myvars]

library(jsonlite)

# Calculates Hours on Weekdays and on Weekends
hoursW <- datRUB_PHX$hours
hoursW <- flatten(hoursW)

hoursW$Monday.close <- strptime(hoursW$Monday.close, format="%H:%M")
hoursW$Monday.open <- strptime(hoursW$Monday.open, format="%H:%M")
hoursW$Tuesday.close <- strptime(hoursW$Tuesday.close, format="%H:%M")
hoursW$Tuesday.open <- strptime(hoursW$Tuesday.open, format="%H:%M")
hoursW$Wednesday.close <- strptime(hoursW$Wednesday.close, format="%H:%M")
hoursW$Wednesday.open <- strptime(hoursW$Wednesday.open, format="%H:%M")
hoursW$Thursday.close <- strptime(hoursW$Thursday.close, format="%H:%M")
hoursW$Thursday.open <- strptime(hoursW$Thursday.open, format="%H:%M")
hoursW$Friday.close <- strptime(hoursW$Friday.close, format="%H:%M")
hoursW$Friday.open <- strptime(hoursW$Friday.open, format="%H:%M")

hoursW$WKD <- abs(hoursW$Monday.open - hoursW$Monday.close)/60/60 +
  abs(hoursW$Tuesday.open - hoursW$Tuesday.close)/60/60 +
  abs(hoursW$Wednesday.open - hoursW$Wednesday.close)/60/60 +
  abs(hoursW$Thursday.open - hoursW$Thursday.close)/60/60 +
  abs(hoursW$Friday.open - hoursW$Friday.close)/60/60

hoursW$WKD[is.na(hoursW$WKD)] <- 0

hoursW$Saturday.close <- strptime(hoursW$Saturday.close, format="%H:%M")
hoursW$Saturday.open <- strptime(hoursW$Saturday.open, format="%H:%M")
hoursW$Sunday.close <- strptime(hoursW$Sunday.close, format="%H:%M")
hoursW$Sunday.open <- strptime(hoursW$Sunday.open, format="%H:%M")

hoursW$WKND <- abs(hoursW$Sunday.open - hoursW$Sunday.close)/60/60 +
  abs(hoursW$Saturday.open - hoursW$Saturday.close)/60/60

hoursW$WKND[is.na(hoursW$WKND)] <- 0

datRUB_PHX$WKND <- hoursW$WKND
datRUB_PHX$WKD <- hoursW$WKD

# Remove irrelevant columns
myvars <- names(datRUB_PHX) %in% c("hours")
datRUB_PHX <- datRUB_PHX[!myvars]

# Characterizes categories for Restaurants (c for categ) #141k
datRUB_PHX$categ <- grepl ( "^(.*[Rr]estaurant.*)",  datRUB_PHX$categories)
datRUBc_PHX <- subset(datRUB_PHX, datRUB_PHX$categ == TRUE)  

# Subsets for votes useful
# datRUBcu_PHX <- subset(datRUBc_PHX, datRUBc_PHX$votes.x$useful > 0)
# rm(datRUBc_PHX)

datRUBcu_PHX <- datRUBc_PHX

rm(datRUBc_PHX)

# Flattens 'attributes' and select variables for restaurant
attrib <- datRUBcu_PHX$attributes
attrib <- flatten(attrib)
# Remove irrelevant columns
myvars <- names(attrib) %in% c("Accepts Credit Cards", 
                               "Good For Groups", "Outdoor Seating", "Price Range",  
                               "Good for Kids", "Alcohol",  "Noise Level", "Has TV",  
                               "Attire", "Good For Dancing", "Delivery", "Coat Check",  
                               "Smoking", "Take-out",  "Takes Reservations",   "Waiter Service",  
                               "Wi-Fi",  "Caters",  "Drive-Thru",  "Wheelchair Accessible",  
                               "BYOB", "Corkage",  "BYOB/Corkage",  
                               "Order at Counter", "Good For Kids",  "Dogs Allowed",  
                               "Open 24 Hours", "Parking.garage", "Parking.street",  
                               "Parking.validated", "Parking.lot",  "Parking.valet")
attrib <- attrib[myvars]

datRUBcua_PHX <- cbind(datRUBcu_PHX, attrib)

rm(datRUBcu_PHX)
rm(attrib)

myvars <- names(datRUBcua_PHX) %in% c("attributes", "categories")
datRUBcua_PHX <- datRUBcua_PHX[!myvars]

###########################################################################
# Sentiment analysis for 'text'
###########################################################################
#  

library(sentiment)

fortext <- datRUBcua_PHX[120001:130000,]

vectexto <- fortext$text

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
sent_df_13a = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)


# sent <- rbind(sent_df_1, sent_df_2, sent_df_3, sent_df_4, sent_df_5,
#               sent_df_6, sent_df_7, sent_df_8, sent_df_9, sent_df_10,
#               sent_df_11, sent_df_12, sent_df_13a, sent_df_13)

saveRDS(sent, file = "sent.rds")
saveRDS(datRUBcua_PHX, file = "datRUBcua_PHX.rds")



datRUBcuas_PHX_full <- cbind(datRUBcua_PHX, sent)

saveRDS(datRUBcuas_PHX_full, file = "datRUBcuas_PHX-full.rds")



#############################################################################

rm(class_pol)
rm(class_emo)
rm(datRUBcua_PHX)
rm(fortext)
rm(sent)
rm(emotion)
rm(polarity)
rm(some_txt)
rm(vectexto)
rm(hoursW)

# Final dataframe with sentiment columns
datRUBcuas_PHX_full <- readRDS("datRUBcuas_PHX-full.rds")


