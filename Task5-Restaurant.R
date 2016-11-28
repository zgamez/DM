#install.packages("fuzzyjoin")

setwd("/coursera/task4")

library(dplyr)
library(stringr)
library(fuzzyjoin)
library(readr)
require(plyr)   

dishes <- readr::read_csv("ItalianDishes.csv")

#reviews <- datRUBcuas_PHX_full$text

#x <- readLines("italianT.txt")  # read data with readLines
set.seed(555)
samp <- datRUBcuas_PHX_full[sample(nrow(datRUBcuas_PHX_full), 10000), ]
x <- samp$text

cReviews <- data_frame(text = samp$text, review = samp$review_id) %>% group_by(review) 


review_dishes <- cReviews %>% regex_inner_join(dishes, by = c(text = "Regex"))

colnames(review_dishes)[2] <- "review_id"


finaldf <- merge(samp, review_dishes, by="review_id")

keeps <- c("review_id","name.y", "stars.x", "average_stars", "polarity", "DISH")
vis.df <- finaldf[ , keeps, drop = FALSE]

## Visualization
## Dish ranking

detach("package:plyr", unload=TRUE)

TopDishes <- table(vis.df$DISH)

vis.df <- vis.df[vis.df$DISH %in% "pizza",] 

TopReviewed <- table(vis.df$name.y)

TopRestaurants <- group_by(vis.df, name.y, polarity) %>% 
  tally(sort = TRUE) %>% 
  filter( n == 2)


vis.df <- vis.df[vis.df$name.y %in% TopRestaurants$name.y,] 

Summ <-  group_by(vis.df, name.y, polarity, DISH)   %>% 
  summarise(avgStars = mean(stars.x, na.rm = T)) %>% 
  arrange(desc(avgStars)) 



library(lattice)

bwplot(Summ$avgStars ~ Summ$name.y |Summ$polarity,
       ylab="AvgStars", xlab="Restaurant", scales=list(cex=.7, col="blue", x=list(rot=45)),
       main="Stars vs Restaurant by Polarity", 
       layout=(c(1,3)))

       
library(ggplot2)
#Let's plot it with ggplot2
p <- ggplot(Summ, aes(fill=Summ$polarity, y=reorder(Summ$avgStars, Summ$polarity), x=Summ$name.y)) +
 geom_bar(position="dodge", stat="identity") +
 labs(title = "Stars vs Restaurant by Polarity", x = "Restaurant", y = "avgStars", color = "polarity") + 
 scale_y_discrete(breaks=c(1,2,3,4,5)) +
 theme(axis.text.x = element_text(angle = 60, hjust = 1))
p
