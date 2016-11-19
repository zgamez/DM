setwd("/coursera/task2")


library(tm)

#load files into corpus
#get listing of .txt files in directory
filenames <- list.files(getwd(), pattern='*.txt')

#read files into a character vector
files <- lapply(filenames,readLines)


#create corpus from vector
docs <- Corpus(VectorSource(files))

#inspect a particular document in corpus
writeLines(as.character(docs[[30]]))

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)

library(slam)
cosine_dist_mat <- 1 - crossprod_simple_triplet_matrix(tdm)/(sqrt(col_sums(tdm^2) %*% t(col_sums(tdm^2))))
m<-as.matrix(cosine_dist_mat)
#write as csv file
write.csv(m,file='cosine_dist_mat.csv')
#Map filenames to matrix row numbers
#these numbers will be used to reference
#files in the network graph
filekey <- cbind(rownames(m),filenames)
write.csv(filekey,'filekey.csv')

fmtFile <- read.csv(file ='cosine_dist_mat.csv', header = TRUE, row.names = 1)

# create heatmap and don't reorder columns
heatmap(as.matrix(fmtFile), Rowv=NA, Colv=NA, scale='none', col = cm.colors(256), 
                       margins=c(5,10))
#HeatMap with cluster dendogram
#install if necessary
#install.packages("gplots")
library(gplots)
#heatmap.2(as.matrix(fmtFile))
#using a red and blue colour scheme without traces and scaling each row
library("RColorBrewer")
heatmap.2(as.matrix(fmtFile),col=brewer.pal(11,"RdBu"),scale="row", trace="none")

#Kmeans Clusters
library(fpc)   
library(cluster)

d <- dist(t(fmtFile), method="euclidean")  

#kmeans - determine the optimum number of clusters (elbow method)
#look for "elbow" in plot of summed intra-cluster distances (withinss) as fn of k
wss <- 2:29
for (i in 2:29) wss[i] <- sum(kmeans(d,centers=i,nstart=25)$withinss)
plot(2:29, wss[2:29], type='b', xlab='Number of Clusters',ylab='Within groups sum of squares')

kfit <- kmeans(d, 7)   
clusplot(as.matrix(d), main='LDA Clusters', kfit$cluster, color=T, shade=T, labels=2, lines=0)  

#Multidimensional Scaling (MDS)

fit <- cmdscale(t(fmtFile), eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x, y, pch = 19, xlim = range(x) + c(0, 0.2))
pl.names <- colnames(fmtFile)
text(x, y, pos = 4, labels = pl.names)

