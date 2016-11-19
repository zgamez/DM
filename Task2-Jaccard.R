setwd("/coursera/task2")


library(tm)

#load files into corpus
#get listing of .txt files in directory
filenames <- list.files(getwd(), pattern='*.txt')

#read files into a character vector
files <- lapply(filenames,readLines)
docs <- Corpus(VectorSource(files))

#inspect a particular document in corpus
writeLines(as.character(docs[[30]]))

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)

library("Matrix") 
mat <- sparseMatrix(i=tdm$i, j=tdm$j, x=tdm$v, dims=c(tdm$nrow, tdm$ncol))
mat[1,1:5]
head(as.vector(tdm[1,]), 5)
mat <- as.matrix(mat)

out <- crossprod(mat)  # Same as: t(X) %*% X
diag(out) <- 0       # (b/c you don't count co-occurrences of an aspect with itself)
out <-as.matrix(out)

library(vegan)
jcc.mat<-as.matrix(vegdist(out,method="jaccard"))


#write as csv file
write.csv(jcc.mat,file='jaccard_dist_mat.csv')
#Map filenames to matrix row numbers
filekey <- cbind(rownames(jcc.mat),filenames)
write.csv(filekey,'filekey.csv')

fmtFile <- read.csv(file ='jaccard_dist_mat.csv', header = TRUE, row.names = 1)

# create heatmap and don't reorder columns
heatmap(as.matrix(fmtFile), Rowv=NA, Colv=NA, scale='none', col = cm.colors(256), margins=c(5,10))
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
clusplot(as.matrix(d), main='Clusters', kfit$cluster, color=T, shade=T, labels=2, lines=0)  
