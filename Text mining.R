setwd("C:/Users/Pierrick/Desktop/")
install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC") 
install.packages("RColorBrewer")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(tm)
library(wordcloud)


MyData <- read.csv2("Inno.csv", header=T,fill=T,  row.names=NULL)
head(MyData)

str(MyData)

madata <- Corpus(VectorSource(MyData))
inspect(madata)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
madata <- tm_map(madata, toSpace, "/")
madata <- tm_map(madata, toSpace, "@")
madata <- tm_map(madata, toSpace, "\\|")

madata <- tm_map(madata, content_transformer(tolower))

madata <- tm_map(madata, removeWords, stopwords("english"))

madata <- tm_map(madata, stemDocument)

'''lords <- tm_map(lords, removeWords, c(“noble”, “lord”))'''

dtm <- TermDocumentMatrix(madata)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Cloudword
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))


#Frequents terms
findFreqTerms(dtm, 50)

#Find association with correlation above 0.8
findAssocs(dtm, "administr", 0.9)


# TF_IDF ponderation gives more weight to less used terms
dtm_tfxidf <- weightTfIdf(dtm)

# Clustering
m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)

norm_eucl <- function(m)
  m/apply(m, 1, function(x) sum(x^2)^.5)

m_norm <- norm_eucl(m)

#Clustering
results <- kmeans(m_norm, 5, 30)


#Terms that appears at least 5 times
clusters <- 1:5
for (i in clusters){
  cat("Cluster ", i, ':', findFreqTerms(dtm_tfxidf[results$cluster == i], 10), "\n\n")
}



