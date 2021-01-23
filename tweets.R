library(utf8)
library("twitteR") 
library("ROAuth") 
library(base64enc) 
library(httpuv) 
library(tm) 
library(wordcloud) 
library(wordcloud2)
library(syuzhet) 
library(lubridate) 
library(ggplot2) 
library(scales)
library(reshape2) 
library(dplyr)

cred <- OAuthFactory$new(consumerKey='BagGgBbanzbdpPNNp8Uy6TQBP', 
                         consumerSecret='pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa',
                         requestURL='https://api.twitter.com/oauth/request_token',                
                         accessURL='https://api.twitter.com/oauth/access_token',                 
                         authURL='https://api.twitter.com/oauth/authorize')

save(cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")

#Accessing Token Secret of Twitter to perform analysis
setup_twitter_oauth("BagGgBbanzbdpPNNp8Uy6TQBP", # Consumer Key (API Key)
                    "pFxap1Jzc1fClDQ9psLNU3RKSQ5FvS2PhJz8E2R7ix0cawPKfa", 
                    "1076425245521731584-Ev31ZLB7Cf0idVMqDI8BxiVG2SgRnu",  
                    "ZVUw0Z0mFrX7d6sjQxuB08l48JHhmnjmlAm86G2OPG7BS")  
Tweets <- userTimeline('Twitter', n = 1000,includeRts = T) 
TweetsDF <- twListToDF(Tweets) 
dim(TweetsDF) 
View(TweetsDF)
setwd("C:/Users/india/Desktop/data science/assignments/text mining") 
write.csv(TweetsDF, "Tweets.csv",row.names = F)
getwd()
Twitter <- read.csv(file.choose())
str(Twitter)
#Build Corpus and DTM/TDM
corpus <- Twitter$text
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:20]) 
#Performing cleansing 
corpus <- tm_map(corpus,tolower) # Converting the tweets into lowercase
inspect(corpus[1:20])
corpus <- tm_map(corpus,removePunctuation) #Removing all punctuations
inspect(corpus[1:20])
corpus <- tm_map(corpus,removeNumbers) # Removing all numbers
inspect(corpus[1:20])
corpus_clean<-tm_map(corpus,stripWhitespace) # Stripping off the extra whitespaces
inspect(corpus[1:20])
cleanset<-tm_map(corpus,removeWords, stopwords('english')) # Removing set of stopwords
inspect(cleanset[1:20])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL)) # Removing the URL links
inspect(cleanset[1:20])
cleanset<-tm_map(cleanset,removeWords, c('twitter','going','know','following','consider','best','far','least','can',
                                         'ok','okay','like','now','keep','perhaps','take','even','isnt','youre',
                                         'shouldnt','used','thats','saying','said'))
cleanset <- tm_map(cleanset, gsub,pattern = 'tweets', replacement = 'tweet') 
inspect(cleanset[1:20])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:20])
tdm <- TermDocumentMatrix(cleanset) # A matrix containing cleanset data with terms in rows and documents in columns
tdm
tdm <- as.matrix(tdm) 
tdm[1:10,1:20] 
#Bar Plot 
w <- rowSums(tdm)  
w <- subset(w, w>= 20) 
barplot(w, las = 2, col = rainbow(50))
w <- sort(rowSums(tdm), decreasing = TRUE) .
set.seed(1)
wordcloud(words = names(w), freq = w,max.words = 50,random.order = F, min.freq =  3, colors = brewer.pal(8, 'Dark2'), scale = c(5,0.3),rot.per = 0.6)
w <- data.frame(names(w),w) 
colnames(w) <- c('word','freq')
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)
#Sentiment Analysis  
Twitterdata <- read.csv(file.choose(), header = TRUE)
tweets <- as.character(Twitterdata$text)
class(tweets)
#Sentimental scores 
Senti <- get_nrc_sentiment(tweets)
head(Senti)
tweets[4]
get_nrc_sentiment('Irritating') 
get_nrc_sentiment('Optimist') 
barplot(colSums(s), las = 2.5, col = rainbow(10), ylab = 'Count',main= 'Sentiment scores for Twitter Tweets')