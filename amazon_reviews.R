setwd("C:/Users/india/Desktop/data science/assignments/text mining")
install.packages("rvest")
install.packages("XML")
install.packages("magrittr")
library(rvest)
library(XML)
library(magrittr)
#Amazon Reviews 
aurl <- "https://www.amazon.in/gp/customer-reviews/R2C1OVZ9IOA62H/ref=cm_cr_dp_d_rvw_ttl?ie=UTF8&ASIN=B086977J3K"
amazon_reviews <- NULL
for (i in 1:20){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
length(amazon_reviews)
write.table(amazon_reviews,"nord.txt",row.names = F)
Oneplus_Lap <- read.delim('nord.TXT')
str(Oneplus_Lap)
View(Oneplus_Lap)
#corpus
library(tm)
corpus <- Oneplus_Lap[-1,]
head(corpus)
class(corpus)
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:5])
# Clean the text 
corpus <- tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
corpus <- tm_map(corpus,removeNumbers)
inspect(corpus[1:5])
corpus_clean<-tm_map(corpus,stripWhitespace)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords, stopwords('english'))
inspect(cleanset[1:5])
removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])
cleanset<-tm_map(cleanset,removeWords, c('also','better','phone','fps','good','well','makes','can','youre','phones','much','bit','n','go','know','following','consider','best','far','least','can','ok','okay','like','now','keep','perhaps','take','even','isnt','your','shouldnt','used','that','saying','said','will','t','n','ill','the', 'due', 'are', 'not', 'for', 'this', 'and',  'that', 'there', 'new', 'near', 'beyond','will','also','can', 'time','from', 'been', 'both','than',  'has','now', 'until', 'all', 'use', 'two', 'east', 'between', 'end', 'have', 'avenue','before','just', 'mac', 'being','when','levels','remaining','based', 'still', 'of','over', 'only', 'past', 'one', 'while','then','quite','using'))
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])         
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10,1:20]
# Bar Plot 
w <- rowSums(tdm) 
w <- subset(w, w>= 30) 
barplot(w, las = 2, col = rainbow(50))
# Word Cloud 
library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(123)
windows()
wordcloud(words = names(w), freq = w,max.words = 250,random.order = F,min.freq =  3, colors = brewer.pal(8, 'Dark2'),scale = c(5,0.3), rot.per = 0.6)
install.packages("wordcloud2")
library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
windows()
wordcloud2(w,size = 0.5, shape = 'triangle', rotateRatio = 0.5, minSize = 1)
# Sentiment Analysis 
install.packages("syuzhet")
install.packages("lubridate")
install.packages("ggplot2")
installed.packages("reshape")
install.packages("dyplr")
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
Amazon_reviews <- read.delim('nord.TXT')
reviews <- as.character(Amazon_reviews[-1,])
class(reviews)
# Obtaining Sentimental scores 
s <- get_nrc_sentiment(reviews)
head(s)
reviews[4]
get_nrc_sentiment('Beautiful') 
get_nrc_sentiment('callous') 
barplot(colSums(s), las = 2.5, col = rainbow(10), ylab = 'Count',main= 'Sentiment scores for Amazon Reviews -Oneplus Phone')
