library(dplyr)
library(data.table)
library(qdap)
library(ngram)
library(tm)
library(RWeka)
library(stringr)
library(stringi)
library(NLP)
library(wordcloud)
library(ggplot2)

setwd("C:/Week7Assignment")

blogs <- readLines("en_US.blogs.txt",skipNul = TRUE, warn = TRUE)
news <- readLines("en_US.news.txt",skipNul = TRUE, warn = TRUE)
twitter <- readLines("en_US.twitter.txt",skipNul = TRUE, warn = TRUE)

#generate sample_set
set.seed(12345)
sample_size = 3000

sBlogs <- blogs[sample(1:length(blogs),sample_size)]
sNews <- news[sample(1:length(news),sample_size)]
sTwitter <- twitter[sample(1:length(twitter),sample_size)]

sample_set<-rbind(sBlogs,sNews,sTwitter)
rm(blogs,news,twitter)

#clean the sample_set, create corpus
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
testdata <- iconv(sample_set, "UTF-8", "ASCII", sub="")
corpus <- VCorpus(VectorSource(testdata))
corpus <- tm_map(corpus, toSpace, "/")
corpus <- tm_map(corpus, toSpace, "@")
corpus <- tm_map(corpus, toSpace, "\\|")
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)

#process NGram Tokenizer
ugt <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bgt <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tgt <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
qgt <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))

#process Term Document Matrix
uniTDM <- TermDocumentMatrix(corpus, control = list(tokenize = ugt))
biTDM <- TermDocumentMatrix(corpus, control = list(tokenize = bgt))
triTDM <- TermDocumentMatrix(corpus, control = list(tokenize = tgt))
quadTDM <- TermDocumentMatrix(corpus, control = list(tokenize = qgt))

#find frequency terms
uniTFF <- findFreqTerms(uniTDM, lowfreq = 4)
biTFF <- findFreqTerms(biTDM, lowfreq = 3)
triTFF <- findFreqTerms(triTDM, lowfreq = 3)
quadTFF <- findFreqTerms(quadTDM, lowfreq = 2)

uni_freq <- rowSums(as.matrix(uniTDM[uniTFF, ]))
uni_freq <- data.frame(unigram=names(uni_freq), frequency=uni_freq)
uni_freq <- uni_freq[order(-uni_freq$frequency),]
u_list <- setDT(uni_freq)
save(u_list,file="unigram.Rda")

bi_freq <- rowSums(as.matrix(biTDM[biTFF, ]))
bi_freq <- data.frame(bigram=names(bi_freq), frequency=bi_freq)
bi_freq <- bi_freq[order(-bi_freq$frequency),]
b_list <- setDT(bi_freq)
save(b_list,file="bigram.Rda")

tri_freq <- rowSums(as.matrix(triTDM[triTFF, ]))
tri_freq <- data.frame(trigram=names(tri_freq), frequency=tri_freq)
tri_freq <- tri_freq[order(-tri_freq$frequency),]
t_list <- setDT(tri_freq)
save(t_list,file="trigram.Rda")

quad_freq <- rowSums(as.matrix(quadTDM[quadTFF, ]))
quad_freq <- data.frame(quadgram=names(quad_freq), frequency=quad_freq)
quad_freq <- quad_freq[order(-quad_freq$frequency),]
q_list <- setDT(quad_freq)
save(q_list,file="quadgram.Rda")