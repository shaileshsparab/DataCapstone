library(dplyr)
library(wordcloud)
library(ggplot2)
library(ngram)
library(stringi)
library(NLP)
library(data.table)
library(qdap)
library(RWeka)
library(tm)
library(stringr)

#setwd("C:/Week7Assignment")

load("unigram.Rda")
load("bigram.Rda")
load("trigram.Rda")
load("quadgram.Rda")

#Main function
wordprediction <- function(sentence){
  value = ""
  sen1 = unlist(strsplit(sentence,' '))
  if(length(sen1)>=3){value = quadgram(sen1[(length(sen1)-2):length(sen1)])
  }
  if(is.null(value)||length(sen1)==2){
    value = trigram(sen1[(length(sen1)-1):length(sen1)])
  }
  if(is.null(value)||length(sen1)==1){value = bigram(sen1[length(sen1)])
  }
  if(is.null(value)){value = "a"} #default value
  
  return(value)
}

#quadgram function
quadgram <- function(fourg){
  four <- paste(fourg,collapse = ' ')
  quadsum <- data.frame(quadgram="test",frequency=0)
  le1 <- t_list[trigram==four]
  m <- as.numeric(le1$frequency)
  if(length(m)==0) return(NULL)
  
  for(string0 in u_list$unigram){
    text = paste(four,string0)
    found <- q_list[quadgram==text]
    n<- as.numeric(found$frequency)
    
    if(length(n)!=0){
      quadsum <- rbind(quadsum,found)
      
    }
  }
  if(nrow(quadsum)==1) return(NULL)
  quadsum <- quadsum[order(-frequency)]
  sen1 <- unlist(strsplit(as.String(quadsum[1,quadgram]),' '))
  return (sen1[length(sen1)])
}

#trigram function
trigram <- function(threeg){
  three <- paste(threeg,collapse = ' ')
  trisum <- data.frame(trigram="test",frequency=0)
  le1 <- b_list[bigram==three]
  m <- as.numeric(le1$frequency)
  if(length(m)==0) return(NULL)
  
  for(string0 in u_list$unigram){
    text = paste(three,string0)
    found <- t_list[trigram==text]
    n<- as.numeric(found$frequency)
    
    if(length(n)!=0){
      trisum <- rbind(trisum,found)
    }
  }
  if(nrow(trisum)==1) return(NULL)
  trisum <- trisum[order(-frequency)]
  sen1 <- unlist(strsplit(as.String(trisum[1,trigram]),' '))
  return (sen1[length(sen1)])
}

#bigram function
bigram <- function(twog){
  two <- paste(twog,collapse = ' ')
  bisum <- data.frame(bigram="test",frequency=0)
  le1 <- u_list[unigram==two]
  m <- as.numeric(le1$frequency)
  if(length(m)==0) return(NULL)
  
  for(string0 in u_list$unigram){
    text = paste(two,string0)
    found <- b_list[bigram==text]
    n<- as.numeric(found$frequency)
    
    if(length(n)!=0){
      bisum <- rbind(bisum,found)
    }
  }
  
  if(nrow(bisum)==1) return(NULL)
  bisum <- bisum[order(-frequency)]
  
  sen1 <- unlist(strsplit(as.String(bisum[1,bigram]),' '))
  return (sen1[length(sen1)])
}
