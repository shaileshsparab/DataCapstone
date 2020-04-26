library(shiny)
library(data.table)
library(stringr)
library(qdap)
library(ngram)
library(stringi)
library(NLP)
library(tm)
library(RWeka)
library(dplyr)

#set source file for NLP and next word prediction
source("wordprediction.R")

shinyServer(
  function(input, output, session) {
    output$inputValue <- renderPrint({input$text})
    output$prediction <- renderPrint({wordprediction(input$text)})
  }
)