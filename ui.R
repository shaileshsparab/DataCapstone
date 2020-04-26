library(shiny)

shinyUI(fluidPage(
  titlePanel("Word Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("text",h6("type a word or phrase and press a SPACE bar"), value = ""),
      #submitButton("submit")
    ),
    
    mainPanel(
      h3('The next word is :'),
      verbatimTextOutput("prediction"),
      
      h4('Incase if there is no generic word derived then "A" is used as predicted word')
    )
  )
))