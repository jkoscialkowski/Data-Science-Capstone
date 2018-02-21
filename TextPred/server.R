#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
source("clean.R")

shinyServer(function(input, output) {
    
    
    output$textOut <- reactive({clean(textOrPath = input$textIn)[[1]][[1]]})
    
})
