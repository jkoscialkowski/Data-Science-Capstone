#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(dplyr)
library(ggplot2)
library(shiny)
library(tm)
library(wordcloud)
source("clean.R")
wordCountMat <- read.table(file = "wordCountMat.txt", header = TRUE)
knownWords <- read.table(file = "knownWords.txt")
knownWords <- knownWords$V1
testAccuracies <- read.table(file = "testAccuracies.txt", header = TRUE)
load("trigrams.RData")

pal = brewer.pal(8,"Purples")
pal = pal[-c(1:3)]

shinyServer(function(input, output) {
    
    pred <- eventReactive(input$action, {
        
        text <- clean(textOrPath = input$textIn, includeStopwords = TRUE)[[1]][[1]]
        text <- strsplit(text, split = " ")[[1]]
        text[!(text %in% knownWords)] <- "<UNK>"
        
        if(sum(!(text %in% stopwords("english"))) >= 2 & sum(text %in% stopwords("english"))/length(text) < .8) {
            text <- tail(text[!(text %in% stopwords("english"))], 2)
            
            if(sum(trigramNoStop[,1] == text[1] & trigramNoStop[,2] == text[2]) == 0) {
                pred <- "Such combination did not appear in the training set, try something else!"
                missing <- TRUE
            } else {
                pred <- trigramNoStop[trigramNoStop[,1] == text[1] & trigramNoStop[,2] == text[2], c(3,4)]
                colnames(pred) <- c("word", "prob")
                pred <- pred %>% arrange(desc(prob))
                pred$word[is.na(pred$word)] <- "<UNK>"
                pred <- pred[1:min(100, nrow(pred)), ]
                missing <- FALSE
            }
            
        } else {
            text <- tail(text, 2)
            
            if(sum(trigramStop[,1] == text[1] & trigramStop[,2] == text[2]) == 0) {
                pred <- "Such combination did not appear in the training set, try something else!"
                missing <- TRUE
            } else {
                pred <- trigramStop[trigramStop[,1] == text[1] & trigramStop[,2] == text[2], c(3,4)]
                colnames(pred) <- c("word", "prob")
                pred <- pred %>% arrange(desc(prob))
                pred$word[is.na(pred$word)] <- "<UNK>"
                pred <- pred[1:min(100, nrow(pred)), ]
                missing <- FALSE
            }
            
        }
        pred = list(pred = pred, missing = missing)

    })
    
    output$textOut <- reactive({
        if(pred()$missing) {
            pred()$pred
        } else {
            pred()$pred[1,1]    
        }
    })
    
    output$wordcloud <- renderPlot({
        if(pred()$missing) {
            
        } else {
            wordcloud(pred()$pred$word, pred()$pred$prob, max.words = 100, random.order = FALSE, colors = pal)
        }
    }, height = 400, width = 400)
    
    output$sampleSizePlot <- renderPlot({
        g <- ggplot(data = wordCountMat, mapping = aes(x = sampleSize, y = numberOfWords, col = source)) + 
            geom_line()
        g
    }, width = 800)
    
    output$lambdaAccuracyPlot <- renderPlot({
        g <- ggplot(data = testAccuracies, mapping = aes(x = lambdas, y = accuracy, col = type)) +
            geom_line()
        g
        }, width = 800)
        
})
