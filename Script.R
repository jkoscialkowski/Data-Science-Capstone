## Coursera Data Science Capstone

## A workplace script for developing various elements of the model

## AND FOR NOTES

library(tm)
library(RWeka)

setwd("F:/Naukowe/RӯNE/Coursera/10 Data Science Capstone/Week 2")

blogsCorp <- clean("en_US.blogs.txt")
newsCorp <- clean("en_US.news.txt")
twitterCorp <- clean("en_US.twitter.txt")

## How large the sample has to be to ensure having a given percentage of words.

plotRequiredSampleSize <- function(x) {
    
    samples <- c(.10, .15, .20, .25, .30, .35, .40, .45, .50)
    
    for (i in samples) {
        
    }
    
}
    

## easy given matrices 
    
## in bookmarks: the guy repo gives good pipeline of creating the model from ngrams ++++ UNKING is there as well

## combining models with and without stopwords

## data split: 55-15-15-15
## compare n in ngrams by cv in the building set, decide on smoothing on first 15, decide on boosting on second 15,
## validation on the last 15