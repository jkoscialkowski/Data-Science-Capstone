## Coursera Data Science Capstone

## Scipt used for training the model and preparing any auxiliary data which needs to be loaded with the Shiny app.
## Execute chunk by chunk

############################
# READING DATA AND LIBRARIES
############################
    
    library(data.table)
    library(quanteda)
    library(tm)
    library(tidyverse)
    
    setwd("D:/10 Data Science Capstone/Data-Science-Capstone/TextPred")
    source("clean.R")
    
    setwd("D:/10 Data Science Capstone/final/en_US")
    blogsCorpStopwords <- clean("en_US.blogs.txt", isText = FALSE, includeStopwords = TRUE)
    newsCorpStopwords <- clean("en_US.news.txt", isText = FALSE, includeStopwords = TRUE)
    twitterCorpStopwords <- clean("en_US.twitter.txt", isText = FALSE, includeStopwords = TRUE)
    setwd("D:/10 Data Science Capstone")

#######################
# NECESSARY SAMPLE SIZE
#######################
    
    set.seed(2137)
    samples <- c(.10, .15, .20, .25, .30, .35, .40, .45, .50)
    wordCountMat <- matrix(rep(0, times = length(samples)*3), ncol = 3)
    
    for (i in 1:length(samples)) {
        wordCountMat[i, 1] <- length(Terms(TermDocumentMatrix(sample(blogsCorpStopwords, 
                                                                     size = length(blogsCorpStopwords)*
                                                                         samples[i]))))
        wordCountMat[i, 2] <- length(Terms(TermDocumentMatrix(sample(newsCorpStopwords, 
                                                                     size = length(newsCorpStopwords)*
                                                                         samples[i]))))
        wordCountMat[i, 3] <- length(Terms(TermDocumentMatrix(sample(twitterCorpStopwords, 
                                                                     size = length(twitterCorpStopwords)*
                                                                         samples[i]))))
    }
    
    wordCountMat <- as.data.frame(cbind(samples, wordCountMat))
    colnames(wordCountMat) <- c("sampleSize", "Blogs", "News", "Twitter")
    wordCountMat <- wordCountMat %>% gather(source, numberOfWords, Blogs:Twitter)
    write.table(wordCountMat, file = "wordCountMat.txt", sep = "\t", row.names = FALSE)
    
    g <- ggplot(data = wordCountMat, mapping = aes(x = sampleSize, y = percentWords, col = source)) + geom_line()
    g
    
    ## There are many artifacts, own names and foreign words in the corpora
    ## According to:
    ## https://en.oxforddictionaries.com/explore/how-many-words-are-there-in-the-english-language
    ## There are around a quarter of a million English words.
    ## Thus, according to the plot, samples of 0.15 for blogs, 0.175 for Twitter and 0.2 for news should suffice.
    

##################
# PREPARE DATASETS
##################

    ## Data split: 60-20-20
    ## Train ngrams on the 60% part, decide on smoothing on first 20%, validation on the latter 20.
    
    corpusStopwords <- c(
        sample(blogsCorpStopwords, size = length(blogsCorpStopwords)*0.15),
        sample(newsCorpStopwords, size = length(newsCorpStopwords)*0.2),
        sample(twitterCorpStopwords, size = length(twitterCorpStopwords)*0.175))
    
    corpusStopwords <- SimpleCorpus(VectorSource(unlist(corpusStopwords)))
    
    l <- length(corpusStopwords)
    indicesTrain <- sample(1:l, size = l*0.60)
    indicesTestSmoothing <- sample(1:l[-indicesTrain], size = l*0.20)
    indicesValidation <- sample(1:l[-c(indicesTrain, indicesTestSmoothing)], size = l*0.20)
    
    trainStopwords <- corpusStopwords[indicesTrain]
    trainNoStopwords <- tm_map(trainStopwords, removeWords, stopwords("english"))
    trainNoStopwords <- tm_map(trainNoStopwords, stripWhitespace)
    testSmoothing <- corpusStopwords[indicesTestSmoothing]
    validation <- corpusStopwords[indicesValidation]
    
    save(list = c("trainStopwords", "trainNoStopwords", "testSmoothing", "validation"), file = "datasets.RData")
    
    rm(list = c("l", "indicesTrain", "indicesTestSmoothing", "indicesValidation", 
                "blogsCorpStopwords", "newsCorpStopwords", "twitterCorpStopwords"))
    gc()
        

###########################
# CREATE NGRAMS AND <UNK>'s
###########################

    # <UNK> words appearing fewer than 3 times
    # Prune ngrams by probability
    
    # WITH STOPWORDS
    
    unigramStop <- tokens(unlist(trainStopwords), ngrams = 1)
    unigramStop <- data.frame(table(unlist(unigramStop)))
    unigramStop <- unigramStop %>% arrange(desc(Freq))
    unknownWords <- unigramStop[unigramStop[,2] <= 2,1]
    knownWords <- unigramStop[unigramStop[,2] > 2,1]
    unigramStop <- unigramStop[unigramStop[,2] > 2, ]
    unigramStop[,1] <- as.character(unigramStop[,1])
    unigramStop[,2] <- unigramStop[,2]/sum(unigramStop[,2])
    
    bigramStop <- tokens(unlist(trainStopwords), ngrams = 2)
    bigramStop <- data.frame(table(unlist(bigramStop)))
    a <- sapply(as.character(bigramStop[,1]), strsplit, split = "_")
    a <- matrix(unlist(a), ncol = 2, byrow = TRUE)
    bigramStop <- cbind(as.data.frame(a, stringsAsFactors = FALSE), bigramStop[,2])
    colnames(bigramStop) <- c("w1", "w2", "Freq")
    bigramStop$w1[bigramStop$w1 %in% unknownWords] <- "<UNK>"
    bigramStop$w2[bigramStop$w2 %in% unknownWords] <- "<UNK>"
    bigramStop <- as.data.table(bigramStop)
    setkeyv(bigramStop, c("w1", "w2"))
    bigramStop <- bigramStop[, sum(Freq), by = c("w1", "w2")]
    setnames(bigramStop, "V1", "Freq")
    bigramStop <- bigramStop %>% arrange(desc(Freq))
    bigramStop <- bigramStop[!(bigramStop[,1] == "<UNK>" & bigramStop[,2] == "<UNK>"), ]
    bigramStop$Freq <- bigramStop$Freq/sum(bigramStop$Freq)
    bigramStop <- bigramStop[bigramStop$Freq > 1e-06,]
    colnames(bigramStop)[3] <- "Prob"
    
    trigramStop <- tokens(unlist(trainStopwords), ngrams = 3)
    trigramStop <- data.frame(table(unlist(trigramStop)))
    a <- sapply(as.character(trigramStop[,1]), strsplit, split = "_")
    a <- matrix(unlist(a), ncol = 3, byrow = TRUE)
    trigramStop <- cbind(as.data.frame(a, stringsAsFactors = FALSE), trigramStop[,2])
    colnames(trigramStop) <- c("w1", "w2", "w3", "Freq")
    trigramStop$w1[trigramStop$w1 %in% unknownWords] <- "<UNK>"
    trigramStop$w2[trigramStop$w2 %in% unknownWords] <- "<UNK>"
    trigramStop$w3[trigramStop$w3 %in% unknownWords] <- "<UNK>"
    trigramStop <- as.data.table(trigramStop)
    setkeyv(trigramStop, c("w1", "w2", "w3"))
    trigramStop <- trigramStop[, sum(Freq), by = c("w1", "w2", "w3")]
    setnames(trigramStop, "V1", "Freq")
    trigramStop <- trigramStop %>% arrange(desc(Freq))
    trigramStop <- trigramStop[rowSums(trigramStop == "<UNK>") <= 1, ]
    trigramStop$Freq <- trigramStop$Freq/sum(trigramStop$Freq)
    trigramStop <- trigramStop[trigramStop$Freq > 3e-07,]
    colnames(trigramStop)[4] <- "Prob"
    
    rm(trainStopwords)
    gc()
    
    # NO STOPWORDS

        # unigramNoStop <- tokens(trainNoStopwords[[1]], ngrams = 1)
        # unigramNoStop <- data.frame(table(unlist(unigramNoStop)))
        # unigramNoStop <- unigramNoStop %>% arrange(desc(Freq))
        # unigramNoStop <- unigramNoStop[unigramNoStop[,2] > 3, ]
        # unigramNoStop[,1] <- as.character(unigramNoStop[,1])
        # unigramNoStop[,2] <- unigramNoStop[,2]/sum(unigramNoStop[,2])
    
    bigramNoStop <- tokens(unlist(trainNoStopwords), ngrams = 2)
    bigramNoStop <- data.frame(table(unlist(bigramNoStop)))
    a <- sapply(as.character(bigramNoStop[,1]), strsplit, split = "_")
    a <- matrix(unlist(a), ncol = 2, byrow = TRUE)
    bigramNoStop <- cbind(as.data.frame(a, stringsAsFactors = FALSE), bigramNoStop[,2])
    colnames(bigramNoStop) <- c("w1", "w2", "Freq")
    bigramNoStop$w1[bigramNoStop$w1 %in% unknownWords] <- "<UNK>"
    bigramNoStop$w2[bigramNoStop$w2 %in% unknownWords] <- "<UNK>"
    bigramNoStop <- as.data.table(bigramNoStop)
    setkeyv(bigramNoStop, c("w1", "w2"))
    bigramNoStop <- bigramNoStop[, sum(Freq), by = c("w1", "w2")]
    setnames(bigramNoStop, "V1", "Freq")
    bigramNoStop <- bigramNoStop %>% arrange(desc(Freq))
    bigramNoStop <- bigramNoStop[!(bigramNoStop[,1] == "<UNK>" & bigramNoStop[,2] == "<UNK>"), ]
    bigramNoStop$Freq <- bigramNoStop$Freq/sum(bigramNoStop$Freq)
    bigramNoStop <- bigramNoStop[bigramNoStop$Freq > 1e-06,]
    colnames(bigramNoStop)[3] <- "Prob"
    
    trigramNoStop <- tokens(unlist(trainNoStopwords), ngrams = 3)
    trigramNoStop <- data.frame(table(unlist(trigramNoStop)))
    a <- sapply(as.character(trigramNoStop[,1]), strsplit, split = "_")
    a <- matrix(unlist(a), ncol = 3, byrow = TRUE)
    trigramNoStop <- cbind(as.data.frame(a, stringsAsFactors = FALSE), trigramNoStop[,2])
    colnames(trigramNoStop) <- c("w1", "w2", "w3", "Freq")
    trigramNoStop$w1[trigramNoStop$w1 %in% unknownWords] <- "<UNK>"
    trigramNoStop$w2[trigramNoStop$w2 %in% unknownWords] <- "<UNK>"
    trigramNoStop$w3[trigramNoStop$w3 %in% unknownWords] <- "<UNK>"
    trigramNoStop <- as.data.table(trigramNoStop)
    setkeyv(trigramNoStop, c("w1", "w2", "w3"))
    trigramNoStop <- trigramNoStop[, sum(Freq), by = c("w1", "w2", "w3")]
    setnames(trigramNoStop, "V1", "Freq")
    trigramNoStop <- trigramNoStop %>% arrange(desc(Freq))
    trigramNoStop <- trigramNoStop[rowSums(trigramNoStop == "<UNK>") <= 1, ]
    trigramNoStop$Freq <- trigramNoStop$Freq/sum(trigramNoStop$Freq)
    trigramNoStop <- trigramNoStop[trigramNoStop$Freq > 3e-07,]
    colnames(trigramNoStop)[4] <- "Prob"
    
    rm(trainNoStopwords)
    gc()
    
    # Saving
    write.table(knownWords, file = "knownWords.txt", sep = "\t", row.names = FALSE)
    save(list = c("unigramStop", "bigramStop", "trigramStop", "bigramNoStop", "trigramNoStop"), file = "ngrams.RData")
    
    rm(a)
    gc()

    
################
# CHOOSE LAMBDAS
################

    # Preparation of test sets
    testTrigramStop <- tokens(unlist(testSmoothing), ngrams = 3)
    testTrigramStop <- sapply(as.character(unlist(testTrigramStop)), strsplit, split = "_")
    testTrigramStop <- as.data.frame(matrix(unlist(testTrigramStop), ncol = 3, byrow = TRUE), 
                                     stringsAsFactors = FALSE)
    testTrigramStop$V1[testTrigramStop$V1 %in% unknownWords] <- "<UNK>"
    testTrigramStop$V2[testTrigramStop$V2 %in% unknownWords] <- "<UNK>"
    
    testTrigramNoStop <- tm_map(testSmoothing, removeWords, stopwords("english"))
    testTrigramNoStop <- tm_map(testTrigramNoStop, stripWhitespace)
    testTrigramNoStop <- tokens(unlist(testTrigramNoStop), ngrams = 3)
    testTrigramNoStop <- sapply(as.character(unlist(testTrigramNoStop)), strsplit, split = "_")
    testTrigramNoStop <- as.data.frame(matrix(unlist(testTrigramNoStop), ncol = 3, byrow = TRUE), 
                                       stringsAsFactors = FALSE)
    testTrigramNoStop$V1[testTrigramNoStop$V1 %in% unknownWords] <- "<UNK>"
    testTrigramNoStop$V2[testTrigramNoStop$V2 %in% unknownWords] <- "<UNK>"
    
    save(list = c("testTrigramStop", "testTrigramNoStop"), file = "testDatasets.RData")
    
    rm(testSmoothing)
    gc()
    
    # Prediction function
    predictSmoothTrigram <- function(bigramObj = NULL, trigramObj, newdata, lambda3 = 1) {
        # Requires data.table package
        predictions <- rep(0, times = nrow(newdata))
        
        for (i in 1:nrow(newdata)) {
            if(i/1000 == floor(i/1000)) print(i)
            
            trigramPred <- trigramObj[trigramObj[,1] == newdata[i, 1] & trigramObj[,2] == newdata[i,2], c(3,4)]
            trigramPred[,2] <- trigramPred[,2]*lambda3
            if(!is.null(bigramObj)) {
                colnames(trigramPred)[1] <- "wordPred"
                bigramPred <- bigramObj[bigramObj[,1] == newdata[i, 1], c(2,3)]
                bigramPred[,2] <- bigramPred[,2]*(1 - lambda3)
                colnames(bigramPred)[1] <- "wordPred"
                smoothPred <- rbind(trigramPred, bigramPred)
                colnames(smoothPred) <- c("w", "Prob")
                smoothPred <- as.data.table(smoothPred)
                setkeyv(smoothPred, "w")
                smoothPred <- smoothPred[, sum(Prob), by = "w"]
            } else {
                smoothPred <- trigramPred
                colnames(smoothPred) <- c("w", "V1")
            }
            smoothPred <- smoothPred %>% arrange(desc(V1))
            predictions[i] <- smoothPred[1,1]
            if (is.na(predictions[i])) predictions[i] <- "<UNK>"
        }
        
        list(pred = predictions, ref = newdata[, 3])
    }
    
    # Lambda optimization for stopwords (only 10000 words were predicted - time)
    lambda3s <- seq(from = .9, to = 1, by = .01)
    testAccuracies <- matrix(rep(0, times = length(lambda3s)*2), ncol = 2)
    predictionList <- vector("list", 11)
    
    for (i in 1:length(lambda3s)) {
        
        predictionList[[i]] <- 
            predictSmoothTrigram(bigramObj = bigramStop, trigramObj = trigramStop,
                                 newdata = testTrigramStop[sample(1:nrow(testTrigramStop), 10000), ],
                                 lambda3 = lambda3s[i])
        
        print(i)
    }
    save(list = "predictionList", file = "predStop.RData")
    
    for (i in 1:length(lambda3s)) {
        testAccuracies[i,1] <- sum(predictionList[[i]]$pred == predictionList[[i]]$ref)/10000
    }
    
    # Lambda optimization for no stopwords
    predictionList <- vector("list", 11)
    
    for (i in 1:length(lambda3s)) {
        
        predictionList[[i]] <- 
            predictSmoothTrigram(bigramObj = bigramNoStop, trigramObj = trigramNoStop,
                                 newdata = testTrigramNoStop[sample(1:nrow(testTrigramNoStop), 10000), ],
                                 lambda3 = lambda3s[i])
        
        print(i)
    }
    save(list = "predictionList", file = "predNoStop.RData")
    
    for (i in 1:length(lambda3s)) {
        testAccuracies[i,2] <- sum(predictionList[[i]]$pred == predictionList[[i]]$ref)/10000
    }
    
    testAccuracies <- cbind(lambda3s, testAccuracies)
    colnames(testAccuracies) <- c("lambdas", "stopwords", "noStopwords")
    testAccuracies <- testAccuracies %>% gather(type, accuracy, stopwords:noStopwords)
    write.table(testAccuracies, file = "testAccuracies.txt", sep = "\t", row.names = FALSE)
    
    
############
# VALIDATION
############
    
    # ODPALIÆ FUNKCJÊ, ZMIENIONA!!!!!!!!!!!!!!!!
    
    # Preparation of the validation set
    validation5gram <- tokens(unlist(validation), ngrams = 5)
    validation5gram <- sapply(as.character(unlist(validation5gram)), strsplit, split = "_")
    validation5gram <- as.data.frame(matrix(unlist(validation5gram), ncol = 5, byrow = TRUE),
                                        stringsAsFactors = FALSE)
    validation5gram$V1[!(validation5gram$V1 %in% knownWords)] <- "<UNK>"
    validation5gram$V2[!(validation5gram$V2 %in% knownWords)] <- "<UNK>"
    validation5gram$V3[!(validation5gram$V3 %in% knownWords)] <- "<UNK>"
    validation5gram$V4[!(validation5gram$V4 %in% knownWords)] <- "<UNK>"
    validation5gram$V5[!(validation5gram$V5 %in% knownWords)] <- "<UNK>"
        
    indicesStop <- rowSums(matrix(as.matrix(validation5gram) %in% stopwords("english"), ncol = 5)) >= 3
    validationSampleStop <- validation5gram[sample(1:length(indicesStop), 
                                                   size = 10000*sum(indicesStop)/length(indicesStop)), 3:5] 
    
    
    validationSampleNoStop <- validation5gram[!indicesStop, ]
    validationSampleNoStop <- t(apply(validationSampleNoStop[sample(1:nrow(validationSampleNoStop), 100000), ], 1, 
                                      function(x) tail(x[!(x %in% stopwords("english"))], 3)))

    
    validationSampleNoStop <- as.data.frame(validationSampleNoStop, stringsAsFactors = FALSE)
    validationSampleNoStop <- validationSampleNoStop[sample(1:nrow(validationSampleNoStop), 
                                                            size = 10000*sum(!indicesStop)/length(indicesStop)), ]

    save(list = c("validationSampleStop", "validationSampleNoStop"), file = "validationDatasets.RData")
    
    # Prediction for validation
    validationPredStop <- predictSmoothTrigram(trigramObj = trigramStop, newdata = validationSampleStop)
    validationPredNoStop <- predictSmoothTrigram(trigramObj = trigramNoStop, newdata = validationSampleNoStop)
    
    sum(validationPredStop$pred == validationPredStop$ref & validationPredStop$pred != "<UNK>")/10000 +
        sum(validationPredNoStop$pred == validationPredNoStop$ref & validationPredNoStop$pred != "<UNK>")/10000
    
    save(list = c("validationPredStop", "validationPredNoStop"), file = "validationPred.RData")
    
############################################
# WHAT NEEDS TO BE LOADED INTO THE SHINY APP
############################################

# wordCountMat.txt
# knownWords.txt
# ngrams.RData
# testAccuracies.txt
    
###############
# READING CHUNK
###############
    
    load("D:/10 Data Science Capstone/final/en_US/en_US.blogs_refinedStopwords.RData")
    blogsCorpStopwords <- textCorpus
    load("D:/10 Data Science Capstone/final/en_US/en_US.news_refinedStopwords.RData")
    newsCorpStopwords <- textCorpus
    load("D:/10 Data Science Capstone/final/en_US/en_US.twitter_refinedStopwords.RData")
    twitterCorpStopwords <- textCorpus