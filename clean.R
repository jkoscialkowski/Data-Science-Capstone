## R function for cleaning text files

## Requires the tm package to be loaded

clean <- function(path) {
    
    # Reading
    textCon <- file(path, encoding = "UTF-8", open = "rb")
    textCorpus <- readLines(textCon, skipNul = TRUE)
    textCorpus <- Corpus(VectorSource(textCorpus))
    
    # Conversion of UTF-8 artifacts to ASCII
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) iconv(x, to="ASCII", sub = "'")))
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "(''|''')", replacement = "'", x)))

    # Removing stopwords, numbers and enforcing lowercase
    textCorpus <- tm_map(textCorpus, content_transformer(tolower))
    textCorpus <- tm_map(textCorpus, removeWords, stopwords("english"))
    textCorpus <- tm_map(textCorpus, content_transformer(removeNumbers))
        
    # A substitution for apostrophes to preserve contractions
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "'t ", replacement = "TTTT ", x)))
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "'ll ", replacement = "LLLL ", x)))
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "'ve ", replacement = "VVVV ", x)))
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "'d ", replacement = "DDDD ", x)))
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "'m ", replacement = "MMMM ", x)))
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "'s ", replacement = "SSSS ", x)))
    
    # Removing other punctutation, profanity and URLs
    textCorpus <- tm_map(textCorpus, content_transformer(removePunctuation))

    profanityList <- readLines("profanity_list.txt")
    textCorpus <- tm_map(textCorpus, removeWords, profanityList)
    
    removeURLs <- function(x) gsub(pattern = "http[[:alnum:]]*", replacement = " ", x = x)
    textCorpus <- tm_map(textCorpus, content_transformer(removeURLs))
    
    # Restoring apostrophes
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "TTTT", replacement = "'t ", x)))
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "LLLL", replacement = "'ll ", x)))
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "VVVV", replacement = "'ve ", x)))
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "DDDD", replacement = "'d ", x)))
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "MMMM", replacement = "'m ", x)))
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "SSSS", replacement = "'s ", x)))
    
    # Removing 1-chararacter words left after the above manipulations
    textCorpus <- tm_map(textCorpus, content_transformer(function(x) gsub(pattern = "\\s[a-z]\\s([a-z]\\s)*", 
                                                                          replacement = "", x)))
    
    # Removing whitespaces, important to put it at the end
    textCorpus <- tm_map(textCorpus, stripWhitespace)
    
    # Saving to file
    save(textCorpus, file = gsub(pattern = "\\.txt", replacement = "_refined\\.RData", x = path))
    
    # Returning refined text
    textCorpus
    
}