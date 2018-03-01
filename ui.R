#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        tags$head(
            tags$style(
                HTML("
                .myTextOutput {
                color: #4A1486;
                font-size: 30px;
                font-family: helvetica;
                }"
                )
            )
        ),
        
    navbarPage(
    
    
    "Text Prediction App",
    
    tabPanel("Application",
             
             sidebarLayout(
                 
                 sidebarPanel(
                     
                     textInput("textIn",
                               "Please input text for prediction (at least two words)",
                               width = '400px',
                               placeholder = "Some text..."),
                     actionButton("action", "Predict"),
                     
                     h4("The application needs a moment to load properly, thank you for your patience. ;)"),
                     hr(),
                     h4("An '<UNK>' is not an error, it means that an out-of-vocabulary word is the most probable.")
                 ),
                 
                 mainPanel(
                     
                     h1("Predicted word"),
                     
                     span(textOutput("textOut"), class = "myTextOutput"),
                     
                     h1("Wordcloud of most probable predictions"),
                     
                     plotOutput("wordcloud")
                     
                 )
                 
             )
    ),
    
    tabPanel("Documentation",
             h1("Documentation"),
             
             h3("Introduction and links to resources"),
             p("The objective was to create a functional and fully documented Shiny application predicting the next word of a given string. The model uses ngrams which have been refined using pruning and interpolation (as in ", a("here", href = "https://www.cs.cornell.edu/courses/cs4740/2012sp/lectures/smoothing+backoff-1-4pp.pdf"), "). Interpolation parameter was chosen to maximise accuracy on the test set (and turned out to be in favour of no interpolation at all). Later, when predicting strings unseen by the model, strings with fewer than 2 non-stopwords or consisting of stopwords in more than 80% was labeled as a stopword-intensive one and predicted using a trigram constructed with stopwords included. For the remaining strings, a trigram constructed with stopwords dropped was used, as it exhibited consistently better performance. R packages used:"),
             HTML("<ul>
                  <li><code>dplyr</code></li>
                  <li><code>ggplot2</code></li>
                  <li><code>quanteda</code></li>
                  <li><code>tm</code></li>
                  <li><code>wordcloud</code></li>
                  </ul>"),
             p("The main script used for producing the model, ", code("ui.R"), " and ", code("server.R"), " of the Shiny app and other relevant files can be found in a GitHub repository."), 
             a("https://github.com/jkoscialkowski/Data-Science-Capstone", href = "https://github.com/jkoscialkowski/Data-Science-Capstone"),
             br(),
             br(),
             p("There exists a pitch summarising the application concisely."),
             a("http://rpubs.com/Koscial/DSCapstonePitch", href = "http://rpubs.com/Koscial/DSCapstonePitch"),
             br(),
             br(),
             p("Also, there is a markdown document summarising the whole concept at an intermediate stage."),
             a("https://rpubs.com/Koscial/DSCMilestone", href = "https://rpubs.com/Koscial/DSCMilestone"),
             
             h3("Optimal sample size"),
             p("Firstly, sample size required to ensure demanded word coverage was investigated."),
             p("According to ", a("this", href = "https://en.oxforddictionaries.com/explore/how-many-words-are-there-in-the-english-language"), "source there are around a quarter of a million English words. The plots show many more, which is caused by many artifacts and foreign words present in the corpora. Thus, samples of 15% for blogs, 17,5% for Twitter and 20% for news were chosen as possibly representative enough. According to the plot this should ensure inclusion of most English words in use."),
             
             plotOutput("sampleSizePlot"),
             
             h3("Dataset preparation"),
             p("The data were cleaned using the ", code("clean()"), " function sourced from the ", code("clean.R"), " script in the repository. A large dataset obtained by taking samples as in the previous paragraph was then split in a 60-20-20 ratio into a training, testing, and validation sets. For each set there was also its copy with English stopwords removed."),
             
             h3("Creation of ngrams, unking, optimization"),
             p("Data sampling resulted in ngrams being created using a corpus containing almost 450,000 documents which entitled 9.5 million words.Words appearing more than 2 times were chosen as known, thus reducing their number from 407,815 to 83,440. Bigrams and trigrams were created afterwards, with versions for corpora with and without English stopwords. Any words which did not appear in the 'known' list was 'unked', meaning substituted with an arbitrary string, e.g. '<UNK>'. It is later processed and modelled as any other word and stands as a representative for any words lost during pruning, as well as any out-of-vocabulary words. Finally, probabilities were computed for all ngrams and then pruned with a 1e-06 threshold for bigrams and 1e-07 for trigrams."),
             
             h3("Choosing lambdas"),
             p("Lambdas for interpolation were chosen using prediction accuracy on the test set. It appeared that the best results were achieved using no interpolation, only pure trigrams. This is clear from the below plot."),
             
             plotOutput("lambdaAccuracyPlot"),
             
             h3("Validation and final model choice"),
             p("Accuracy attained on the validation set was 31.4% which is a pretty good result."),
             
             h3("Possible further improvements"),
             HTML("<ul>
                  <li>Test set accuracies seem to favour trigrams over bigrams - introduce larger ngrams?</li>
                  <li>There are many other, more sophisticated methods, used in next word prediction.</li>
                  <li>Making the model work faster - more intelligent pruning of ngrams, more efficient storage.</li>
                  </ul>"),
             
             h2("Have fun with the application!")
    )
)))
