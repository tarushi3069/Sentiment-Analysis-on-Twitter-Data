########################################################Libraries required ####################################################
library(shiny)
library(httr)
library(devtools)
library(base64enc)
library(twitteR)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(tm)
library(wordcloud)
##########################################################Connection to Twitter API############################################
consumer_key ="0JgpQiHzLGoValA6EOh1Xp6Vp"
consumer_secret ="mBdajaSFvetDYLJgmH8O38vWjWFstQU95PjNVVyvTgnVLLDjx1"
access_token ="1168606699420356608-a6gwYeOhTVx8wvORCvHP7mpYRHLPuk"
access_secret ="5tfsPwcVACvxj8fyponTfpEDBe3oCUdinScfnpSpTRHsT"
setup_twitter_oauth(consumer_key, consumer_secret,access_token,access_secret)
##########################################################Front End Code########################################################
ui <- fluidPage(theme = "lol.css",
  h1("Sentiment Analysis Application"),
  wellPanel(
  fluidRow(
  column(3,textInput(inputId="topic",label="Topic")),
  column(3,numericInput(inputId="ntweet",label="Count",300))),
  radioButtons(inputId="citwit",label="Wanna see the tweets?",c("Yes"="one",
                                                                "No"="two")),
  selectInput(inputId="choice",label="What do you want today?",c(
                                                                 "Wordcloud"="two",
                                                                 "Sentiment Analysis"="three")),
 ),

  tabsetPanel(              
    tabPanel(title = "Figure",
             plotOutput("mainstuff")
    ),
    tabPanel(title = "Tweets",
             textOutput("tweets")
    )
  )
)
########################################################Backend Code###########################################################
server <- function(input,output){
  output$mainstuff <- renderPlot({
    x<-reactive({input$choice})
    y<-reactive({input$comalgo})
    a<-reactive({input$choice1})
    tweets <- searchTwitter(input$topic,n=input$ntweet,lang='en')
    tweetsdf <- twListToDF(tweets)
    write.csv(tweetsdf,file="appexperiments.csv",row.names=F)
    data <- read.csv("appexperiments.csv", header = T)
    str(data)
    # Build corpus
    corpus <- iconv(data$text, to='UTF-8', sub = "byte")
    corpus <- Corpus(VectorSource(corpus))
    
    #####################################################Clean text##################################################
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    cleanset <- tm_map(corpus, removeWords, stopwords('english'))
    #removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
    #cleanset <- tm_map(cleanset, content_transformer(removeURL))
    cleanset <- tm_map(cleanset, stripWhitespace)
    
    ###############################################Term document matrix###############################################
    tdm <- TermDocumentMatrix(cleanset)
    tdm <- as.matrix(tdm)
    tdm <- tdm[rowSums(tdm)>10,]
    
   
    
    #######################################################wordcloud###################################################
    if(x()=="two"){
    w <- sort(rowSums(tdm), decreasing = TRUE)
    #set.seed for repeatability
    set.seed(222)
    wordcloud(words = names(w),
              freq = w,
              max.words = 150,
              random.order = F,
              min.freq = 5,
              colors = brewer.pal(8, 'Dark2'),
              scale = c(5, 0.3),
              rot.per = 0.3)}
    #####################################################sentiment analysis#############################################
    if(x()=="three"){
      data <- read.csv("appexperiments.csv", header = T)
      tweets <- iconv(data$text, to = 'UTF-8')
      # Obtain sentiment scores
      s <- get_nrc_sentiment(tweets)
      # Bar plot
      barplot(colSums(s),
              las = 2,
              col = rainbow(10),
              ylab = 'Count',
              main = 'Sentiment Scores for data Tweets')}
    })
  output$tweets <- renderPrint({
    z<-reactive({input$citwit})
    if(z()=="one"){
      tweets <- searchTwitter(input$topic,n=input$ntweet,lang='en')
      tweets
    }
  })
}

shinyApp(ui =ui,server= server)
