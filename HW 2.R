library(shiny)
library(tm)
library(wordcloud)
library(tidyverse)
library(rvest)
URL<- "https://en.wikipedia.org/wiki/data_visualization"

# The goal of this part is to write a code to read the texts in the data visualization page.
#  Store the url of data visualization on a new variable named URL. The url of the data
# visualization page is “https://en.wikipedia.org/wiki/data_visualization”.
#  Use the read_html, html_nodes, and html_text functions to read the texts of all the
# content of URL and store them on a new variable named texts. If you look at the source
# page of any Wikipedia page, you will realize that its content is 
#under a div element with id attribute bodyContent

texts <- URL %>% read_html %>% html_nodes("div[id=bodyContent]")%>% html_text()
texts



library(twitteR) #To get tweets from twitter
library(tidyverse) #For reading the file from a txt file and for the %>% operator
library(tm) #For text analysis
library(wordcloud) #For the wordcloud visualization. You can also use wordcloud2 package
library(syuzhet) # Foe the sentiment analysis

# #Twitter API Info#
# consumer_key <- "put your  api key here"
# consumer_secret <- "put your api secret key here"
# access_token <- "put access token here"
# access_secret <- "put access secret here"
# 
# #Query Info
# hashtag=c("SuperBowlAds", "Dodge", "BigGameBingo", "Pringles")
# numwords=500
# 
# # Connect to twitter
# options(httr_oauth_cache=T)
# setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
# 
# # Save the query on a dataframe named rt_subset
# rt_subset = searchTwitter(hashtag, n=numwords, lang = "en") %>% 
#   tweets1 %>% strip_retweets %>% twListToDF
# 

# I already saved my tweets on a a text file
# The following line will read the text file
# If you are collecting data in real-time, you should skip the following line 
rt_subset <- read_delim(texts,
                        delim ="\t" )
head(rt_subset, n=2)
head(rt_subset$text, n=2)

# Find the frequency of each word and store it on dataframe d
v <- rt_subset$text %>% 
  VectorSource %>% 
  Corpus %>% 
  TermDocumentMatrix %>%
  as.matrix %>%
  rowSums  %>%
  sort(decreasing=TRUE)
d <- data.frame(word = names(v),freq=v, 
                stringsAsFactors = FALSE)

head(d,20)

















ui <- fluidPage(
  titlePanel("Wikipedia Text Visualizer"),
  sidebarLayout(
    sidebarPanel(
      textInput(
        inputId = "input1",label = "Wikipedia key", value = "Data Visualization"
      ),
      sliderInput(
        inputId = "input2", min = 5, max = 50, label = "Number of words",value = 30
      ),
      sliderInput(
        inputId = "input3", min = 1, max = 10, label = "Minimum Frequency", value = 3
      ),
      radioButtons(
        inputId = "input5", label = "Scale method", 
        choices = c("none","sqrt","log2","log"), selected = "sqrt"
      ),
      selectInput(
        inputId = "input4", label = "Color pallet",
        choices = c("Set1","Set2","Set3","Dark2","Accent"), selected = "Set1"
      )
    ),
    mainPanel(
      plotOutput("plot1"),
      uiOutput("images")
    )
  )
)

server <- function(input, output) {
  #### Paste your functions below
  
  
  
  
  
  ##### Paste your functions above
  output$plot1 <- renderPlot({
    wikiWebScraper(input$input1,input$input2,input$input3,input$input4,input$input5)
  })
  
  output$images <- renderUI({
    srcs=wikiImgScraper(input$input1)
    v <- list()
    v <- lapply(1:length(srcs), function(index){ 
      v[[index]] <- column(width = 3,
                           img(src=srcs[index], width = 150, height = 150)
      )
    })
    return(v)
  })
}

shinyApp(ui = ui, server = server)