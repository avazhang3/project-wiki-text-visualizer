library(shiny)
library(tm)
library(wordcloud)
library(tidyverse)
library(rvest)
library(SnowballC)
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

ui <- fluidPage(
    titlePanel("Wikipedia Text Visualizer"),
    sidebarLayout(
            sidebarPanel(
                textInput(
                   inputId = "input1",label = "Wikipedia key", value = "Data Visualization"),
            sliderInput(
                inputId = "input2", min = 5, max = 50, label = "Number of words",value = 30),
            sliderInput(
                inputId = "input3", min = 1, max = 10, label = "Minimum Frequency", value = 3),
            radioButtons(
                inputId = "input5", label = "Scale method", 
                choices = c("none","sqrt","log2","log"), selected = "sqrt"),
            selectInput(
                inputId = "input4", label = "Color pallet",
                choices = c("Set1","Set2","Set3","Dark2","Accent"), selected = "Set1")
            ),
    mainPanel(plotOutput("plot1"),
                uiOutput("images"))
               )
)


server <- function(input, output) {
    #### Paste your functions below
    
    wikiWebScraper <-function(key, num, minFreq, pal , method ){
            # paste your code below
            key = gsub(" ", "_", key)
            URL=paste0("https://en.wikipedia.org/wiki/", key)
            texts <- URL %>% read_html %>% html_nodes("div[id=bodyContent]")%>% html_text()
            myWords=c("can", "just", 
                      "one", "think", 
                      "like", "get", 
                      "see", "will")
            
            v <- texts %>% 
                VectorSource %>% 
                Corpus %>% 
                # Convert the text to lower case
                tm_map(content_transformer(tolower)) %>%
                # Remove numbers
                tm_map(removeNumbers) %>%
                # Remove english common stopwords
                tm_map(removeWords, stopwords("english")) %>%
                # Remove your own stop word 
                # specify your stopwords as a character vector
                tm_map(removeWords, myWords) %>% 
                # Remove punctuations
                tm_map(removePunctuation) %>% 
                # Eliminate extra white spaces
                tm_map(stripWhitespace) %>%
                # Text stemming
                tm_map(stemDocument) %>%
                TermDocumentMatrix %>%
                as.matrix %>%
                rowSums %>%
                sort(decreasing=TRUE) 
            
            if(method=="none") {d <-
                data.frame(word = names(v),freq=v) }
            else if(method=="sqrt") { d <-
                data.frame(word = names(v),freq=round(sqrt(v))) }
            else if(method=="log") { d <-
                data.frame(word = names(v),freq=round(log(v))) }
            else if(method=="log2") { d <-
                data.frame(word = names(v),freq=round(log2(v))) }
            
            set.seed(5)
            
            w <- wordcloud(words = d$word, freq = d$freq, max.words=num,
                           random.order=FALSE, min.freq = minFreq, colors=brewer.pal(8,pal))
            
            return(w)
        }
    
    
    
    wikiImgScraper <-
        function(key) {
            # write your code below
            key = gsub(" ", "_", key)
            URL=paste0("https://en.wikipedia.org/wiki/", key)
            imgList<- URL %>% read_html %>% html_nodes("img")%>% html_attr("src")
            
            return(imgList)
        }
    
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