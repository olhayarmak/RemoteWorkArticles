#loading libraries
#library(rsconnect)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(shinydashboard)
library(shiny)



########################################################################################

remotesearch <- read.csv("remotesearch.csv")


#get publisher name from articles
remotesearch$publisher <- trimws(sub('.*-', '', remotesearch$title))

#dates transformation
remotesearch$datestart <- substr(remotesearch$published, 6, nchar(remotesearch$published)-13)
remotesearch$updated_string <- gsub(" ", "", remotesearch$datestart)
remotesearch$Date <- as.Date(remotesearch$updated_string, "%d%B%Y")
class(remotesearch$Date)

#put date and title in data frame
df22 <- data.frame(date = remotesearch$Date,articles = remotesearch$title)

#calculating number of articles by day
newframe <- df22 %>% 
  group_by(day = lubridate::floor_date(date, 'day')) %>%
  summarize(articles = n())


newframemonth <- df22 %>% 
  group_by(month = lubridate::floor_date(date, 'month')) %>%
  summarize(articles = n())

#putting article and publisher in new data frame
dfpubl <- data.frame(publisher = remotesearch$publisher,articles = remotesearch$title)



#number of articles by publisher
newframepublisher <- dfpubl %>% 
  group_by(publisher = publisher) %>%
  summarize(articles = n())




#plotting number of articles by publisher
plotpublisher<-
  
  newframepublisher %>%     
  filter( articles > 2) %>%
  ggplot(newframepublisher,
         mapping = aes(reorder(publisher, -articles),articles)) + 
  geom_bar(stat = "identity", fill = '#0099f9')+
  geom_text(
    aes(x = publisher, y = articles, label = articles), 
    hjust = -0.5, size = 2,
    position = position_dodge(width = 1),
    inherit.aes = TRUE
  )+
  labs(
    title = "News Articles About Remote Work (by publisher)",
    subtitle = "Data from Jul 2023 to Nov 2023",
  )+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 90)
  )


plotpublisher <- plotpublisher + coord_flip() +labs(x ="Publisher", y = "Articles")

#preparing data for word map

#Create a vector containing only the text
title <- sub("\\-.*", "", remotesearch$title)
# Create a corpus  
docs <- Corpus(VectorSource(title))


docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df10 <- data.frame(word = names(words),freq=words)

########################################################################################
#app
ui <- dashboardPage(
  dashboardHeader(title = "Remote Work"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1",width="100%",height = 535)),
      box(plotOutput("plot2",width="100%",height = 535)),
      column(6,box(wordcloud2Output("plot3",width = "100%",height = 450),width = "100%",height = "100%", title = h1("Word Map", align="center",style = 'font-size:15px;color:black;'))),
      column(6,box(plotOutput("plot4",width = "100%",height = 535),width = "100%",height = "100%"))
      
      
    )
  )
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot(
    ggplot(newframe, aes(x = day, y = articles)) +
      geom_line(color = '#0099f9',size = 0.2) +
      geom_point(color = '#0099f9',size = 1 ) +
      labs(
        title = "News Articles Abount Remote Work",
        subtitle = "Data from July 2023 to November 2023",
      )+
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )
    
  )
  
  output$plot2 <- renderPlot( newframemonth %>%     
                                filter( month != '2023-11-01') %>%
                                ggplot(newframemonth  = na.omit('2023-11-01'),
                                       aes(month,articles)) + 
                                geom_bar(stat = "identity", fill = '#0099f9')+
                                geom_label(
                                  aes(label = articles),
                                  nudge_x = 0.25,
                                  nudge_y = 0.25,
                                )+
                                labs(
                                  title = "News Articles About Remote Work (by month)",
                                  subtitle = "Data from Jul 2023 to Oct 2023",
                                )+
                                theme(
                                  plot.title = element_text(hjust = 0.5),
                                  plot.subtitle = element_text(hjust = 0.5)
                                ))
  
  
  output$plot3 <- renderWordcloud2(df10 %>%     
                                     filter(word != "remote" | word != "work") %>%
                                     wordcloud2(data=df10, size=3, color='random-dark'))
  
  
  output$plot4 <- renderPlot( newframepublisher %>%     
                                filter( articles > 2) %>%
                                ggplot(newframepublisher,
                                       mapping = aes(reorder(publisher, -articles),articles)) + 
                                geom_bar(stat = "identity", fill = '#0099f9')+
                                geom_text(
                                  aes(x = publisher, y = articles, label = articles), 
                                  hjust = -0.5, size = 2,
                                  position = position_dodge(width = 1),
                                  inherit.aes = TRUE
                                )+
                                
                                
                                
                                labs(
                                  title = "News Articles About Remote Work (by publisher)",
                                  subtitle = "Data from Jul 2023 to Nov 2023",
                                )+
                                theme(
                                  plot.title = element_text(hjust = 0.5),
                                  plot.subtitle = element_text(hjust = 0.5),
                                  axis.text.x = element_text(angle = 90)
                                )
                              + coord_flip() +labs(x ="publisher", y = "articles"))
  
}
shinyApp(ui = ui, server = server)
