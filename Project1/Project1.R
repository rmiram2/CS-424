library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(leaflet)
library(scales)
library(stringr)

data <- read.csv("litterati challenge-65.csv")

#labeling the tags with blank values
data$tags <- as.character(data$tags)
data$tags[data$tags==""] <- "untagged"
data$tags <- as.factor(data$tags)

#shrinking the dataset
df <- subset(data, select = -c(url, user_id, username))
#df

#counting the litter picked up
litterpicked <- nrow(subset(data, select = c(tags)))

#counting the littered picked by an individual for top 10
countoflitter <- subset(data, select = c(username, tags))
top10pickers <- aggregate(cbind(count = username) ~ username, 
                          data = countoflitter, 
                          FUN = function(x){NROW(x)})
top10pickers <- top10pickers[with(top10pickers,order(-count)),]
top10pickers <- top10pickers[1:10, ]

#Information for a bar graph for littered picked up per day
litterperday <- subset(data, select = c(litterTimestamp))
litterperday <- data.frame(date = litterperday,do.call(rbind,str_split(litterperday$litterTimestamp, " ")))
colnames(litterperday) <- c("time", "date", "times")

countlitterperday <- aggregate(cbind(count = date) ~ date, 
                              data = litterperday, 
                              FUN = function(x){NROW(x)})

#litter picked per day
litterperday$day <- wday(litterperday$time, label=TRUE)
litterperdaycount <- aggregate(cbind(count = day) ~ day, 
                          data = litterperday, 
                          FUN = function(x){NROW(x)})

#litter picked per hour of the day
litterperday$hour <- hour(litterperday$time)
litterperhour <- aggregate(cbind(count = hour) ~ hour, 
                           data = litterperday, 
                           FUN = function(x){NROW(x)})


#Creating a shiny app
ui <- dashboardPage( 
  dashboardHeader(
    title = "CS 424 Project 1"
  ),
  dashboardSidebar(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          box(
            leafletOutput(outputId = "mymap")
          ),
          
          box(
            h1("The number of littered picked up: ", litterpicked, align = "center"),
            
            tableOutput("table")
            
          ),
        ),
        
        fluidRow(
          box(
            h2("Amount of Litter picked up in a day", align = "center"),
            plotOutput("bar", height = 800, width = 1500)
          ),
        ),
        
        fluidRow(
          box(
            h2("Amount of litter picked up per day of the week", align = "center"),
            plotOutput("daybar", height = 600, width = 800)
          ),
          box(
            h2("Amount of litter picked up per hour of the day", align = "center"),
            plotOutput("hourbar", height = 600, width = 800)
          )
        )
      ),
      tabItem(tabName = "widgets",
              h2("Hello")
              )
    )
  )
)

server <- function(input, output) {
  #Map for litter
  output$mymap <- renderLeaflet({leaflet(df) %>% 
      setView(lng = -87.8, lat = 41.87, zoom = 13)  %>%
      addTiles() %>% 
      addMarkers(lat = df$lat, lng = df$lon, clusterOptions = markerClusterOptions(), popup = as.character(df$tags))})
  
  #table for top 10 pickers
  output$table <- renderTable({top10pickers})
  
  #bargraph for litter picked per date
  output$bar <- renderPlot({
    ggplot(countlitterperday, aes(date, count)) + geom_bar(stat = "identity")
  })
  
  #bargraph for litter picker per day of the week
  output$daybar <- renderPlot({
    ggplot(litterperdaycount, aes(day, count, fill=day)) + geom_bar(stat = "identity")
  })
  
  #bargraph for litter picked up each hour of the day
  output$hourbar <- renderPlot({
    ggplot(litterperhour, aes(hour, count, fill=hour)) + geom_bar(stat = "identity")
  })
}

shinyApp(ui, server)

