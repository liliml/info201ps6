#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#for ps6

library(shiny)
library(tidyverse)

# NOTE: set your working directory to this folder when you're working on it, don't
# move the data file.
fullData <- read_delim("streaming-platform-data.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Age, Movies, and Year"), 
  p("There are", nrow(fullData), "movies"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", 
                  "Year", 
                  min = min(fullData$Year), 
                  max = max(fullData$Year), 
                  min(fullData$Year),
                  sep = "")
    ),
    mainPanel(
      tabsetPanel(
        #Text, Plot, and Table are the names of the tabs
        tabPanel("Text", textOutput("stuff")), 
        tabPanel("Plot", plotOutput("mainplot")), 
        tabPanel("Table", titlePanel("Movies Per Streaming Service Per Year"), dataTableOutput("table")),
      )
      # titlePanel("Movies Per Streaming Service Per Year")
      # , 
      # plotOutput("table")
      )
      
  ) 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$stuff <- renderText({
    "hello"  
  })

  output$mainplot <- renderPlot({
    
    # Get year from input
    year <- input$year
    
    filtered_by_year <- fullData %>% 
      filter(Year == year)
    
    names <- c("Netflix", "Hulu", "Prime Video", "Disney+")
    number <- c(sum(filtered_by_year$Netflix), sum(filtered_by_year$Hulu), sum(filtered_by_year$`Prime Video`), sum(filtered_by_year$`Disney+`))
    by_service <- data.frame(names, number)
    by_service
    
    ggplot(by_service) +
      geom_col(mapping = aes(x = names, y = number, fill = factor(names))) +
      labs(title="Movies by Year",
           x = "Streaming Service",
           y = "Movie count") + 
      scale_fill_manual(
        values = c("Netflix" = "red", "Hulu" = "seagreen2", "Prime Video" = "skyblue", "Disney+" = "blue")
      )
  })
  
  output$table <- renderDataTable({
    fullData
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
