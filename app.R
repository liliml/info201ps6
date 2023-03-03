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
#alphaorderFullData <- fullData[order(fullData$Title), ]


names <- c("Netflix", "Hulu", "Prime Video", "Disney+")
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Age, Movies, and Year"), 
  p("There are", nrow(fullData), "movies"),
  
  #sidebarLayout(
    #sidebarPanel(
      # sliderInput("year", 
      #             "Year", 
      #             min = min(fullData$Year), 
      #             max = max(fullData$Year), 
      #             min(fullData$Year),
      #             sep = "")
    #),
  
    mainPanel(
      tabsetPanel(
        #Text, Plot, and Table are the names of the tabs
        tabPanel("Data Information", titlePanel("Info About Movies and Streaming Service Dataset"), 
                 sidebarPanel(width = "auto",
                              
                              
                   p("This dataset focuses on various streaming services", em("(Netflix, Hulu, Prime Video,"), "and", em("Disney+)"), 
                                                    "and the movies that they have available, as well as the",
                                                     strong("movie ratings"), "of each movie. Each tab shown here 
                                                     displays different information. The first tab shows general info about 
                                                     this dataset, the second tab shows a bar graph that can be interacted with that",
                                                     strong("allows the user to pick a year to show the amount of movies for each streaming 
                                                            service that year, as well as unselect streaming services to show for that year."), "
                                                            The third panel shows a small table of a movie, year, if the streaming services have the 
                                                            movies or not", em("(1 means the streaaming service has the movie, 0 means the streaming service
                                                            doesn't have the movie)"), "as well as the movie rating and other data relating to the movie title.",
                                                            strong("The user pickes a movie using the dropdown menu, and after the movie is picked, data for that
                                                                   movie will be shown."), 
                                                            "The fourth pannel shows", strong("all the data"), "including movie titles, movie ratings, the year the
                                                            movie is from, if streaming platforms have the movie or not, and more."))
                 ), 
        tabPanel("Graph", 
                 titlePanel("Movies Per Streaming Service Per Year"), 
                 sidebarPanel(
                   sliderInput("year", 
                               "Year", 
                               min = min(fullData$Year), 
                               max = max(fullData$Year), 
                               min(fullData$Year),
                               sep = "")
                 ),
                 sidebarPanel(  
                   checkboxGroupInput("checkGroup", 
                                      label = h3("Select Streaming Services"),
                                      choices = names,
                                      selected = names),
                 ), 
                 plotOutput("mainplot")), 
        #FOR THIS TABLE, CREATE A WIDGET BELOW THAT WILL ALLOW IT TO ONLY SHOW ONE ROW AT A TIME, AND WILL HAVE A DROP DOWN 
        #FOR EACH MOVIE AND WILL DISPLAY THE ROW FOR THAT MOVIE
        tabPanel("Data Table by Movie", 
                 titlePanel("Movies by Streaming Service"), 
                 sidebarPanel(
                   selectInput("MovieOptions", label = h3("Movie Options"), 
                               choices = sort(fullData$Title), 
                               selected = fullData[fullData$Title, 1]),
                 ),
                 dataTableOutput("table")),
        tabPanel("Data Table of All Movies, Years, and Ratings", 
                 titlePanel("Movies Per Streaming Service Per Year"), 
                 
                 dataTableOutput("allData"))
      )
    )
    
  #) 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # output$stuff <- renderText({
  #   #p("There are", nrow(fullData), "movies")
  #   #strong("hello")  
  #   #
  # })
  
 
  
  output$value <- renderPrint({ 
    input$checkGroup 
  })
  
  output$mainplot <- renderPlot({
    # Get year from input
    year <- input$year
    selected <- input$checkGroup
    #print(input$checkGroup)
    #NEED TO FIX ZERO ISSUE
    # if (length(selected) == 0) {
    #   by_service$number = 0
    # }
    filtered_by_year <- fullData %>% 
      filter(Year == year) #%>% 
    number <- c(sum(filtered_by_year$Netflix), 
                sum(filtered_by_year$Hulu), 
                sum(filtered_by_year$`Prime Video`), 
                sum(filtered_by_year$`Disney+`))
    by_service <- data.frame(names, number) %>% 
      filter(names %in% selected)
      print(selected)
    # cat("-- by service\n")
    # print(by_service)
    ggplot(by_service) +
      geom_col(mapping = aes(x = names, y = number, fill = factor(names))) +
      labs(title="Movies by Year",
           x = "Streaming Service",
           y = "Movie count") + 
      scale_fill_manual(
        values = c("Netflix" = "red", "Hulu" = "seagreen2", "Prime Video" = "skyblue", "Disney+" = "blue")
      )
    
      
  })
  
  output$value <- renderPrint({ 
    input$MovieOptions 
  })
  
  output$table <- renderDataTable({
    selectedMovie <- input$MovieOptions
    fullData %>% 
      filter(Title == selectedMovie)
  })
  
  output$allData <- renderDataTable({
    fullData
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
