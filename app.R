#ps6
library(shiny)
library(tidyverse)

fullData <- read_delim("streaming-platform-data.csv")
fullData <- fullData %>%
  select("ID", "Title", "Year", "Age", "Rotten Tomatoes", "Netflix", "Hulu", "Prime Video", "Disney+")
names <- c("Netflix", "Hulu", "Prime Video", "Disney+")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Streaming Services, Movies, and Years"), 
  mainPanel(
    tabsetPanel(
      tabPanel("Data Information", titlePanel("Info About Movies and Streaming Service Dataset"), 
               sidebarPanel(width = "auto",
                            
                            
                            p("This dataset focuses on various streaming services", em("(Netflix, Hulu, Prime Video,"), "and", em("Disney+)"), 
                              "and the movies that they have available, as well as the",
                              strong("movie ratings"), "of each movie. There are several movies in this dataset as well.", "There are", 
                              nrow(fullData), "movies", "Each tab shown here displays different information. The first tab shows general info about 
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
               
               
               sidebarPanel(width = 6, titlePanel("Movies Per Streaming Service Per Year"), 
                            p("The bar plot below shows the amount of movies per year 
                                                that are availiable on each streaming service. 
                                                The slider will allow you to select a particular year to look at.
                                                The years shown on the slider range from 1914 to 2021 which allows for
                                                a broad selection of movies. The checkboxes displayed will also let you 
                                                select which services to show for the selected year"), 
                            fluidRow(column(align = "center", width = 6, sliderInput("year", 
                                        h4(strong("Year")), 
                                        min = min(fullData$Year), 
                                        max = max(fullData$Year), 
                                        min(fullData$Year),
                                        sep = "")), 
                            column(width = 6, align = "center", checkboxGroupInput("checkGroup", 
                                               label = h4(strong("Select Streaming Services")),
                                               choices = names,
                                               selected = names))
                            )
                            , 
                            h4(strong(uiOutput("choosenYearandServices"))), 
                            
               ),
               mainPanel(width = 6, plotOutput("mainplot"))
      ), 
      tabPanel("Data Table by Movie", 
               
               sidebarPanel(
                            titlePanel("Movies by Streaming Service"), 
                            p("The small table below allows you to use the drop down menu to select a movie of your choosing to see data for. 
                   The moves in the drop down menu are shown in alphabetical order. The columns show the movie's ID, the title of
                   of the movie, the year the movie was released, the age rating of the movie, the rating of the movie on Rotten Tomatoes, and 
                   if the streaming service has the movie (1 = the streaming service has the movie, 0 = the streaming service does not have 
                   the movie)"),
                            selectInput("MovieOptions", label = h3("Movie Options"),  
                                        choices = sort(fullData$Title), 
                                        selected = fullData[fullData$Title, 1]),
                            h3(strong(textOutput("choosenM")))
               ), 
               
                  mainPanel(dataTableOutput("table"))
               
               
               ),
      tabPanel("Data Table of All Movies, Years, and Ratings", 
               titlePanel("Movies Per Streaming Service Per Year"), 
               dataTableOutput("allData"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$value <- renderPrint({ 
    input$checkGroup 
  })
  
  output$mainplot <- renderPlot({
    year <- input$year
    selected <- input$checkGroup
    filtered_by_year <- fullData %>% 
      filter(Year == year) 
    number <- c(sum(filtered_by_year$Netflix), 
                sum(filtered_by_year$Hulu), 
                sum(filtered_by_year$`Prime Video`), 
                sum(filtered_by_year$`Disney+`))
    by_service <- data.frame(names, number) %>% 
      filter(names %in% selected)
    print(selected)
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
  
  output$choosenM = renderText({
    paste0("You have selected the movie: ", input$MovieOptions)
  })
  
  output$choosenYearandServices = renderUI({
    HTML("You have selected the year: ", input$year, "<br/>", "You have selected these movie services: ", toString(input$checkGroup), "<br/>") 
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
