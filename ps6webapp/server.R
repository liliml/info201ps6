#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#for ps6:
library(shiny)
library(ggplot2)
library(shinyjs)
library(DT)

function(input, output, session) {
  # observeEvent(input$controller, {
  #   #updateTabsetPanel(session, )
  # })
  output$table <- renderDataTable(iris)
  output$ui <- renderUI({
    tags$div(
      style = 'pading: 40px', 
      dataTableOutput('df'),
      tags$br(),
      verbatimTextOutput('text')
    )
  })
  
  output$df <- renderDataTable({
    DT::datatable(
      diamonds, 
      options = list(
        searching = FALSE,
        pageLength = 10
      )
    )
  })
  output$text <- renderText('Nothing is selected')
}

# # Define server logic required to draw a histogram
# function(input, output, session) {
#     
# 
#     output$distPlot <- renderPlot({
# 
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white',
#              xlab = 'Waiting time to next eruption (in mins)',
#              main = 'Histogram of waiting times')
# 
#     })
# 
# }
