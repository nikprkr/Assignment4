#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cerulean"),
    selectInput("democracy", label = "Country",country),
    plotOutput("plot_science", width = "400px"),
    plotOutput("plot_democracy", width = "400px"),
    dataTableOutput("table_science"),
    dataTableOutput("table_democracy"),
    textOutput("text_heading_science"),
    tableOutput("science_tech_all_df"),
    textOutput("text_heading_democracy"),
    tableOutput("democracy_all_df")
   
)

server <- function(input, output, session) {

    dataset <- reactive({
        get(input$democracy)
    })
    
    output$plot_science <- renderPlot({
        plot(dataset())
    })
    
    output$table_democracy  <- renderTable({
        dataset()
    })
    
    output$text_heading_democracy <- renderText("Democracy Topics: Mean Response Value")
    output$democracy_all_df  <- renderTable({
        democracy_all_df
    })
    
    output$text_heading_science <- renderText("Science Topics: Mean Response Value")
    output$science_tech_all_df  <- renderTable({
        science_tech_all_df
    })
}

shinyApp(ui, server)




# Define UI for application that draws a histogram
# ui <- fluidPage(
#     selectInput("country",
#                 "Please select a country",
#                 country))
# 
#     # Application title
#     titlePanel("World Value Study")
# 
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         x    <- faithful[, 2]
#         bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
#         # draw the histogram with the specified number of bins
#         hist(x, breaks = bins, col = 'darkgray', border = 'white')
#     })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
