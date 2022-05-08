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
library(plotly)

ui <- fluidPage(theme = shinytheme("cerulean"),
                
    titlePanel("World Value Study: Summary of Results from Question Subset"),
    
    selectInput(inputId = "country", label = "Country", choices=country),
    
#plot of average democracy and science responses based on drop down
navlistPanel(
tabPanel("Overview", "This project explores the responses to a subset of questions from the World Values Survey (WVS). The WVS is described as 'a global network of social scientists studying changing values and their impact on social and political life, led by an international team of scholars, with the WVS association and secretariat headquartered in Stockholm, Sweden.'

The data used in this analysis was generated as part of Wave 6 of the WVS (2010-2014) and it explores only a subset of the questions: 

-attitudes toward democracy
-news consumption
-attitudes toward science'

         All tables and charts illustrate the mean response. See the survey questionnaire for details on the question format and scaling (https://www.worldvaluessurvey.org/imgaj/pdf.png)."),
tabPanel("Exploring Attitudes toward Democracy",
    textOutput("text_heading_democracy"),
    plotlyOutput("barplot_democracy", width = "800px"),
    tableOutput("table_democracy"),
    tableOutput("democracy_all_df")),
    
#table of average democracy and science values based on drop down
tabPanel("Exploring Attitudes toward Science",
    #textOutput("text_heading_science"),
    plotlyOutput("barplot_science", width = "800px"),
    #textOutput("text_heading_science"),
    tableOutput("table_science"),
    #textOutput("text_heading_science_all"),
    tableOutput("science_tech_all_df")),
   
))

server <- function(input, output, session) {

    democracy_filtered<-reactive({
        democracy %>%
            filter(C_COW_ALPHA==input$country)
    })
    
    democracy_filtered_table<-reactive({
        democracy %>%
            filter(C_COW_ALPHA==input$country)
        
    })
    
    science_filtered<-reactive({
        science_tech %>%
            filter(C_COW_ALPHA==input$country)
    })
    
    democracy_all_df <- reactive ({
        democracy_all
        
    })
    
    science_tech_all_df <- reactive({
        science_tech_all
        
    })
    
    output$barplot_democracy <- renderPlotly ({
        ggplotly(
            ggplot(democracy_filtered(), aes_string(x="value",y="var"))  +
            geom_bar(stat="identity") +
            theme_minimal()+
            theme(axis.text.x=element_text(size=10)) +
            theme(axis.text.y=element_text(size=10))+
            theme(axis.title.x = element_blank())+
            theme(axis.title.y = element_blank()))
        
    })
    
    output$barplot_science <- renderPlotly({
        ggplotly(
            ggplot(science_filtered(), aes_string(x="value",y="var"))  +
            geom_bar(stat="identity") +
            theme_minimal()+
            theme(axis.text.x=element_text(size=10))+
            theme(axis.text.y=element_text(size=10))+
            theme(axis.title.x = element_blank())+
            theme(axis.title.y = element_blank()))
        
    })
    
    output$text_heading_democracy <- renderText("Democracy Topics: Mean Response Value (selected country)")
    

    output$table_democracy  <- renderTable({
        democracy_filtered_table()
    })
    
    output$text_heading_democracy_all <- renderText("Democracy Topics: Mean Response Value (All countries)")
    
    output$democracy_all_df  <- renderTable({
        democracy_all_df()
    })
    
    output$text_heading_science_all <- renderText("Science Topics: Mean Response Value (All countries)")
    
    output$science_tech_all_df  <- renderTable({
        science_tech_all_df()
    })
        
    output$text_heading_science <- renderText("Science Topics: Mean Response Value (secelected country)")
        
        
    output$table_science  <- renderTable({
            science_filtered()    
    })
}

shinyApp(ui, server)


