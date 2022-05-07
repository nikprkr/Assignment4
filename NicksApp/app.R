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
                
    titlePanel("World Value Study: Summary of Results from Question Subset"),
    
    selectInput(inputId = "country", label = "Country", choices=country),
    
#plot of average democracy and science responses based on drop down
navlistPanel(
tabPanel("Overview", "This project explores how sex, religious affiliation, and income influence the degree to which respondents agree with the following statements:

    When a mother works for pay, the children suffer
    When jobs are scarce, employers should give priority to [NATIONALITY] people over immigrants

The data used in this analysis was generated as part of the 2017 European Value Study (EVS). The EVS is described as 'a large-scale, cross-national and longitudinal survey research program on how Europeans think about family, work, religion, politics, and society. Repeated every nine years in an increasing number of countries, the survey provides insights into the ideas, beliefs, preferences, attitudes, values, and opinions of citizens all over Europe.'"),
tabPanel("Exploring Attitudes toward Democracy",
    textOutput("text_heading_democracy"),
    plotOutput("barplot_democracy", width = "400px"),
    dataTableOutput("table_democracy"),
    tableOutput("democracy_all_df")),
    
#table of average democracy and science values based on drop down
tabPanel("Exploring Attitudes toward Science",
    textOutput("text_heading_science"),
    plotOutput("barplot_science", width = "400px"),
    dataTableOutput("table_science"),
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
    
    democracy_all_df <- democracy_all
    
    science_tech_all_df <- science_tech_all
    
    output$barplot_democracy <- renderPlot({
        ggplot(democracy_filtered(), aes_string(x="value",y="var"))  +
            geom_bar(stat="identity") +
            theme_minimal()+
            theme(axis.text.x=element_text(size=15)) +
            theme(axis.text.y=element_text(size=15))
        
    })
    
    output$barplot_science <- renderPlot({
        ggplot(science_filtered(), aes_string(x="value",y="var"))  +
            geom_bar(stat="identity") +
            theme_minimal()+
            theme(axis.text.x=element_text(size=15))+
            theme(axis.text.y=element_text(size=15))
        
    })
    
    output$table_democracy  <- renderTable({
        democracy_filtered_table()
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


