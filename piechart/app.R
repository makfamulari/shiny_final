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
library(ggplot2)

FBI <- read_csv("FBI Top Ten Most Wanted_Fixed - Sheet1.csv") %>% 
  clean_names() 


# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("lumen"),
                 "The FBI's Top Ten Most Wanted List (2010-2020)",
                 tabPanel("The Criminals",
                          sidebarPanel(
                            selectInput("demographics", "Choose a demographic characteristic:",
                                        choices = c("Race" = "race", 
                                                    "Gender" = "gender",
                                                    "Nationality" = "nationality"),
                                        selected = "race")),
                          mainPanel(
                            plotOutput("distPlot"))))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    
    pie_chart <- FBI %>% 
      filter(! (.data[[input$demographics]] == 0)) %>% 
      group_by(.data[[input$demographics]]) %>% 
      count() %>% 
      mutate(prop = (n)/(sum(n))) 
    
    pie_chart %>% 
      ggplot(aes(x = "", y = n, col = .data[[input$demographics]])) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) + 
      scale_fill_brewer("") +
      theme(axis.text.x=element_blank()) +
      theme_classic() 
  })
}




# Run the application 
shinyApp(ui = ui, server = server)
