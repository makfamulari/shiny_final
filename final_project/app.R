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

FBI2010 <- read_csv("FBI Top Ten Most Wanted  - Sheet1.csv") %>% 
  clean_names() 

FBI2000 <- read_csv("FBI_2000 - Sheet1.csv") %>% 
  clean_names()

# Define UI for application that draws a histogram
ui <- navbarPage(theme = shinytheme("sandstone"),
                 "The FBI's Top Ten Most Wanted List (2010-2020)",
                 tabPanel("About",
                          column(7,
                                 h1("Background"),
                                 p("The Federal Bureau of Investigation introduced its now infamous Top Ten Most 
                                   Wanted List on March 14th, 1950. The list was first designed to bring publicity 
                                   to the most dangeous fugitives that otherwise did not merit media attention. 
                                   Of the 523 fugitives that have been placed on the list, 488 have been apprehended 
                                   or located."),
                                 p("There are two criteria a criminal must meet to merit placement on the list. 
                                   First, the criminal must either be considered particularily dangerous due to 
                                   current charges OR the criminal must have a meaningful record involving
                                   serious crimes. Second, the publicity resulting from placement on the 
                                   list must be of assistance in apprehending the criminal. In other words,
                                   if the criminal is already garnering meaningful media coverage, their placement
                                   would be considered unnecessary."),
                                 p("Removal from the list is only possible under one of the following conditions:"),
                                 p("- They are captured"),
                                 p("- The case of the criminal is dismissed"),
                                 p("- The criminal no longer fits the criteria"),
                                 h1("Purpose"),
                                 p("The purpose of this project is to investigate the members of the FBI's Top Ten Most
                                   Wanted list from the years 2010-2020. Specifically, we aim to discover the types of
                                   criminals that merit placement based upon biographical information, the nature of their
                                   crimes, and their victims. The list's members beginning in the year 2010 will be
                                   studied: their lives, crimes, and backgrounds."),
                                 h1("Data"),
                                 p("Information about data"))),
                 tabPanel("The Crimes"),
                 tabPanel("The Criminals"),
                 tabPanel("Efficacy of FBI's List from 2010-2020",
                 h3("How do crimes differ across gender?"),
                 sidebarPanel(
                   span(),
                   selectInput("gender", "Gender",
                               choices = list("Male" = "male",
                                              "Female" = "female"),
                               selected = "male")),
                 
                 mainPanel(plotOutput("pie_chart"))),
                 tabPanel("Gang Related Crimes"),
                 tabPanel("Notable Cases"),
                 tabPanel("Contact",
                 h1("About me"),
                 p("You can reach me at ",
                            a("mfamulari@college.harvard.edu",
                   href = "mailto: mfamulari@college.harvard.edu"))))
  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   output$pie_chart <- renderPlot({
       pie_chart_gender <- FBI2010 %>% 
         group_by(reason_for_removal) %>% 
         count()
       
       ggplot(pie_chart_gender, aes(x = reason_for_removal, y = n, fill = reason_for_removal)) +
         geom_col(stat = "identity") +
         labs(
           title = "Reasons for Removal from FBI's Top Ten List",
           fill = "Reason",
           x = "Reason",
           y = "Count"
         ) +
         theme_classic()
       
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

