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
                                 p("There are no current existing databases aggregating information on members of the 
                                   FBI's Most Wanted List. Data scrubbing was conducted by myself using information from
                                   the FBI's archived most wanted lists. These lists contained information on dates of placement, 
                                   dates of removal, and biographical information such as race, gender, and nationality. For a significant 
                                   number of past criminals, the nature of crimes/profiles were not available through the FBI. To determine 
                                   the reasons for placement (including suspected charges
                                   and convicted charges, if apprehended), archived court files were utilized. Given the fact that
                                   many of the criminals had lengthy criminal records, only the two most significant charges were considered
                                   for each criminal."))),
                 tabPanel("The Crimes",
                 column(7,
                 h1("What crimes end up on the list?"),
                        p("The following chart lists the occurance of crimes on the FBI's Most Wanted Lists 
                          for the years 2000-2020.")),
                 column(12,
                 mainPanel(plotOutput("crime_breakdown")))),
                 tabPanel("The Criminals",
                 column(4,
                 h1("Demographics of Criminals"),
                          p("To view the demographics of the criminals placed on the FBI's Most Wanted List from
                            the years 2000 to 2020, please select a demographic characteristic."),
                 h2("Race Findings"),
                 p("The majority of offenders are identified as white."),
                 h2("Gender Findings"),
                 p("The vast majority of offenders on the list are male."),
                 h2("Nationality Findings"),
                 p("Most offenders are American citizens.")),
                 column(7,
                 sidebarPanel(
                 selectInput("demographics", "Choose a demographic characteristic:",
                             choices = c("Race" = "race", 
                                         "Gender" = "gender",
                                         "Nationality" = "nationality"),
                             selected = "race")),
                 mainPanel(
                   plotOutput("distPlot")))),
                 tabPanel("Efficacy of FBI's List from 2000-2020"),
                 tabPanel("Gang Related Crimes"),
                 tabPanel("Notable Cases"),
                 tabPanel("Contact",
                 h1("About me"),
                 p("You can reach me at ",
                            a("mfamulari@college.harvard.edu",
                   href = "mailto: mfamulari@college.harvard.edu"))))
  

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   output$crime_breakdown <- renderPlot({
       crime <- FBI %>% 
         select(-name,
                -placed_on_list,
                - removed_from_list,
                - reason_for_removal,
                - race,
                - gender,
                - nationality,
                - gang_related,
                - police_victim) %>% 
         pivot_longer(everything(),
                      names_to = "crime",
                      values_to = "count") %>% 
         filter(count == 1) %>% 
         select(crime) %>% 
         count(crime) %>% 
         filter(n > 1)
       
       ggplot(crime, aes(x = crime, y = n, fill = crime)) +
         geom_col(stat = "identity") +
         labs(
           title = "Frequency of Crimes on the List (2000-2020)",
           fill = "Type of Crime",
           x = "Crime",
           y = "Count",
           caption = "*Excluding crimes that appeared on the list only once."
         ) +
         theme_dark()
       
     })
   
   output$distPlot <- renderPlot({
     
     pie_chart <- FBI %>% 
       filter(! (.data[[input$demographics]] == 0)) %>% 
       group_by(.data[[input$demographics]]) %>% 
       count() %>% 
       mutate(prop = (n)/(sum(n))) 
     
     pie_chart %>% 
       ggplot(aes(x = "", y = n, fill = .data[[input$demographics]])) +
       geom_bar(width = 1, stat = "identity") +
       coord_polar("y", start = 0) + 
       scale_fill_brewer(palette = "Spectral") +
       theme_dark() +
       theme(axis.text = element_blank(),
             axis.ticks = element_blank(),
             panel.grid  = element_blank()) +
       labs(
         x = NULL,
         y = NULL, 
         fill = NULL
       )
   })
}
     
   


# Run the application 
shinyApp(ui = ui, server = server)

