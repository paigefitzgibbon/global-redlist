library(shiny)
library(tidyverse)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage("Exploring Population Growth and the Global Distribution of Red List Species",
                 theme = shinytheme("cerulean"),
   
  # Home page panel               
  tabPanel("Summary",
           
           fluidPage(
             sidebarLayout(
               sidebarPanel(tags$img(src = "turtle.jpg", align = "center", height = '500px', width = '250px')
                            ),
               mainPanel(
                 h4("Summary of the app:"),
                 h4("This app will examine the relationship between human
                    population growth and the global distribution of IUCN Red List species. The goal of this app is to help visualize how many and what class of listed species are located in each country compared to historical and projected population growth."),
                 br(),
                 br(),
                 h4("Our app will utilize three datasets:"),
                 br(),
                 h4("1) Number of threatened International Union for the Conservation of Nature (IUCN) Red List species in each country categorized by continent (IUCN Red List of Threatened Species 2018)."),
                 h4("2) Total population (including both sexes) by region, subregion, and country, annually for 1950-2100 (thousands) (United Nations 2017)."),
                 h4("3) Average annual rates of population change by region, subregion, and country for 1950-2100 (percentage) (United Nations 2017)."),
                 br(),
                 br(),
                 h5("Creators: Paige Fitzgibbon, Rachel Kenny, and Madison Meltzer")
               )
                          
                        )
                 )
           ),
           
  tabPanel("Bar Graph"),
  tabPanel("Map"),
  tabPanel("Scatterplot")
            

           )

  
   
# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

