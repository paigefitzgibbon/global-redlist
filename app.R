library(shiny)
library(tidyverse)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Exploring Population Growth and the Global Distribution of Red List Species"),
  
  # Sidebar with a picture of threatened species 
  sidebarLayout( position = "left",
    sidebarPanel("Leatherback Sea Turtle"),
    
    # Show a summary of the app on main panel
   navbarPage("Here's the main title!",
              tabPanel(
                h1("Summary of the app"),
                h2("This app will examine the relationship between human
                   population growth and the global distribution of IUCN Red List species. The goal of this app is to help visualize how many and what class of listed species are located in each country compared to historical and projected population growth."),
                h3("Our app will utilize three datasets:"),
                h3("The first dataset contains the number of threatened International Union for the Conservation of Nature (IUCN) Red List species in each country catego- rized by continent (IUCN Red List of Threatened Species 2018)"),
                h3("The second dataset describes 273 observations of total population (includ- ing both sexes) by region, subregion, and country, annually for 1950-2100 (thousands) (United Nations 2017)."),
                h3("The third dataset describes 273 observations of average annual rates of population change by region, subregion, and country for 1950-2100 (per- centage) (United Nations 2017).")
              )
   ),
     mainPanel(
       tabsetPanel(
         tabPanel("Home Page",
                  plotOutput("distPlot")) )
        
      )
    )
  )


# Define server logic required to draw a histogram
#server <- function(input, output) {
   
   #output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
     # x    <- faithful[, 2] 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
   #})
#}

# Run the application 
#shinyApp(ui = ui, server = server)

