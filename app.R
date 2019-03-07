library(shiny)
library(tidyverse)
library(dplyr)
library(shinythemes)
library(sf)
library(rgeos)
library(ggplot2)
library(rmapshaper)
library(janitor)
library(datasets)
library(RColorBrewer)
library(ggrepel)

# Read in data
scatterplot_df1 <- read_csv("cleaned_scatterplot.csv")
bio <- read_csv("bio.csv")

# User interface 
ui <- navbarPage("Exploring Population Growth and the Global Distribution of Red List Species",
                 theme = shinytheme("cerulean"),
                 
                 # Home page panel               
                 tabPanel("Summary",
                          
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(tags$img(src = "turtle.jpg", align = "center", height = '400px', width = '250px')
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
                 
                 # Bar graph panel
                 tabPanel("Bar Graph",
                          titlePanel("Number of Threatened Species by Country"),
                          sidebarLayout(
                            sidebarPanel(selectInput("z", "Country",
                                                     choices = c("Algeria", "Ghana", "Denmark"),
                                                     selected = "Total",
                                                     multiple = FALSE)
                            ),
                            mainPanel(
                              plotOutput(outputId = "bargraph")
                            )
                          )
                          
                 ),
                 
                 # Map panel
                 tabPanel("Map"),
                 
                 # Scatterplot panel
                 tabPanel("Scatterplot",
                          titlePanel("Number of Threatened Species & Projected Population Increase to 2050"),
                          sidebarLayout(
                            sidebarPanel(
                                          
                              #Drop-down menu to choose continent of interest
                              #selectInput("continent", "Continent",
                                          #label = "Select continent:",
                                          #choices = levels(scatterplot_df1$Continent),
                                          #selected = "Africa"),
                              
                              #Drop-down menu to choose species class (X axis)
                              selectInput("x", "Species Class",
                                          choices = c("Mammals", "Birds", "Reptiles", "Amphibians", "Fishes", "Molluscs", "Total"),
                                          selected = "Total",
                                          multiple = FALSE),
                              
                              #Drop-down menu to choose growth rate variant (Y axis)
                              selectInput("y", "Growth Rate Variant",
                                          choices = c("Low", "Medium", "High"),
                                          selected = "High",
                                          multiple = FALSE)
                            ),
                            mainPanel(
                              plotOutput(outputId = "scatterplot")
                            )
                          )
                           
                         )
                 
                 
)



# Define server logic
server <- function(input, output) {

  #scatterplot_subset <- reactive({
    #scatterplot_df1 %>%
      #dplyr::filter(Continent == input$continent)

  #})
  
  # Generate ggplot scatterplot of requested variables
  output$scatterplot <- renderPlot({
    ggplot(data = scatterplot_df1, 
           aes_string(x = input$x, y = input$y)) +
      geom_point() +
      geom_text(label = scatterplot_df1$Country) +
      labs(x = "Number of Threatened Species", y = "Rate of Population Increase (2050)") +
      theme(panel.grid.major = element_line(color = gray(0.5), linetype = "blank", 
                                            size = 0.5), panel.background = element_rect(fill = "white"))


  })

# Generate  bar graph of requested variables
output$bargraph <- renderPlot({
  ggplot(data = bio, 
         aes_string(x = bio$species_type, y = bio$thr_count, fill=input$z)) +
    geom_bar(stat='identity', position='dodge') +
    geom_text(label = bio$Country) +
    labs(x = "Number of Threatened Species", y = "Count") +
    theme(panel.grid.major = element_line(color = gray(0.5), 
          linetype = "blank", size = 0.5), 
          panel.background = element_rect(fill = "white"))+
    theme_classic()+
    xlab("Species Type")+
    ylab("Count of Threatened Species")+
    theme(axis.title = element_text(face="bold"), title = element_text(face="bold"))})  
  
}


# Run the application 
shinyApp(ui = ui, server = server)




