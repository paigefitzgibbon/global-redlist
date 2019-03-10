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

scatterplot_df1 <- read_csv("clean_scatterplot.csv")

bio <- read_csv("bio.csv")

bio$Country <- as.character(bio$Country)

country_vector <- bio$Country

PopulationPredictions <- read_csv("PopulationPredictions.csv", 
                                  col_types = cols(Total2000 = col_factor(levels = c("2", 
                                                                                     "3", "4", "5", "6", "7", "8", "9", 
                                                                                     "10")), Total2010 = col_factor(levels = c("2", 
                                                                                                                               "3", "4", "5", "6", "7", "8", "9", 
                                                                                                                               "10")), Total2020 = col_factor(levels = c("2", 
                                                                                                                                                                         "3", "4", "5", "6", "7", "8", "9", 
                                                                                                                                                                         "10")), Total2030 = col_factor(levels = c("2", 
                                                                                                                                                                                                                   "3", "4", "5", "6", "7", "8", "9", 
                                                                                                                                                                                                                   "10")), Total2040 = col_factor(levels = c("2", 
                                                                                                                                                                                                                                                             "3", "4", "5", "6", "7", "8", "9", 
                                                                                                                                                                                                                                                             "10")), Total2050 = col_factor(levels = c("2", 
                                                                                                                                                                                                                                                                                                       "3", "4", "5", "6", "7", "8", "9", 
                                                                                                                                                                                                                                                                                                       "10"))))
world_outline <- read_sf(dsn = ".", layer = "countries")
world_df <- full_join(world_outline, PopulationPredictions, by = "COUNTRY")
cols <- c("2" = "#FFFFCC", "3" = "#FFEDA0", "4" = "#FED976", "5" = "#FEB24C", "6" = "#FD8D3C", "7" = "#FC4E2A", "8" = "#E31A1C", "9" = "#BD0026", "10" = "#800026")

# Define plots
Graph2000 <- ggplot(data = world_df) + 
  geom_sf(aes(fill = Total2000), 
          colour = "gray10", 
          show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(x = "", y = "", title = "Distribution of Worldwide Population (Decile)")

Graph2010<- ggplot(data = world_df) + 
  geom_sf(aes(fill = Total2010), 
          colour = "gray10", 
          show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(x = "", y = "", title = "Distribution of Worldwide Population (Decile)")

Graph2020 <- ggplot(data = world_df) + 
  geom_sf(aes(fill = Total2020), 
          colour = "gray10", 
          show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(x = "", y = "", title = "Distribution of Worldwide Population (Decile)")


Graph2030 <- ggplot(data = world_df) + 
  geom_sf(aes(fill = Total2030), 
          colour = "gray10", 
          show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(x = "", y = "", title = "Distribution of Worldwide Population (Decile)")

Graph2040<- ggplot(data = world_df) + 
  geom_sf(aes(fill = Total2040), 
          colour = "gray10", 
          show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(x = "", y = "", title = "Distribution of Worldwide Population (Decile)")

Graph2050<- ggplot(data = world_df) + 
  geom_sf(aes(fill = Total2050), 
          colour = "gray10", 
          show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_fill_manual(values = cols) +
  theme_minimal() +
  coord_sf(datum=NA) +
  labs(x = "", y = "", title = "Distribution of Worldwide Population (Decile)")


# User interface 
ui <- navbarPage("Exploring Population Growth and the Global Distribution of IUCN Red List Species",
                 theme = shinytheme("flatly"),
                 
                 # Home page panel               
                 tabPanel("Summary",
                          
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(tags$img(src = "turtle.jpg", align = "center", width="100%")
                              ),
                              mainPanel(
                                br(),
                                h5("This app will examine the relationship between human
                                   population growth and the global distribution of IUCN Red List species (Critically Endangered, Endangered and Vulnerable). 
                                   The goal of this app is to help visualize how many and what class of listed species are located in each country compared to historical and projected population growth."),
                                br(),
                                br(),
                                br(),
                                h5("Our app will utilize three datasets:"),
                                br(),
                                h5("1) Number of threatened International Union for the Conservation of Nature (IUCN) Red List species in each country categorized by continent (IUCN Red List of Threatened Species 2018)."),
                                h5("2) Total population (including both sexes) by region, subregion, and country, annually for 1950-2100 (thousands) (United Nations 2017)."),
                                h5("3) Average annual rates of population change by region, subregion, and country for 1950-2100 (percentage) (United Nations 2017)."),
                                br(),
                                br(),
                                br(),
                                h6("Creators: Paige FitzGibbon, Rachel Kenny, and Madison Meltzer")
                                )
                              
                            )
                          )
                 ),
                 
                 # Bar graph panel
                 tabPanel("Bar Graph",
                          titlePanel("Number of IUCN Threatened Species by Country"),
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("country_1", "Country 1",
                                          choices = c("Algeria","China","Denmark","France","Ghana"),
                                                     selected = "China",
                                                     multiple = FALSE),
                              selectInput("country_2", "Country 2",
                                          choices = c("Algeria","China","Denmark","France","Ghana"),
                                          selected = "Ghana",
                                          multiple = FALSE)
                            ),
                            mainPanel(
                              plotOutput(outputId = "bargraph")
                            )
                          )
                          
                 ),
                 
                 # Map panel
                 tabPanel("Map",
                          titlePanel("Distribution of Worldwide Population"),
                          sidebarLayout(
                            sidebarPanel(sliderInput("year", "Year",
                                                     min = 2000, max = 2050,
                                                     value = 2000, step = 10)),
                            mainPanel(
                              plotOutput(outputId = "map")
                            )
                          )
                          
                 ),
                 
                 # Scatterplot panel
                 tabPanel("Scatterplot",
                          titlePanel("Countries At Risk"),
                          sidebarLayout(
                            sidebarPanel(
                                          
                              #Drop-down menu to choose continent of interest
                              radioButtons("continent", "Continent",
                                           label = "Highlight a Continent:",
                                          choices = c(Africa = "Africa", Asia = "Asia", Europe = "Europe", 'North America' = "North America", Oceania = "Oceania", 'South America' = "South America")
                                          ),
                              
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

  scatterplot_filtered <- reactive({
    
    scatterplot_df1 %>%
    filter(Continent == input$continent)
  })

  bio_df <- reactive({
    bio %>%
    filter(Country == input$country_1 | Country == input$country_2)
  })
  
  # Generate ggplot scatterplot of requested variables 
  output$scatterplot <- renderPlot({

    ggplot(data = scatterplot_filtered(), 
           aes_string(x = input$x, y = input$y)) +
      geom_point(aes(fill = input$continent)) +
      geom_text(aes(label = Country)) +
      labs(x = "Number of IUCN Listed Threatened Species", y = "Rate of Population Increase (2050)") +
      theme(panel.grid.major = element_line(color = gray(0.5), linetype = "blank", 
                                            size = 0.5), panel.background = element_rect(fill = "white"))
  })

# Generate  bar graph of requested variables
output$bargraph <- renderPlot({
  ggplot(data = bio_df(), 
         aes(x = bio_df()$species_type, y = bio_df()$thr_count, fill = bio_df()$Country)) +
    geom_bar(stat="identity", position="dodge") +
    labs(x = "Species Type", y = "Count") +
    theme(panel.grid.major = element_line(color = gray(0.5), 
          linetype = "blank", size = 0.5), 
          panel.background = element_rect(fill = "white"))+
    theme_classic()+
    theme(axis.title = element_text(face="bold"), title = element_text(face="bold"))+
    scale_fill_brewer(palette="Paired")+
    scale_x_discrete(expand=c(0.15,0))+
    scale_y_continuous(expand=c(0,0))+
    labs(fill="Country")
  })

output$map <- renderPlot({
  if (input$year == 2000) 
  {plot(Graph2000)}
  else if (input$year == 2010)
  {plot(Graph2010)}
  else if (input$year == 2020)
  {plot(Graph2020)}
  else if (input$year == 2030)
  {plot(Graph2030)}
  else if (input$year == 2040)
  {plot(Graph2040)}
  else (input$year == 2050)
  {plot(Graph2050)}}) 

 
  
}





# Run the application 
shinyApp(ui = ui, server = server)



