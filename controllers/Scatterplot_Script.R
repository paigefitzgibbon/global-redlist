library(tidyverse)
library(dplyr)
library(janitor)
library(ggrepel)

threatened_species <- read_csv("IUCN_thr_species_data.csv")
population <- read_csv("PopGrowthRate_Medium.csv")

worldpop2050 <- population %>% 
  select(Country, `2050-2055`)

total_species2 <- threatened_species %>% 
  adorn_totals("row")

total_species2$Plants <- NULL
total_species2$Fungi_Protists <- NULL
total_species2$Other_Invertebrates <- NULL
total_species2$Region <- NULL

df_species_worldpop <- merge(total_species2, worldpop2050)

summary(df_species_worldpop)
head(df_species_worldpop)

##X axis = total threatened species (or species by class) (Numbers of threatened species (Critically Endangered, Endangered and Vulnerable)
##Y axis = projected population increase to 2050

plot(x = df_species_worldpop$Total, y = df_species_worldpop$`2050-2055`)
plot(x = df_species_worldpop$Mammals, y = df_species_worldpop$`2050-2055`)

rate2050 <- df_species_worldpop$`2050-2055`
total <- df_species_worldpop$Total

ggplot(df_species_worldpop, aes(x = total, y = rate2050)) +
  geom_point() +
  xlab("Number of Threatened Species") +
  ylab("Rate of Population Increase to 2050") +
  



