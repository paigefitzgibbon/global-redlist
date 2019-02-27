output$map <- renderPlot({
  st_read("countries.shp")
  world_outline <- read_sf(dsn = ".", layer = "countries")
  st_crs(world_outline) = 4326
  
  regions <- ms_simplify(world_outline) %>% 
    select(NAME, geometry)
  
  
  
  
  
  ggplot(regions) + 
    geom_sf(aes(fill = "NAME"), 
            show.legend = FALSE) +
    theme_classic()
})


