output$bargraph <- renderPlot({
  ggplot(data = bio, 
         aes_string(x = species_type, y = thr_count, fill=input$z)) +
    geom_bar(stat='identity', position='dodge') +
    geom_text(label = bio$Country) +
    labs(x = "Number of Threatened Species", y = "Count") +
    theme(panel.grid.major = element_line(color = gray(0.5), linetype = "blank",size = 0.5), panel.background = element_rect(fill = "white"))+
    theme_classic()+
    xlab("Species Type")+
    ylab("Count of Threatened Species")+
    theme(axis.title = element_text(face="bold"), title = element_text(face="bold")))  