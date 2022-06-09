library(shiny)

server <- function(input, output){
  output$anime_trend <- renderPlotly({
    anime_trend <- unique_anime %>% 
      filter(prem_year < 2019) %>% 
      filter(!is.na(prem_year)) %>%
      group_by(year = prem_year) %>% 
      summarise(freq = n())%>% 
      ungroup() %>% 
      mutate(label = glue("Year: {year}
                      Total: {freq} anime"))
    
    plot1 <- ggplot(anime_trend, mapping = aes(x = year, y = freq)) +
      geom_line() +
      geom_point(col = "navy",aes(text = label)) +
      labs(title = 'Anime Production per Year',
           x = "Premiered Year",
           y = NULL) +
      theme_minimal() +
      theme(legend.position = 'none') + theme(plot.title = element_text(face = "bold", hjust = 0.5),
                                              plot.subtitle = element_text(hjust = 0.5))
    
    ggplotly(plot1, tooltip = "text")
  })
  
  output$plot_genre <- renderPlotly({
    anime_genre <- unique_anime %>% 
      filter(genre == input$category) %>% 
      group_by(name) %>% 
      summarise(avg_scored_by = mean(scored_by)) %>% 
      ungroup() %>% 
      arrange(desc(avg_scored_by))
    anime_genre <- head(anime_genre, 15) %>% 
      mutate(label = glue("{name}
                      Average Rating: {comma(avg_scored_by)}"))
    
    plot2 <- ggplot(anime_genre, aes(x = avg_scored_by, y = reorder(name, avg_scored_by), text = label)) +
      geom_col(aes(fill = avg_scored_by)) +
      labs(title = paste("Top 15", input$category, "Anime"),
           x = "Average Score",
           y = NULL) +
      scale_fill_gradient(low = "cornflowerblue", high= "navy") +
      scale_x_continuous(labels = comma) +
      theme_minimal() +
      theme(legend.position = 'none') + theme(plot.title = element_text(face = "bold", hjust = 0.5),
                                              plot.subtitle = element_text(hjust = 0.5))
    scale_x_continuous(labels = label_number(suffix = " M", scale = 1e-6))
    
    ggplotly(plot2, tooltip = "text")
  })
  
  output$plot_stud <- renderPlotly({
    anime_season <- unique_anime %>% filter(prem_season != 'NA')
      filter(prem_season == input$season) %>% 
      group_by(genre) %>% 
      summarise(fav = sum(favorites)) %>% 
      ungroup() %>% 
      arrange(desc(fav))
    anime_season <- head(anime_season, 5) %>% 
      mutate(label = glue("{genre}
                      Total Favorites: {comma(fav)}"))
    
    plot3 <- ggplot(anime_season, aes(x = fav, y = reorder(genre, fav), text = label)) +
      geom_col(aes(fill = 'blue')) +
      labs(title = paste("Top 5", input$season, "Anime Genres"),
           x = "Total of Favorites",
           y = NULL) +
      scale_x_continuous(labels = comma) +
      theme_minimal() +
      theme(legend.position = 'none') + theme(plot.title = element_text(face = "bold", hjust = 0.5),
                                              plot.subtitle = element_text(hjust = 0.5))
    
    ggplotly(plot3, tooltip = "text")
  })
  
  
}
