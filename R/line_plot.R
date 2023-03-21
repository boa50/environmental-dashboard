linePlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("line_plot"))
  )
}

linePlotServer <- function(id, selected_country) {
  moduleServer(
    id,
    function(input, output, session) {
      output$line_plot <- renderPlotly(
        (df %>% 
           filter(country != selected_country()) %>% 
           ggplot(aes(x = year, y = solar_electricity, group = country)) +
           {
             if (selected_country() == all_countries) {
               geom_line(colour = my_colours$axis)
             } else {
               list(
                 geom_line(colour = "#d9d9d9"),
                 geom_line(data = (df %>% filter(country == selected_country())),
                           colour = my_colours$line_main)
               )
             }
           } +
           theme(legend.position = "none")) %>% 
          ggplotly(tooltip = c("country", "solar_electricity"))
      )
    }
  )
}