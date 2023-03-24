linePlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Energy produced over the last 10 years"),
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
           labs(x = "Year", y = "Solar Energy Generated") +
           scale_x_discrete(breaks = c(2010, 2019),
                            expand = expansion(mult = c(.02, .02))) +
           scale_y_continuous(labels = label_number(suffix = " TWh"),
                              expand = expansion(mult = c(.02, .02))) +
           theme(legend.position = "none")) %>% 
          ggplotly(tooltip = c("country", "solar_electricity"))
      )
    }
  )
}