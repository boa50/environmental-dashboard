linePlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    chart_title("Energy generated over the last 10 years", 
                margin_bottom = FALSE),
    plotlyOutput(ns("line_plot"))
  )
}

linePlotServer <- function(id, selected_country, selected_energy) {
  moduleServer(
    id,
    function(input, output, session) {
      output$line_plot <- renderPlotly(
        (df %>% 
           filter(country != selected_country()) %>% 
           ggplot(aes(x = year, y = .data[[selected_energy()]], group = country)) +
           {
             if (selected_country() == all_countries) {
               geom_line(colour = app_palette$line_default)
             } else {
               list(
                 geom_line(colour = app_palette$line_no_emphasis),
                 geom_line(data = (df %>% filter(country == selected_country())),
                           colour = app_palette$line_highlighted)
               )
             }
           } +
           labs(x = "Year", y = "Solar Energy Generated") +
           scale_x_discrete(breaks = c(2010, 2019),
                            expand = expansion(mult = c(.02, .02))) +
           scale_y_continuous(labels = label_number(suffix = " TWh"),
                              expand = expansion(mult = c(.02, .02))) +
           theme(legend.position = "none")) %>% 
          ggplotly(tooltip = c("country", selected_energy()))
      )
    }
  )
}