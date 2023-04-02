linePlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("chart_title")),
    plotlyOutput(ns("line_plot"))
  )
}

linePlotServer <- function(id, selected_country, data_column) {
  moduleServer(
    id,
    function(input, output, session) {
      output$chart_title <- renderUI({
        chart_title(paste(get_plot_energy_title(data_column()),
                          "over the last 10 years"),
                    margin_bottom = FALSE)
      })
      
      output$line_plot <- renderPlotly(
        (df %>% 
           filter(country != selected_country()) %>% 
           ggplot(aes(x = year, 
                      y = .data[[data_column()]], 
                      group = country,
                      text = paste(
                        "Country:", country, 
                        "\nYear:", year,
                        "\nProduced:", 
                        label_number(accuracy = 0.01)(.data[[data_column()]]), 
                        trimws(get_data_suffix(data_column()))
                      ))) +
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
           labs(x = "Year", y = get_line_plot_y_title(data_column())) +
           scale_x_discrete(
             breaks = c(2010, 2019),
             expand = expansion(mult = c(.02, .02))
           ) +
           scale_y_continuous(
             labels = label_number(suffix = get_data_suffix(data_column())),
             expand = expansion(mult = c(.02, .02))
           ) +
           theme(legend.position = "none")) %>% 
          ggplotly(tooltip = "text")
      )
    }
  )
}