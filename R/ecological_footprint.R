ecologicalFootprintUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("chart_title")),
    plotlyOutput(ns("plot"))
  )
}

ecologicalFootprintServer <- function(id, selected_country) {
  moduleServer(
    id,
    function(input, output, session) {
      output$chart_title <- renderUI(
        chart_title("How many earths do we need?")
      )
      
      output$plot <- renderPlotly(
        (df_footprint %>% 
           mutate(
             country = factor(
               country, 
               levels = country[order(earths_required, decreasing = TRUE)]
             )
           ) %>% 
           ggplot(aes(x = country, 
                      y = earths_required,
                      text = paste0(
                        "Country: ", country,
                        "\nEarths Required: ", earths_required
                      ))) +
           labs(x = "Country",
                y = "Earths Required") +
           {
             if (selected_country() == all_countries) {
               geom_col(fill = app_palette$line_default)
             } else {
               list(
                 geom_col(aes(
                   fill = factor(ifelse(country == selected_country(),
                                        "highlighted",
                                        "no_emphasis"))
                 )),
                 scale_fill_manual(values = c(app_palette$nonrenewables,
                                              app_palette$line_no_emphasis))
               )
             }
           } +
           scale_y_continuous(limits = c(0, 10),
                              expand = expansion(mult = 0.01)) +
           theme(legend.position = "none",
                 axis.line.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank())) %>% 
          ggplotly(tooltip = "text")
      )
    }
  )
}