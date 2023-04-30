ecologicalFootprintUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"))
  )
}

ecologicalFootprintServer <- function(id, selected_country) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlotly(
        (df_footprint %>% 
           ggplot(aes(x = country, 
                      y = earths_required,
                      text = paste0(
                        "Country: ", country,
                        "\nEarths Required: ", earths_required
                      ))) +
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
           theme(legend.position = "none",
                 axis.line.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank())) %>% 
          ggplotly(tooltip = "text")
      )
    }
  )
}