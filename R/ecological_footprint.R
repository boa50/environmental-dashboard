ecologicalFootprintUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("chart_title")),
    plotlyOutput(ns("plot"))
  )
}

ecologicalFootprintServer <- function(id, selected_country) {
  df_plot <- df_footprint %>% 
    mutate(
      region = factor(
        region,
        levels = c("North America", "Latin America", "European Union",
                   "Northern/Eastern Europe", "Africa",
                   "Middle East/Central Asia", "Asia-Pacific")
      ),
      country = factor(
        country, 
        levels = country[order(region, -earths_required)]
      )
    ) %>%
    inner_join(
      (df_footprint %>% 
         group_by(region) %>% 
         summarise(region_earths_avg = mean(earths_required))),
      by = "region"
    )
  
  moduleServer(
    id,
    function(input, output, session) {
      output$chart_title <- renderUI(
        chart_title("How many earths do we need?")
      )
      
      output$plot <- renderPlotly(
        (df_plot %>% 
           ggplot(aes(x = country, 
                      y = earths_required,
                      fill = region,
                      text = paste0(
                        "Region: ", region,
                        "\nCountry: ", country,
                        "\nEarths Required: ", earths_required,
                        "\nEarths Required (region average): ", 
                        number_format()(region_earths_avg)
                      ))) +
           labs(x = "Country",
                y = "Earths Required") +
           {
             if (selected_country() == all_countries) {
               list(
                geom_col(),
                scale_fill_manual(values = app_palette$region)
               )
             } else {
               list(
                 geom_col(aes(
                   fill = ifelse(country == selected_country(),
                                 " highlighted",
                                 as.character(region))
                 )),
                 scale_fill_manual(values = c(
                   " highlighted" = app_palette$nonrenewables,
                   app_palette$region_no_emphasis
                 ))
               )
             }
           } +
           scale_y_continuous(limits = c(0, 10),
                              breaks = c(0, 1, 2.5, 5, 7.5, 10),
                              expand = expansion(mult = 0.01)) +
           theme(axis.line.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank())) %>% 
          ggplotly(tooltip = "text")
      )
    }
  )
}