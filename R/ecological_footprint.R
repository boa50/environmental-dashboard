ecologicalFootprintUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotlyOutput(ns("plot"))
  )
}

ecologicalFootprintServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlotly(
        (df_footprint %>% 
           ggplot(aes(x = country, y = earths_required)) +
           geom_col() +
           theme(axis.line.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank())) %>% 
          ggplotly()
      )
    }
  )
}