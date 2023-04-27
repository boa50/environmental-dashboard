ecologicalFootprintUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("plot"))
  )
}

ecologicalFootprintServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$plot <- renderPlot(
        df_footprint %>% 
          ggplot(aes(x = country, y = earths_required)) +
          geom_col()
      )
    }
  )
}