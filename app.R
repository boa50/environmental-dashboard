library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)
library(leaflet)
library(maps)


ui <- fluidPage(
    titlePanel("Envronmental Dashboard"),
    fluidRow(
      column(3, 
             selectInput("selected_country", 
                         "Country",
                         c(all_countries, unique(df$country))))
    ),
    fluidRow(
      column(4, linePlotUI("line_plot")),
      column(4, mapPlotUI("map_plot"))
    ),
    
)

server <- function(input, output, session) {
  linePlotServer("line_plot", reactive(input$selected_country))
  
  highlighted_country <- mapPlotServer("map_plot",
                                       reactive(input$selected_country))
  
  observeEvent(highlighted_country(), {
    updateSelectInput(session,
                      "selected_country",
                      selected = highlighted_country())
  })

}

shinyApp(ui = ui, server = server)