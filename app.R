library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)
library(leaflet)
library(maps)
library(shinycssloaders)

theme_set(theme_minimalistic())

ui <- fluidPage(
  pageSpinner(type = 1, color = "#0dc5c1"),
  titlePanel("Envronmental Dashboard"),
  fluidRow(
    column(3, 
           selectInput("selected_country", 
                       "Country",
                       c(all_countries, unique(df$country))))
  ),
  fluidRow(
    column(6, linePlotUI("line_plot")),
    column(6, mapPlotUI("map_plot"))
  )
)

server <- function(input, output, session) {
  showPageSpinner()
  linePlotServer("line_plot", reactive(input$selected_country))
  
  highlighted_country <- mapPlotServer("map_plot",
                                       reactive(input$selected_country))
  
  highlighted_country <- mapPlotServer("map_plot",
                                       reactive(input$selected_country))
  
  observeEvent(highlighted_country(), {
    updateSelectInput(session,
                      "selected_country",
                      selected = highlighted_country())
    
    hidePageSpinner()
  })
}

shinyApp(ui = ui, server = server)