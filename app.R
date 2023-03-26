library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(janitor)
library(leaflet)
library(maps)
library(stringr)
library(shinycssloaders)

theme_set(theme_minimalistic())

select_box <- function(id, title, options) {
  column(2, selectInput(id, title, options))
}

ui <- fluidPage(
  pageSpinner(type = 1, color = app_palette$loader),
  titlePanel("Envronmental Dashboard"),
  fluidRow(
    select_box("selected_country", 
               "Country", 
               c(all_countries, unique(df$country))),
    select_box("selected_energy", 
               "Energy", 
               energies_available),
    select_box("selected_prod_cons", 
               "Produced / % of Consumption", 
               c("Produced", "Percentage of Consumption")),
    select_box("selected_total_capita", 
               "Total / Per Capita", 
               c("Total", "Per Capita")),
  ),
  fluidRow(
    column(6, linePlotUI("line_plot")),
    column(6, mapPlotUI("map_plot"))
  )
)

server <- function(input, output, session) {
  showPageSpinner()
  
  data_column <- reactive(
    paste(input$selected_energy, 
          input$selected_prod_cons, 
          input$selected_total_capita) %>% 
      str_replace_all(c(" Total" = "", " " = "_")) %>% 
      str_to_lower()
  )
  
  linePlotServer("line_plot", 
                 reactive(input$selected_country),
                 data_column)
  
  highlighted_country <- mapPlotServer("map_plot",
                                       reactive(input$selected_country),
                                       data_column)
  
  observeEvent(highlighted_country(), {
    updateSelectInput(session,
                      "selected_country",
                      selected = highlighted_country())
    
    hidePageSpinner()
  })
}

shinyApp(ui = ui, server = server)