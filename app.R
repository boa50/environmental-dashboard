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
library(leaflegend)

theme_set(theme_minimalistic())
options(spinner.type = 7, 
        spinner.color = app_palette$loader, 
        spinner.hide.ui = FALSE)

select_box <- function(id, title, options) {
  column(2, selectInput(id, title, options))
}

plot_area <- function(column_size, plot_element) {
  column(column_size, plot_element %>% withSpinner())
}

ui <- fluidPage(
  pageSpinner(type = 7, color = app_palette$loader),
  titlePanel("Envronmental Dashboard"),
  fluidRow(
    select_box("selected_country", 
               "Country", 
               c(all_countries, unique(df$country))),
    select_box("selected_energy", 
               "Energy", 
               energies_available),
    select_box("selected_metric", 
               "Metric", 
               c("Total", "Per Capita", "% of Consumption")),
  ),
  fluidRow(
    plot_area(6, linePlotUI("line_plot")),
    plot_area(6, mapPlotUI("map_plot"))
  )
)

server <- function(input, output, session) {
  showPageSpinner()
  
  data_column <- reactive(
    paste(input$selected_energy, 
          input$selected_metric) %>% 
      str_replace_all(c(" Total" = "", 
                        "All " = "",
                        " % of " = " percentage ", 
                        " " = "_")) %>% 
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