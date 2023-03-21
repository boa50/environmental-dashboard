library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)
library(leaflet)
library(maps)

my_colours <- list(
  title = "#616161",
  axis = "#9e9e9e",
  main = "#1976d2",
  no_emphasis = "#757575",
  divergent = "#f57c00",
  line_main = "#42a5f5",
  line_complementary = "#78909c"
)

theme_minimalistic <- function() {
  theme_classic() +
    theme(plot.title = element_text(hjust = 0, colour = my_colours$title),
          plot.title.position = "plot",
          axis.line = element_line(colour = my_colours$axis),
          axis.ticks = element_line(colour = my_colours$axis),
          axis.text = element_text(colour = my_colours$axis),
          axis.title = element_text(colour = my_colours$axis),
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA)
    )
}

theme_set(theme_minimalistic())

df <- readRDS("data/energy_consumption.rds")
df_map <- readRDS("data/energy_consumption_map.rds")
all_countries <- "All"

ui <- fluidPage(
    titlePanel("Envronmental Dashboard"),
    fluidRow(
      column(
        3, 
        selectInput("selected_country", 
                    "Country", 
                    c(all_countries, unique(df$country)))
      )
    ),
    fluidRow(
      column(4, linePlotUI("line_plot")),
      column(1, verbatimTextOutput("text_test")),
      column(4, mapPlotUI("map_plot"))
    ),
    
)

server <- function(input, output, session) {
  linePlotServer("line_plot", 
                 df, 
                 reactive(input$selected_country), 
                 all_countries, 
                 my_colours)
  
  highlighted_country <- mapPlotServer("map_plot",
                                       df_map,
                                       reactive(input$selected_country), 
                                       all_countries)
  
  observeEvent(highlighted_country(), {
    updateSelectInput(session,
                      "selected_country",
                      selected = highlighted_country())
  })

}

shinyApp(ui = ui, server = server)
