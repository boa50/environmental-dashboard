library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)
library(leaflet)
library(maps)

source("R/utils.R")

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
      column(
        4,
        plotlyOutput("line_plot")
      ),
      column(
        4,
        leafletOutput("map_plot")
      )
    ),
    
)

server <- function(input, output, session) {
  colours_palette <- colorNumeric("Greens", 
                                  df_map$value, 
                                  na.color = "transparent")
  highlight_opts <- list(
    colour = "black",
    weight = 1.5
  )
  
  output$line_plot <- renderPlotly(
    (df %>% 
       filter(country != input$selected_country) %>% 
       ggplot(aes(x = year, y = solar_electricity, group = country)) +
       {
         if (input$selected_country == all_countries) {
           geom_line(colour = my_colours$axis)
         } else {
           list(
             geom_line(colour = "#d9d9d9"),
             geom_line(data = (df %>% filter(country == input$selected_country)),
                       colour = my_colours$line_main)
           )
         }
       } +
       theme(legend.position = "none")) %>% 
      ggplotly(tooltip = c("country", "solar_electricity"))
  )
  
  output$map_plot <- renderLeaflet(
    leaflet(data = df_map,
            options = leafletOptions(minZoom = 1.45, 
                                     maxZoom = 18, 
                                     doubleClickZoom = FALSE,
                                     scrollWheelZoom = FALSE)) %>% 
      addPolygons(layerId = ~df_map$name,
                  color = "grey",
                  weight = 1,
                  fillColor = ~colours_palette(value),
                  highlightOptions = highlightOptions(color = highlight_opts$colour,
                                                      weight = highlight_opts$weight, 
                                                      bringToFront = TRUE),
                  popup = sprintf(
                    "<h4>%s</h4>
                    Produced: %.2f TW",
                    df_map$country_match, df_map$value)) %>%
      addLegend("bottomright",
                pal = colours_palette,
                values = df_map$value,
                title = "Energy produced",
                labFormat = labelFormat(suffix = "kw/h"),
                opacity = 1)
  )
  
  remove_highlights <- function() {
    leafletProxy("map_plot") %>% 
      clearPopups() %>% 
      removeShape(layerId = session$userData$highlight_layers)
    
    session$userData$highlight_layers <- NULL
  }
  
  # Update the selected country
  observeEvent(input$map_plot_shape_click, {
    event <- input$map_plot_shape_click
    country <- event$id
    
    if (is.null(country)) {
      country <- all_countries
    } else {
      remove_highlights()
      
      selected_region <- map.where(x = event$lng, y = event$lat)
      highlight_region <- map(regions = selected_region,
                              fill = TRUE,
                              plot = FALSE)
      session$userData$highlight_layers <- paste(highlight_region$names,
                                                 "Selected")
      
      leafletProxy("map_plot") %>% 
        addPolygons(data = highlight_region,
                    fillColor = "transparent",
                    color = highlight_opts$colour,
                    weight = highlight_opts$weight,
                    layerId = session$userData$highlight_layers)
    }
    
    
    country <- get_map_country_name(country)
    updateSelectInput(session, "selected_country", selected = country)
  }, ignoreInit = TRUE)
  
  # Reset the country selection
  observeEvent(input$map_plot_click, {
    updateSelectInput(session, "selected_country", selected = all_countries)
  }, ignoreInit = TRUE)
  
  # Clearing the highlights when changing filter
  observeEvent(input$selected_country, {
    if (input$selected_country == all_countries) remove_highlights()
  }, ignoreInit = TRUE)

}

shinyApp(ui = ui, server = server)
