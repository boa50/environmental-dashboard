library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)
library(leaflet)
library(htmltools)

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
df_map$details <- sprintf(
  "<strong>%s</strong><br/>Produced: %g kw/h",
  df_map$country_match, df_map$value
) %>% lapply(htmltools::HTML)

ui <- fluidPage(
    titlePanel("Envronmental Dashboard"),
    fluidRow(
      column(
        3, 
        selectInput("selected_country", 
                    "Country", 
                    c("None", unique(df$country)))
      )
    ),
    fluidRow(
      column(
        6,
        plotlyOutput("line_plot")
      ),
      column(
        6,
        leafletOutput("map_plot")
      )
    )
)

server <- function(input, output) {
  colours_palette <- colorNumeric("Greens", 
                                  map_countries$value, 
                                  na.color = "transparent")
  
  output$line_plot <- renderPlotly(
    (df %>% 
       filter(country != input$selected_country) %>% 
       ggplot(aes(x = year, y = solar_electricity, group = country)) +
       {
         if (input$selected_country == "None") {
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
    leaflet(data = map_countries,
            options = leafletOptions(minZoom = 1.45, maxZoom = 18, 
                                     scrollWheelZoom = FALSE)) %>% 
      addPolygons(color = "grey",
                  weight = 1,
                  fillColor = ~colours_palette(value),
                  highlightOptions = highlightOptions(color = "black",
                                                      weight = 1.5, 
                                                      bringToFront = TRUE),
                  label = map_countries$details,
                  labelOptions = labelOptions(
                    interactive = TRUE,
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLegend("bottomright",
                pal = colours_palette,
                values = map_countries$value,
                title = "Energy produced",
                labFormat = labelFormat(suffix = "kw/h"),
                opacity = 1)
  )

}

shinyApp(ui = ui, server = server)
