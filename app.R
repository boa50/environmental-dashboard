library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)
library(gghighlight)

ui <- fluidPage(
    titlePanel("Envronmental Dashboard"),
    fluidRow(
      column(
        6,
        plotlyOutput("line_plot")
      )
    )
)


df <- vroom::vroom("data-raw/World Energy Consumption.csv")
continent_names <- c("World", "Asia Pacific", "Europe", "North America",
                     "South & Central America", "Africa", "Oceania")

df_test <- df %>% 
  # Removing 2020 year because of many NA values
  filter(year < 2020 & year >= 2010) %>% 
  filter(!country %in% continent_names) %>% 
  remove_empty("cols") %>%
  mutate(year = as.character(year))

server <- function(input, output) {
  
  output$line_plot <- renderPlotly(
    (df_test %>% 
       ggplot(aes(x = year, y = solar_electricity, group = country)) +
       geom_line(colour = my_colours$line_main) +
       theme(legend.position = "none") +
       gghighlight(country == "Japan", use_direct_label = FALSE)) %>% 
      ggplotly(tooltip = c("country", "solar_electricity"))
  )

}

shinyApp(ui = ui, server = server)
