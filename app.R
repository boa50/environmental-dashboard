library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)
library(gghighlight)

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
  
  selected_country <- "Japan"
  
  output$line_plot <- renderPlotly(
    (df_test %>% 
        filter(country != selected_country) %>% 
        ggplot(aes(x = year, y = solar_electricity, group = country)) +
        geom_line(colour = "#d9d9d9") +
        geom_line(data = (df_test %>% filter(country == selected_country)),
                  colour = my_colours$line_main) +
        theme(legend.position = "none")) %>% 
      ggplotly(tooltip = c("country", "solar_electricity"))
  )

}

shinyApp(ui = ui, server = server)
