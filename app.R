library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(janitor)

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

df <- vroom::vroom("data-raw/World Energy Consumption.csv")
country_removed <- c("World", "Asia Pacific", "Europe", "North America",
                     "South & Central America", "Africa", "Oceania",
                     "OPEC", "Other Asia & Pacific", "Other CIS",
                     "Other Caribbean", "Other Middle East", "Other Northern Africa",
                     "Other South America", "Other Southern Africa",
                     "Europe (other)", "Eastern Africa", "Central America")

df_test <- df %>% 
  # Removing 2020 year because of many NA values
  filter(year < 2020 & year >= 2010) %>% 
  filter(!country %in% country_removed) %>% 
  remove_empty("cols") %>%
  mutate(year = as.character(year))

ui <- fluidPage(
    titlePanel("Envronmental Dashboard"),
    fluidRow(
      column(
        3, 
        selectInput("selected_country", 
                    "Country", 
                    c("None", unique(df_test$country)))
      )
    ),
    fluidRow(
      column(
        6,
        plotlyOutput("line_plot")
      )
    )
)

server <- function(input, output) {
  output$line_plot <- renderPlotly(
    (df_test %>% 
       filter(country != input$selected_country) %>% 
       ggplot(aes(x = year, y = solar_electricity, group = country)) +
       {
         if (input$selected_country == "None") {
           geom_line(colour = my_colours$axis)
         } else {
           list(
             geom_line(colour = "#d9d9d9"),
             geom_line(data = (df_test %>% filter(country == input$selected_country)),
                       colour = my_colours$line_main)
           )
         }
       } +
       theme(legend.position = "none")) %>% 
      ggplotly(tooltip = c("country", "solar_electricity"))
  )

}

shinyApp(ui = ui, server = server)
