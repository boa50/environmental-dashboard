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

df <- readRDS("data/energy_consumption.rds")

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
      )
    )
)

server <- function(input, output) {
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

}

shinyApp(ui = ui, server = server)
