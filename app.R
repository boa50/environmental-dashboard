theme_set(theme_minimalistic())
options(spinner.type = 7, 
        spinner.color = app_palette$loader, 
        spinner.hide.ui = FALSE)


select_box <- function(id, title, options) {
  if (typeof(options) == "list") {
    column(2, pickerInput(id, title, options))
  } else {
    column(2, selectInput(id, title, options))
  }
}

plot_area <- function(column_size, plot_element) {
  column(column_size, plot_element %>% withSpinner())
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  pageSpinner(type = 7, color = app_palette$loader),
  titlePanel("Envronmental Dashboard"),
  fluidRow(
    select_box("selected_country", 
               "Country", 
               c(all_countries, unique(df$country))),
    select_box("selected_energy",
               "Energy",
               list(Renewables = energies_available[1:5],
                    Nonrenewable = energies_available[6:10])),
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