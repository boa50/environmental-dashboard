theme_set(theme_minimalistic())
options(spinner.type = 7, 
        spinner.color = app_palette$loader,
        spinner.hide.ui = FALSE)
app_theme <- bs_theme(
  version = 5,
  bg = app_palette$bg, 
  fg = app_palette$fg, 
  primary = app_palette$primary,
  secondary = app_palette$bg, 
  base_font = font_google("Roboto"),
  "controls-border-colour" = app_palette$fg
)
app_theme <- bs_add_rules(
  app_theme,
  c(
    ".bootstrap-select button.btn.dropdown-toggle:focus { 
      outline: 0 !important;
      border-color: #80c5a2;
      box-shadow: 0 0 0 0.25rem rgba(0, 139, 69, 0.25);
    }",
    ".bootstrap-select button.btn.dropdown-toggle {
      border: 1px solid $controls-border-colour;
    }"
  )
)

select_box <- function(id, title, choices) {
  if (typeof(choices) == "list") {
    column(3, pickerInput(id, 
                          title,
                          choices = choices,
                          options = list(
                            style = "btn-secondary"
                          )))
  } else {
    column(3, pickerInput(id, 
                          title,
                          choices = choices,
                          options = list(
                            style = "btn-secondary",
                            `live-search` = TRUE
                          )))
  }
}

plot_area <- function(column_size, plot_element) {
  column(column_size, plot_element %>% withSpinner())
}

ui <- fluidPage(
  theme = app_theme,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),
  pageSpinner(type = 7, 
              color = app_palette$loader, 
              background = app_palette$bg),
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
               c("Total", "Per Capita", "% of Demand")),
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