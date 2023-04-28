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
  heading_font = font_google("Rubik", wght = 500),
  "controls-border-colour" = app_palette$filter_border,
  "renewables-colour" = app_palette$renewables,
  "nonrenewables-colour" = app_palette$nonrenewables,
  "plot-background" = app_palette$plot_background,
  "plot-border-radius" = "4px"
)
app_theme <- app_theme %>% 
  bs_add_rules(sass::sass_file("www/plot.scss")) %>% 
  bs_add_rules(sass::sass_file("www/filter.scss")) %>% 
  bs_add_rules(sass::sass_file("www/map.scss"))

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
                          options = pickerOptions(
                            style = "btn-secondary",
                            liveSearch = length(choices) > 25,
                            size = 10
                          )))
  }
}

plot_area <- function(column_size, plot_element) {
  column(column_size, plot_element %>% withSpinner())
}

ui <- fluidPage(
  theme = app_theme,
  pageSpinner(type = 7, 
              color = app_palette$loader, 
              background = app_palette$bg),
  titlePanel(h1("Energy Production",
                align = "center",
                style = "color: #5E716A;"),
             windowTitle = "Energy Production"),
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
  ),
  fluidRow(
    plot_area(12, ecologicalFootprintUI("footprint_plot"))
  )
)

server <- function(input, output, session) {
  showPageSpinner()
  showtext_auto()
  
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

  ecologicalFootprintServer("footprint_plot")
  
  observeEvent(highlighted_country(), {
    updateSelectInput(session,
                      "selected_country",
                      selected = highlighted_country())

    hidePageSpinner()
  })
}

shinyApp(ui = ui, server = server)