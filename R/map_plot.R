mapPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("chart_title")),
    leafletOutput(ns("map_plot"))
  )
}

mapPlotServer <- function(id, selected_country, data_column) {
  moduleServer(
    id,
    function(input, output, session) {
      output$chart_title <- renderUI({
        chart_title(paste(get_plot_energy_title(data_column()), "in 2019"))
      })
      
      highlighted_country <- reactiveVal(all_countries)
      highlighted_layers <- reactiveVal(NULL)
      data_col <- "solar_produced"
      highlight_opts <- list(colour = app_palette$map_polygon_highlight,
                             weight = 1.5)
      
      remove_highlights <- function() {
        leafletProxy("map_plot") %>% 
          clearPopups() %>% 
          removeShape(layerId = highlighted_layers())
        
        highlighted_layers(NULL)
        highlighted_country(all_countries)
      }
      
      add_highlights <- function(country = NULL, 
                                 latitude = NULL,
                                 longitude = NULL) {
        
        if (is.null(country)) {
          selected_region <- map.where(x = longitude, y = latitude)
          country <- get_country_from_region(selected_region)
          highlighted_country(get_map_country_name(country))
        } else {
          selected_region <- get_region_from_country(country)
          highlighted_country(country)
        }
        
        highlight_region <- map(regions = selected_region,
                                fill = TRUE,
                                plot = FALSE)
        highlighted_layers(paste(highlight_region$names, "Selected"))
        
        leafletProxy("map_plot") %>% 
          addPolygons(data = highlight_region,
                      fillColor = "transparent",
                      color = highlight_opts$colour,
                      weight = highlight_opts$weight,
                      layerId = highlighted_layers())
      }
      
      observeEvent(data_column(), {
        data_col <- data_column()
        colours_palette <- colorNumeric(app_palette$map_fill,
                                        df_map[[data_col]], 
                                        na.color = "transparent")
        
        output$map_plot <- renderLeaflet(
          leaflet(data = df_map,
                  options = leafletOptions(minZoom = 1.30, 
                                           maxZoom = 18, 
                                           doubleClickZoom = FALSE,
                                           scrollWheelZoom = FALSE)) %>% 
            addPolygons(
              layerId = ~df_map$name,
              color = app_palette$map_polygon_border,
              weight = 1,
              fillColor = ~colours_palette(df_map[[data_col]]),
              highlightOptions = highlightOptions(
                color = highlight_opts$colour,
                weight = highlight_opts$weight, 
                bringToFront = TRUE
              ),
              popup = sprintf(
                paste("<h4>%s</h4>
                      Produced: %.2f", get_data_suffix(data_col)),
                df_map$country_match, df_map[[data_col]])) %>%
            addLegendNumeric(
              position = "topright",
              pal = colours_palette,
              values = df_map[[data_col]],
              bins = 2,
              title = div("Energy Produced", 
                          style = "margin-bottom: 5px;"),
              orientation = "horizontal",
              width = 75,
              height = 12,
              numberFormat = function(x) {
                paste(signif(x, 1), get_data_suffix(data_col), sep = "")
              }
            )
        )
      })
      
      # Update the selected country
      observeEvent(input$map_plot_shape_click, {
        event <- input$map_plot_shape_click
        
        if (!is.null(event$id)) {
          remove_highlights()
          add_highlights(latitude = event$lat, longitude = event$lng)
        }
      }, ignoreInit = TRUE)
      
      # Reset the country selection
      observeEvent(input$map_plot_click, 
                   remove_highlights(), 
                   ignoreInit = TRUE)
      
      # Applying the highlights when changing filter
      observeEvent(selected_country(), {
        remove_highlights()
        
        if (selected_country() != all_countries) {
          add_highlights(selected_country())
        }
      }, ignoreInit = TRUE)
      
      return(highlighted_country)
    }
  )
}