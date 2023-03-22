mapPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Energy produced in 2019"),
    leafletOutput(ns("map_plot"))
  )
}

mapPlotServer <- function(id, selected_country) {
  moduleServer(
    id,
    function(input, output, session) {
      highlighted_country <- reactiveVal(all_countries)
      highlighted_layers <- reactiveVal(NULL)
      colours_palette <- colorNumeric("Greens", 
                                      df_map$value, 
                                      na.color = "transparent")
      highlight_opts <- list(colour = "black", weight = 1.5)
      
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