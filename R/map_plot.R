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
      highlight_opts <- list(colour = app_palette$map_polygon_highlight,
                             weight = 1.5)
      
      remove_highlights <- function(country_reset = TRUE) {
        leafletProxy("map_plot") %>% 
          removeShape(layerId = highlighted_layers())
        
        if(country_reset) {
          highlighted_layers(NULL)
          highlighted_country(all_countries)
        }
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
        colours_palette <- colorNumeric(get_map_colours(data_col),
                                        df_map[[data_col]], 
                                        na.color = "transparent")
        
        output$map_plot <- renderLeaflet(
          leaflet(data = df_map,
                  options = leafletOptions(minZoom = 1.30, 
                                           maxZoom = 18, 
                                           zoomControl = FALSE,
                                           doubleClickZoom = FALSE,
                                           scrollWheelZoom = FALSE)) %>% 
            onRender(
              "function(el, x) {
                L.control.zoom({ position:'topright' }).addTo(this);
              }"
            ) %>% 
            addPolygons(
              layerId = ~df_map$name,
              color = app_palette$map_polygon_border,
              weight = 1,
              fillColor = ~colours_palette(df_map[[data_col]]),
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(
                color = highlight_opts$colour,
                weight = highlight_opts$weight, 
                bringToFront = TRUE
              ),
              popup = sprintf(
                paste("<h6>%s</h6>",
                      "<div><strong>", get_energy_display_name(data_col), "</strong></div>",
                      "%.2f",
                      ifelse(get_data_suffix(data_col) == "%",
                             "%%",
                             get_data_suffix(data_col)),
                      "<div style='margin-top: 4px;'><strong>Total</strong></div>
                      <div class='map-popup-bars'>
                        <div class='bar renewable' style='width: %f%%'></div>
                        <div class='bar nonrenewable' style='width: %f%%'></div>
                      </div>
                      <div class='map-popup-bars map-popup-legend'>
                        <div style='display: flex; margin-right: 10px;'>
                          <div class='item renewable'></div>
                          <span>renewable</span>
                        </div>
                        <div style='display: flex'>
                          <div  class='item nonrenewable'></div>
                          <span>nonrenewable</span>
                        </div>
                      </div>
                      "),
                df_map$country_match, 
                df_map[[data_col]],
                (df_map$renewables * 100) / (df_map$renewables + df_map$nonrenewables),
                (df_map$nonrenewables * 100) / (df_map$renewables + df_map$nonrenewables))
              ) %>%
            addLegendNumeric(
              position = "bottomleft",
              pal = colours_palette,
              values = df_map[[data_col]],
              bins = 2,
              title = div(paste0("Energy Produced (",
                                 trimws(get_data_suffix(data_col)),
                                 ")"), 
                          style = "margin-bottom: 5px;"),
              orientation = "horizontal",
              width = 135,
              height = 12,
              numberFormat = function(x) signif(x, 1),
              tickLength = 0
            )
        )
        
        if (selected_country() != all_countries) {
          add_highlights(selected_country())
        }
      })
      
      # Update the selected country
      observeEvent(input$map_plot_shape_click, {
        event <- input$map_plot_shape_click
        
        if (!is.null(event$id)) {
          remove_highlights(country_reset = FALSE)
          add_highlights(latitude = event$lat, longitude = event$lng)
        }
      }, ignoreInit = TRUE)
      
      # Reset the country selection
      observeEvent(input$map_plot_click, 
                   remove_highlights(), 
                   ignoreInit = TRUE)
      
      # Applying the highlights when changing filter
      observeEvent(selected_country(), {
        if(selected_country() == all_countries) {
          remove_highlights()
        } else if (selected_country() != highlighted_country()) {
          remove_highlights(country_reset = FALSE)
          add_highlights(selected_country())
        }
      }, ignoreInit = TRUE)
      
      return(highlighted_country)
    }
  )
}