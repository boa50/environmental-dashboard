mapPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("text_test")),
    leafletOutput(ns("map_plot"))
  )
}

mapPlotServer <- function(id, df_map, selected_country, all_countries) {
  moduleServer(
    id,
    function(input, output, session) {
      # highlighted_country <- "All"
      session$userData$highlighted_country <- all_countries
      colours_palette <- colorNumeric("Greens", 
                                      df_map$value, 
                                      na.color = "transparent")
      highlight_opts <- list(
        colour = "black",
        weight = 1.5
      )
      
      
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
      
      remove_highlights <- function() {
        leafletProxy("map_plot") %>% 
          clearPopups() %>% 
          removeShape(layerId = session$userData$highlight_layers)
        
        session$userData$highlight_layers <- NULL
        # session$userData$highlighted_country <- all_countries
      }
      
      # Update the selected country
      observeEvent(input$map_plot_shape_click, {
        event <- input$map_plot_shape_click
        country <- event$id
        
        if (is.null(country)) {
          country <- all_countries
        } else {
          remove_highlights()
          
          selected_region <- map.where(x = event$lng, y = event$lat)
          highlight_region <- map(regions = selected_region,
                                  fill = TRUE,
                                  plot = FALSE)
          session$userData$highlight_layers <- paste(highlight_region$names,
                                                     "Selected")
          
          leafletProxy("map_plot") %>% 
            addPolygons(data = highlight_region,
                        fillColor = "transparent",
                        color = highlight_opts$colour,
                        weight = highlight_opts$weight,
                        layerId = session$userData$highlight_layers)
          
          
          country <- get_map_country_name(country)
          session$userData$highlighted_country <- country
        }
        
        output$text_test <- renderText(paste("Mod:",session$userData$highlighted_country))
        return(reactive(session$userData$highlighted_country))
        
        # output$text_test <- renderText(paste("Mod:",session$userData$highlighted_country))
        
        # updateSelectInput(session, "selected_country", selected = country)
        # highlighted_country <<- country
        # return(reactive(country))
      }, ignoreInit = TRUE)
      
      # Reset the country selection
      observeEvent(input$map_plot_click, {
        # updateSelectInput(session, "selected_country", selected = all_countries)
        # highlighted_country <- "All"
        # return(reactive(all_countries))
        # session$userData$highlighted_country <- all_countries
        remove_highlights()
      }, ignoreInit = TRUE)
      
      # Clearing the highlights when changing filter
      # observeEvent(selected_country(), {
      #   if (selected_country() == all_countries) remove_highlights()
      # }, ignoreInit = TRUE)
      
      
      # output$text_test <- renderText(paste("Mod:",session$userData$highlighted_country))
      # output$text_test <- renderText(session$userData$highlighted_country)
      # output$text_test <- renderText(highlighted_country)
      # return(reactive(highlighted_country))
      # return(reactive(session$userData$highlighted_country))
      # observeEvent(session$userData$highlighted_country, {
      #   output$text_test <- renderText(paste("Mod:",session$userData$highlighted_country))
      #   return(reactive(session$userData$highlighted_country))
      # })
      # return(reactive("test"))
    }
  )
}