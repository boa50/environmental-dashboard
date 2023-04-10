library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(janitor)
library(leaflet)
library(maps)
library(stringr)
library(shinycssloaders)
library(leaflegend)
library(shinyWidgets)
library(bslib)
library(htmlwidgets)
library(showtext)

### Setting charts themes
app_palette <- list(
  bg = "#e6eae9",
  fg = "#616161",
  primary = "#008B45",
  
  loader = "#7CCD7C",
  renewables = "#008B45",
  nonrenewables = "#8B4500",
  plot_background = alpha("#FFFFFF", 0.5), 
  
  axis = "#9e9e9e",
  line_default = "#9e9e9e",
  line_no_emphasis = "#d9d9d9",
  point_fill = "#FFFFFF",
  
  map_polygon_border = "#9C9C9C",
  map_polygon_highlight = "#2B2B2B"
)
app_palette <- append(
  app_palette,
  list(
    line_highlighted_renewables = app_palette$renewables,
    line_highlighted_nonrenewables = app_palette$nonrenewables,
    map_fill_renewables = colorRampPalette(
      c("transparent", "#00FF7F", app_palette$renewables), bias = 2
    ),
    map_fill_nonrenewables = colorRampPalette(
      c("transparent", "#BE5E00", app_palette$nonrenewables), bias = 2
    )
  )
)

theme_minimalistic <- function() {
  theme_classic() +
    theme(plot.title = element_text(hjust = 0, colour = app_palette$title),
          plot.title.position = "plot",
          axis.line = element_line(colour = app_palette$axis),
          axis.ticks = element_line(colour = app_palette$axis),
          axis.text = element_text(colour = app_palette$axis),
          axis.title = element_text(colour = app_palette$axis),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = app_palette$plot_background)
    )
}

### Setting default values
df <- readRDS("data/energy_data.rds")
df_map <- readRDS("data/energy_data_map.rds")
all_countries <- "All"
energies_available <- names(df)[!names(df) %in% c("country", "year")] %>% 
  str_replace_all(c("_percentage_demand" = "",
                    "_per_capita" = "")) %>% 
  unique() %>% 
  str_to_title()%>% 
  str_replace_all(c("Renewables" = "All Renewables",
                    "Nonrenewables" = "All Nonrenewables")) %>% 
  .[c(9, 1, 2, 3, 4, 10, 5, 6, 7, 8)]