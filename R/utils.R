### Some auxiliary functions
get_map_country_name <- function(map_name) {
  substring(map_name, 
            0, 
            ifelse(!is.na(str_locate(map_name, ":")[1]), 
                   str_locate(map_name, ":")[1] - 1,
                   10000))
}

get_country_from_region <- function(region) {
  str_replace_all(region,
                  c("^USA" = "United States",
                    "^UK" = "United Kingdom",
                    "^Ivory Coast" = "Cote d'Ivoire",
                    "^Republic of Congo" = "Congo",
                    "^Swaziland" = "Eswatini",
                    "^Trinidad" = "Trinidad and Tobago",
                    "^Tobago" = "Trinidad and Tobago",
                    "^Saint Vincent" = "Saint Vincent and the Grenadines",
                    "^Grenadines" = "Saint Vincent and the Grenadines",
                    "^Antigua" = "Antigua and Barbuda",
                    "^Barbuda" = "Antigua and Barbuda"))
}

get_region_from_country <- function(country) {
  str_replace_all(country,
                  c("^United States" = "USA",
                    "^United Kingdom" = "UK",
                    "^Cote d'Ivoire" = "Ivory Coast",
                    "^Congo" = "Republic of Congo",
                    "^Eswatini" = "Swaziland",
                    "^Trinidad and Tobago" = "Trinidad",
                    "^Saint Vincent and the Grenadines" = "Saint Vincent",
                    "^Antigua and Barbuda" = "Antigua"))
}

chart_title <- function(title, margin_bottom = TRUE) {
  tags$h6(title, 
          style = paste("font-weight:600;",
                        ifelse(!margin_bottom,"margin-bottom: -10px", "")))
}

get_plot_energy_title <- function(data_column) {
  paste(data_column, "produced", sep = "_") %>% 
    str_replace("_", "_energy_") %>% 
    ifelse(str_like(., "%per_capita%"), 
           paste(str_replace(., "per_capita", ""), "per_capita", sep = "_"),
           .) %>% 
    str_replace("_percentage_", "_%_of_") %>% 
    str_replace("renewables", "renewable") %>% 
    str_replace_all("_", " ") %>% 
    str_to_sentence()
}

get_energy_display_name <- function(data_column) {
  data_column %>% 
    str_split_1(pattern = "_") %>% 
    first() %>% 
    str_to_title()
}

get_line_plot_y_title <- function(data_column) {
  data_column %>% 
    get_plot_energy_title() %>% 
    str_to_title()
}

get_data_suffix <- function(data_column) {
  case_when(
    str_like(data_column, "%_per_capita") ~ " kWh",
    str_like(data_column, "%percentage%") ~ "%",
    .default = " TWh"
  )
}

is_renewable <- function(data_column) {
  renewables <- c("renewables", "biofuel", "hydro", "solar", "wind")
  str_to_lower(get_energy_display_name(data_column)) %in% renewables
}

get_line_colour <- function(data_column) {
  ifelse(is_renewable(data_column),
         app_palette$line_highlighted_renewables,
         app_palette$line_highlighted_nonrenewables)
}

get_map_colours <- function(data_column) {
  map_palette <- ifelse(is_renewable(data_column),
                        app_palette$map_fill_renewables,
                        app_palette$map_fill_nonrenewables)
  
  return(map_palette(7))
}