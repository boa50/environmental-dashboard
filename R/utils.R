### Some auxiliary functions
library(stringr)

get_map_country_name <- function(map_name) {
  substring(map_name, 
            0, 
            ifelse(!is.na(str_locate(map_name, ":")[1]), 
                   str_locate(map_name, ":")[1] - 1,
                   10000))
}

get_country_from_region <- function(region) {
  str_replace_all(region,
                  c("USA" = "United States",
                    "UK" = "United Kingdom",
                    "Ivory Coast" = "Cote d'Ivoire",
                    "Democratic Republic of the Congo" = "Democratic Republic of Congo",
                    "Republic of Congo" = "Congo",
                    "Swaziland" = "Eswatini",
                    "Trinidad" = "Trinidad and Tobago",
                    "Tobago" = "Trinidad and Tobago",
                    "Saint Vincent" = "Saint Vincent and the Grenadines",
                    "Grenadines" = "Saint Vincent and the Grenadines",
                    "Antigua" = "Antigua and Barbuda",
                    "Barbuda" = "Antigua and Barbuda"))
}

get_region_from_country <- function(country) {
  str_replace_all(country,
                  c("United States" = "USA",
                    "United Kingdom" = "UK",
                    "Cote d'Ivoire" = "Ivory Coast",
                    "Democratic Republic of Congo" = "Democratic Republic of the Congo",
                    "Congo" = "Republic of Congo",
                    "Eswatini" = "Swaziland",
                    "Trinidad and Tobago" = "Trinidad",
                    "Trinidad and Tobago" = "Tobago",
                    "Saint Vincent and the Grenadines" = "Saint Vincent",
                    "Antigua and Barbuda" = "Antigua"))
}

chart_title <- function(title, margin_bottom = TRUE) {
  tags$h4(title, 
          style = paste("font-weight:600;",
                        "color:", app_palette$chart_title, ";",
                        ifelse(!margin_bottom,"margin-bottom: -10px", "")))
}

get_plot_energy_title <- function(data_column) {
  paste(data_column, 
        ifelse(!str_like(data_column, "%percentage%"), "produced", ""), 
        sep = "_") %>% 
    str_replace("_", "_energy_") %>% 
    str_replace("_percentage_", "_%_of_") %>% 
    str_replace_all("_", " ") %>% 
    str_to_sentence()
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