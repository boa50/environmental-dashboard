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