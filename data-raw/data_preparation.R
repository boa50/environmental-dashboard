################### Code to create the used datasets ###########################
library(vroom)
library(dplyr)
library(janitor)
library(maps)
library(stringr)

df <- vroom("data-raw/World Energy Consumption.csv")

country_removed <- c("World", "Asia Pacific", "Europe", "North America",
                     "South & Central America", "Africa", "Oceania",
                     "OPEC", "Other Asia & Pacific", "Other CIS",
                     "Other Caribbean", "Other Middle East", "Other Northern Africa",
                     "Other South America", "Other Southern Africa",
                     "Europe (other)", "Eastern Africa", "Central America")

df %>% 
  # Removing 2020 year because of many NA values
  filter(year < 2020 & year >= 2010) %>% 
  filter(!country %in% country_removed) %>% 
  remove_empty("cols") %>%
  mutate(year = as.character(year),
         # Fixing some country names to match between datasets
         country = case_match(
           country,
           "Czechia" ~ "Czech Republic",
           "Timor" ~ "Timor-Leste",
           .default = country
         )) %>% 
  saveRDS("data/energy_consumption.rds")

### Map data
df_map_match <- df %>% 
  filter(year == 2019) %>%
  select(country, solar_electricity)

region_names <- map(plot = FALSE, namesonly = TRUE) 
map_countries <- map(fill = TRUE, 
                     plot = FALSE,
                     regions = region_names[-grep("Antarctica", region_names)]
)

# Fixing some country names to match between datasets
map_countries$names <- str_replace(map_countries$names, "USA", "United States")
map_countries$names <- str_replace(map_countries$names, "UK", "United Kingdom")
map_countries$names <- str_replace(map_countries$names, "Ivory Coast", "Cote d'Ivoire")
map_countries$names <- str_replace(map_countries$names, "Democratic Republic of the Congo", "Democratic Republic of Congo")
map_countries$names <- str_replace(map_countries$names, "Republic of Congo", "Congo")
map_countries$names <- str_replace(map_countries$names, "Swaziland", "Eswatini")
map_countries$names <- str_replace(map_countries$names, "Trinidad", "Trinidad and Tobago")
map_countries$names <- str_replace(map_countries$names, "Tobago", "Trinidad and Tobago")
map_countries$names <- str_replace(map_countries$names, "Saint Vincent", "Saint Vincent and the Grenadines")
map_countries$names <- str_replace(map_countries$names, "Grenadines", "Saint Vincent and the Grenadines")
map_countries$names <- str_replace(map_countries$names, "Antigua", "Antigua and Barbuda")
map_countries$names <- str_replace(map_countries$names, "Barbuda", "Antigua and Barbuda")

map_countries$country_match <- sapply(map_countries$names, function(name) {
  substring(name, 
            0, 
            ifelse(!is.na(str_locate(name, ":")[1]), 
                   str_locate(name, ":")[1] - 1,
                   10000))
})

match_pos <- match(map_countries$country_match, 
                   df_map_match$country)

map_countries$value <- unlist(df_map_match[match_pos, 2])

saveRDS(map_countries, "data/energy_consumption_map.rds")

rm(df, country_removed, map_countries, region_names, match_pos, df_map_match)