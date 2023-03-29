################### Code to create the used datasets ###########################
library(vroom)
library(dplyr)
library(janitor)
library(maps)
library(stringr)

source("R/utils.R")


df <- vroom("data-raw/World Energy Consumption.csv")

country_removed <- c("World", "Asia Pacific", "Europe", "North America",
                     "South & Central America", "Africa", "Oceania",
                     "OPEC", "Other Asia & Pacific", "Other CIS", "CIS",
                     "Other Caribbean", "Other Middle East", "Other Northern Africa",
                     "Other South America", "Other Southern Africa",
                     "Europe (other)", "Eastern Africa", "Central America")

df <- df %>% 
  # Removing 2020 year because of many NA values
  filter(year < 2020 & year >= 2010) %>% 
  filter(!country %in% country_removed) %>% 
  remove_empty("cols") %>%
  mutate(year = as.character(year),
         # Creating new metrics
         solar = solar_electricity,
         solar_per_capita = (solar_electricity * 1e+09) / population,
         solar_percentage_consumption = (solar_electricity * 100) / primary_energy_consumption,
         # Fixing some country names to match between datasets
         country = case_match(
           country,
           "Czechia" ~ "Czech Republic",
           "Timor" ~ "Timor-Leste",
           .default = country
         )) %>% 
  select(country, year,
         solar, solar_per_capita, solar_percentage_consumption)
  
saveRDS(df, "data/energy_consumption.rds")


### Map data
df_map_match <- df %>% 
  filter(year == 2019) %>%
  select(-year)

region_names <- map(plot = FALSE, namesonly = TRUE) 
map_countries <- map(fill = TRUE, 
                     plot = FALSE,
                     regions = region_names[-grep("Antarctica", region_names)]
)

# Fixing some country names to match between datasets
map_countries$names <- map_countries$names %>% 
  get_country_from_region()

map_countries$country_match <- sapply(map_countries$names, function(name) {
  get_map_country_name(name)
})

match_pos <- match(map_countries$country_match, 
                   df_map_match$country)

value_columns <- names(df)[!names(df) %in% c("country", "year")]
map_countries[value_columns] <- 
  purrr::map(value_columns, ~ unlist(df_map_match[match_pos, .x]))

saveRDS(map_countries, "data/energy_consumption_map.rds")

rm(df, country_removed, map_countries, region_names, match_pos, df_map_match, value_columns)