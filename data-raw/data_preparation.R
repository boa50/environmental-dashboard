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
  mutate(year = as.character(year)) %>% 
  saveRDS("data/energy_consumption.rds")

### Map data
df_map_match <- df %>% 
  filter(year == 2019) %>%
  # Fixing some country names to match between dataframes
  mutate(country_match = case_match(
    country,
    "United States" ~ "USA",
    "United Kingdom" ~ "UK",
    "Czechia" ~ "Czech Republic",
    "Cote d'Ivoire" ~ "Ivory Coast",
    "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    "Congo" ~ "Republic of Congo",
    "Timor" ~ "Timor-Leste",
    "Eswatini" ~ "Swaziland",
    "Trinidad and Tobago" ~ "Trinidad",
    "Saint Vincent and the Grenadines" ~ "Saint Vincent",
    "Antigua and Barbuda" ~ "Antigua",
    .default = country
  )) %>% 
  select(country_match, solar_electricity)

region_names <- map(plot = FALSE, namesonly = TRUE) 
map_countries <- map(fill = TRUE, 
                     plot = FALSE,
                     regions = region_names[-grep("Antarctica", region_names)]
)

map_countries$country_match <- sapply(map_countries$names, function(name) {
  substring(name, 
            0, 
            ifelse(!is.na(str_locate(name, ":")[1]), 
                   str_locate(name, ":")[1] - 1,
                   10000))
})

match_pos <- match(map_countries$country_match, 
                   df_map_match$country_match)

map_countries$value <- unlist(df_map_match[match_pos, 2])

saveRDS(map_countries, "data/energy_consumption_map.rds")

rm(df, country_removed, map_countries, region_names, match_pos, df_map_match)