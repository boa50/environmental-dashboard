################### Code to create the used datasets ###########################
library(vroom)
library(dplyr)
library(janitor)
library(maps)
library(stringr)

source("R/utils.R")

### ENERGY CONSUMPTION
df <- vroom("data-raw/owid-energy-data.csv")

df <- df %>% 
  # Removing 2020 and above years because of many NA values
  filter(year <= 2019 & year >= 2010) %>% 
  filter(!is.na(iso_code)) %>% 
  remove_empty("cols") %>%
  mutate(year = as.character(year),
         # Fixing some country names to match between datasets
         country = case_match(
           country,
           "Czechia" ~ "Czech Republic",
           "Timor" ~ "Timor-Leste",
           "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
           .default = country
         )) 

# Creating energy metrics
energy_names <- c("biofuel", "hydro", "solar", "wind",
                  "coal", "gas", "nuclear", "oil")
result_columns <- c("country", "year")

for (energy in energy_names) {
  per_capita <- paste(energy, "per_capita", sep = "_")
  percentage_demand <- paste(energy, "percentage_demand", sep = "_")
  
  df <- mutate(
    df,
    {{energy}} := !!as.name(paste(energy, "electricity", sep = "_")),
    {{per_capita}} := (!!as.name(energy) * 1e+09) / population,
    {{percentage_demand}} := (!!as.name(energy) * 100) / electricity_demand
  )
  
  result_columns <- append(result_columns, 
                           c(energy, per_capita, percentage_demand))
}

df <- mutate(
  df,
  renewables = purrr::pmap_dbl(list(biofuel, hydro, solar, wind), sum, na.rm = TRUE),
  renewables_per_capita = (renewables * 1e+09) / population,
  renewables_percentage_demand = (renewables * 100) / electricity_demand,
  nonrenewables = purrr::pmap_dbl(list(coal, gas, nuclear, oil), sum, na.rm = TRUE),
  nonrenewables_per_capita = (nonrenewables * 1e+09) / population,
  nonrenewables_percentage_demand = (nonrenewables * 100) / electricity_demand
)

result_columns <- append(result_columns, 
                         c("renewables", 
                           "renewables_per_capita", 
                           "renewables_percentage_demand",
                           "nonrenewables", 
                           "nonrenewables_per_capita", 
                           "nonrenewables_percentage_demand"))

df <- select(df, all_of(result_columns))
  
saveRDS(df, "data/energy_data.rds")


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

saveRDS(map_countries, "data/energy_data_map.rds")

rm(df, map_countries, region_names, match_pos, df_map_match, value_columns,
   energy_names, result_columns, energy, per_capita, percentage_demand)


### ECOLOGICAL FOOTPRINT
df <- vroom("data-raw/ecological-footprint.csv") %>% 
  clean_names() %>% 
  select(country, earths_required, total_ecological_footprint) %>% 
  rename(ecological_footprint = total_ecological_footprint)

saveRDS(df, "data/ecological_footprint.rds")
  
rm(df)