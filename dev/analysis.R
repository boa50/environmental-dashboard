library(dplyr)
library(ggplot2)
# library(gganimate)
library(plotly)
library(janitor)
library(stringr)
library(scales)

source("R/utils.R")

my_colours <- list(
  title = "#616161",
  axis = "#9e9e9e",
  main = "#1976d2",
  no_emphasis = "#757575",
  divergent = "#f57c00",
  line_main = "#42a5f5",
  line_complementary = "#78909c"
)

theme_minimalistic <- function() {
  theme_classic() +
    theme(plot.title = element_text(hjust = 0, colour = my_colours$title),
          plot.title.position = "plot",
          axis.line = element_line(colour = my_colours$axis),
          axis.ticks = element_line(colour = my_colours$axis),
          axis.text = element_text(colour = my_colours$axis),
          axis.title = element_text(colour = my_colours$axis),
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill='transparent', color=NA)
    )
}

theme_set(theme_minimalistic())

df <- readxl::read_xlsx("data-raw/Global_Carbon_Budget_2022v1.0.xlsx",
                        sheet = "Fossil Emissions by Category",
                        range = "A9:I181")

df %>% 
  ggplot(aes(x = Year, y = Per.Capita)) +
  geom_line() 
# +
#   transition_reveal(Year)

### Energy generation and consumption
## Useful columns (got the descriptions form the original website)
# solar_share_elec	Share of electricity consumption that comes from solar
# solar_cons_change_pct	Annual percentage change in solar consumption
# solar_share_energy	Share of primary energy consumption that comes from solar
# solar_cons_change_twh	Annual change in solar consumption, measured in terawatt-hours
# solar_consumption	Primary energy consumption from solar, measured in terawatt-hours
# solar_elec_per_capita	Per capita electricity consumption from solar, measured in kilowatt-hours
# solar_energy_per_capita	Per capita primary energy consumption from solar, measured in kilowatt-hours
# 
# solar_electricity	Electricity generation from solar, measured in terawatt-hours
# 
# primary_energy_consumption	Primary energy consumption, measured in terawatt-hours
# energy_per_capita	Primary energy consumption per capita, measured in kilowatt-hours per year
# per_capita_electricity	Electricity consumption per capita, measured in kilowatt-hours
# 
# population	Total population

df <- vroom::vroom("data-raw/World Energy Consumption.csv")

country_removed <- c("World", "Asia Pacific", "Europe", "North America",
                      "South & Central America", "Africa", "Oceania",
                      "OPEC", "Other Asia & Pacific", "Other CIS",
                      "Other Caribbean", "Other Middle East", "Other Northern Africa",
                      "Other South America", "Other Southern Africa",
                      "Europe (other)", "Eastern Africa", "Central America")

df_test <- df %>% 
  # Removing 2020 year because of many NA values
  filter(year < 2020 & year >= 2010) %>% 
  filter(!country %in% country_removed) %>% 
  remove_empty("cols") %>%
  mutate(year = as.character(year),
         country = case_match(
           country,
           "Czechia" ~ "Czech Republic",
           "Timor" ~ "Timor-Leste",
           .default = country
         ))

selected_country <- "None"

(df_test %>% 
  filter(country != selected_country) %>% 
  ggplot(aes(x = year, y = solar_electricity, group = country)) +
  {
    if (selected_country == "None") {
      geom_line(colour = my_colours$axis)
    } else {
      list(
        geom_line(colour = "#d9d9d9"),
        geom_line(data = (df_test %>% filter(country == selected_country)),
                  colour = my_colours$line_main)
      )
    }
  } +
  labs(x = "Year", y = "Solar Energy Generated") +
  scale_x_discrete(breaks = c(2010, 2019),
                   expand = expansion(mult = c(.02, .02))) +
  scale_y_continuous(labels = label_number(suffix = " TWh"),
                     expand = expansion(mult = c(.02, .02))) +
  theme(legend.position = "none")) %>% 
  ggplotly(tooltip = c("country", "solar_electricity"))


### Creating a map to show the last value
library(leaflet)
library(maps)

df_map_match <- df_test %>% 
  filter(year == 2019) %>%
  select(country, solar_electricity)

region_names <- map(plot = FALSE, namesonly = TRUE) 
map_countries <- map(fill = TRUE, 
                     plot = FALSE,
                     regions = region_names[-grep("Antarctica", region_names)]
                     # regions = c("Brazil", "Argentina", "USA")
                     )

map_countries$names <- map_countries$names %>% 
  str_replace_all(c("USA" = "United States",
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

map_countries$country_match <- sapply(map_countries$names, function(name) {
  get_map_country_name(name)
})

match_pos <- match(map_countries$country_match, 
                   df_map_match$country)

map_countries$value <- unlist(df_map_match[match_pos, 2])

map_countries$details <- sprintf(
  "<strong>%s</strong><br/>Produced: %.2f TW",
  map_countries$country_match, map_countries$value
) %>% lapply(htmltools::HTML)

# China x = 94.3444954473112, y = 37.7342522460484 
# USA x = -96.6157443480968, y = 40.6994211765168
region_test <- map.where(x = -96.6157443480968, y = 40.6994211765168)
map_test <- map(regions = region_test, fill = TRUE)


colours_palette <- colorNumeric("Greens", 
                                map_countries$value, 
                                na.color = "transparent")

leaflet(data = map_countries,
        options = leafletOptions(minZoom = 1.45, 
                                 maxZoom = 18,
                                 doubleClickZoom = FALSE,
                                 scrollWheelZoom = FALSE)) %>% 
  # addProviderTiles(providers$Thunderforest.MobileAtlas) %>%
  addPolygons(color = "grey",
              weight = 1,
              fillColor = ~colours_palette(value),
              highlightOptions = highlightOptions(color = "black",
                                                  weight = 1.5, 
                                                  bringToFront = TRUE),
              popup = map_countries$details,
              # popupOptions = popupOptions()
              # label = map_countries$details,
              # labelOptions = labelOptions(
              #   interactive = TRUE,
              #   style = list("font-weight" = "normal", padding = "3px 8px"),
              #   textsize = "15px",
              #   direction = "auto")
              ) %>%
  addPolygons(data = map_test,
              color = "black",
              weight = 3,
              layerId = paste(map_test$names, "Selected")) %>% 
  addLegend("bottomright",
            pal = colours_palette,
            values = map_countries$value,
            title = "Energy produced",
            labFormat = labelFormat(suffix = "kw/h"),
            opacity = 1)