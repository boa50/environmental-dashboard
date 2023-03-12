library(dplyr)
library(ggplot2)
library(gganimate)
library(plotly)
library(janitor)
library(gghighlight)

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
  geom_line() +
  transition_reveal(Year)

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
  mutate(year = as.character(year))

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
  theme(legend.position = "none")) %>% 
  ggplotly(tooltip = c("country", "solar_electricity"))