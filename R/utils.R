### Some auxiliary functions
library(stringr)

get_map_country_name <- function(map_name) {
  substring(map_name, 
            0, 
            ifelse(!is.na(str_locate(map_name, ":")[1]), 
                   str_locate(map_name, ":")[1] - 1,
                   10000))
}
