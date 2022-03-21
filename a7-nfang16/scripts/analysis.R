library("dplyr")
library("lintr")
data <- read.csv("wdvp.csv", stringsAsFactors = FALSE)

## List that returns information about the dataset 
## The variables that aren't included in the list are 
## to help change the columns to a numeric type in order 
## to do some calculations about that specific column
summary_function <- function(dataset){
  ret <- list()
  ret$max_population <- data %>% filter(population == max(population)) %>%
                              pull(population)
  ret$country_with_max_pop <- data %>% select(country_name, population) %>%
                              filter(population == max(population)) %>%
                              select(country_name) %>% pull(country_name)
  ret$mean_surface_area <- mean(data$surface_area_km2)
  ret$countries <- nrow(dataset)
  high_happiness <- data %>% filter(happy_planet_index != "-")
  happiness_index <- as.numeric(high_happiness$happy_planet_index)
  ret$mean_happiness <- mean(happiness_index)
  human_development <- data %>% filter(human_development_index != "-")
  human_dev <- as.numeric(human_development$human_development_index)
  ret$min_human_dev <- min(human_dev)
  unemployment <- data %>% filter(unemployment_pct != "-")
  unemployment_rate <- as.numeric(unemployment$unemployment_pct)
  ret$max_unemployment <- max(unemployment_rate)
  return(ret)
}
summary <- summary_function(data)