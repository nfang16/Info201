wdvp <- read.csv("data/wdvp.csv", stringsAsFactors = FALSE)

library(dplyr)
library(lubridate)
library(tidyr)
library(plotly)
library(lintr)

    labs(title = "Education vs. Unemployment", x = "School Life Expectancy",
         y = "Unemployment %")

    wdvp[3:34] <- lapply(wdvp[3:34], as.numeric)

create_plot <- function(dataset) {
  plot_info <- plot_ly(data = dataset, x = ~school_life_expectancy_years,
                  y = ~unemployment_pct, type = "scatter",
                  #Hover Text
                  text = ~paste(country_name),
                  color = ~population, size = ~population) %>%
    layout(title = "Education vs. Unemployment",
           xaxis = list(title = "School Life Expectancy (Years)"),
           yaxis = list(title = "Unemployment (%)")
           )}

map <- create_plot(wdvp)