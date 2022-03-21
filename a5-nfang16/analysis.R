library(dplyr)
shootings_2018 <- read.csv("data/shootings-2018.csv", stringsAsFactors = F)
shootings <- data.frame(shootings_2018)

# How many shootings occurred? 
num_shootings <- nrow(shootings_2018)

# How many lives were lost?
lives_lost <- sum(shootings_2018$num_killed)

# Which cities that were most impacted (you can decide how to measure "impact")?
most_impacted_cities <- shootings_2018 %>% 
  group_by(city) %>% 
  count() %>% 
  arrange(-n) %>% 
  ungroup() %>% 
  top_n(10, wt = n)

chicago <- most_impacted_cities %>% 
  filter(n == max(n))
chicago_killed <- chicago$n

# At least one other insight of your choice.
# Which state had the most killed, which state had the most injured
killed_by_state <- shootings_2018 %>% 
  group_by(state) %>% 
  count() %>% 
  arrange(-n) %>% 
  ungroup() %>% 
  top_n(10, wt = n)

injured_by_state <- shootings_2018 %>% 
  group_by(state) %>% 
  tally(num_injured) %>% 
  arrange(-n) %>% 
  ungroup() %>% 
  top_n(10, wt = n)

people_injured <- sum(shootings_2018$num_injured)

#Parkland Shooting 
parkland <- shootings_2018 %>% 
  filter(num_killed == max(num_killed))

parkland_date <- parkland$date
parkland_state<- parkland$state
parkland_city <- parkland$city
parkland_address <- parkland$address
parkland_killed <- parkland$num_killed
parkland_injured <- parkland$num_injured


#Interactive Map
library(leaflet)

shootings_2018 <- shootings_2018 %>% mutate(
  people.killed = cut(num_killed, c(0, 5, 10, 15, 20),
  labels = c('> 0 & <=5', '> 5 & <=10', '> 10 & <=15', '> 15 & <=20'))) %>% 
  split(.$people.killed)

l <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron)

names(shootings_2018) %>% 
  purrr::walk( function(df) {
    l <<- l %>% 
      addMarkers(data = shootings_2018[[df]], 
                 lng = ~long, lat = ~lat,
                 label = ~as.character(address), 
                 labelOptions = labelOptions(direction = 'auto'),
                 popup = ~paste0("On ", as.character(date), ", ", 
                                 as.character(num_killed), " Killed and ", 
                                as.character(num_injured), " Injured"), 
                 popupOptions = popupOptions(),
                 clusterOptions = markerClusterOptions(), group = df
      )})

l <- l %>% 
  addLayersControl(
    overlayGroups = names(shootings_2018),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  addMiniMap(tiles = providers$CartoDB.Positron, width = 120, height = 80)

#Plot
library(ggplot2)
library(lubridate)
library(tidyr)
library(plotly)


shootings$index <- c(1:nrow(shootings))
a <- shootings$date
a <- gsub(",", "", shootings$date)
head(a)
date_df <- as.Date(a, format = "%B %d %Y")
head(date_df)
class(date_df)
date_df <- as.data.frame(date_df)
date_df$index <- c(1:nrow(date_df))
shootings <- merge(date_df, shootings, by = "index")


shooting_plot_a <- shootings %>% 
  select(date_df, num_killed) %>% 
  group_by(month = floor_date(date_df, "month")) %>% 
  summarize(num_killed = sum(num_killed))

shooting_plot_b <- shootings %>% 
  select(date_df, num_injured) %>% 
  group_by(month = floor_date(date_df, "month")) %>% 
  summarize(num_injured = sum(num_injured))

shooting_plot_final <- merge(shooting_plot_a, shooting_plot_b, by = "month")

shooting_plot_final <- shooting_plot_final %>% 
  gather(key = Type, value = Incidents, -month)

plot_final <- ggplot(shooting_plot_final) +
  geom_col(mapping = aes(x = month, y = Incidents, fill = Type)) +
  labs(title = "Deaths and Injuries by Month", x = "Months")
  

