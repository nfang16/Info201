
library("httr")
library("jsonlite")

source("api-keys.R")

library("knitr")
library("kableExtra")
library("tidyr")
library("splitstackshape")
library("dplyr")
library("ggplot2")
library("tidyselect")
library("lubridate")


base_uri_publica <- "https://api.propublica.org/congress/v1"
endpoint_uri_publica <- paste0("/members", "/house", "/HI", "/current.json")
uri_publica <- paste0(base_uri_publica, endpoint_uri_publica)
response_publica <- GET(uri_publica, add_headers("X-API-Key" = propublica_key))
body_publica <- fromJSON(content(response_publica, "text"))

class(body_publica)
is.data.frame(body_publica)
names(body_publica$results)
results_publica <- body_publica$results

results_publica[1, 8] <- "Male"
results_publica[2, 8] <- "Female"
results_publica[1, 9] <- "Democratic"
results_publica [2, 9] <- "Democratic"

first_plot <- ggplot(data = results_publica) +
  geom_col(mapping = aes(x = gender, y = "1", fill = gender)) +
  scale_fill_manual(values = c("pink", "#56B4E9")) +
  labs(title = "Representatives By Gender",
       x = "Genders",
       y = "Number of Representatives") +
  theme_classic(base_size = 14)
  
results_publica <- results_publica %>% mutate(num_reps = c(0, 2)) %>%
  mutate(parties = c("Republican", "Democratic"))


second_plot <- ggplot(data = results_publica) +
  geom_col(mapping = aes(x = parties, y = num_reps, fill = party)) +
  scale_fill_manual(values = c("blue")) +
  labs(title = "Representatives By Party",
       y = "Number of Reps",
       x = "Parties") +
  theme_grey(base_size = 14)

base_uri_gabbard <- "https://api.propublica.org/congress/v1/members"
endpoint_uri_gabbard <- paste0("/G000571", ".json")
uri_gabbard <- paste0(base_uri_gabbard, endpoint_uri_gabbard)
response_gabbard <- GET(uri_gabbard, add_headers("X-API-Key" = propublica_key))
body_gabbard <- fromJSON(content(response_gabbard, "text"))


class(body_gabbard)
names(body_gabbard$results)
results_gabbard <- body_gabbard$results 


dob_gabbard <- as.Date(results_gabbard$date_of_birth)

age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

age_gabbard <- age(dob_gabbard)
twitter_gabbard <- results_gabbard$twitter_account

base_uri_gabbard_votes <- "https://api.propublica.org/congress/v1/members"
endpoint_uri_gabbard_votes <- paste0("/G000571", "/votes.json")
uri_gabbard_votes <- paste0(base_uri_gabbard_votes, endpoint_uri_gabbard_votes)
response_gabbard_votes <- GET(uri_gabbard_votes,
                              add_headers("X-API-Key" = propublica_key))
body_gabbard_votes <- fromJSON(content(response_gabbard_votes, "text"))
  
  
class(body_gabbard_votes)
names(body_gabbard_votes$results) 
results_gabbard_votes <- body_gabbard_votes$results[[4]]
results_votes <- data.frame(results_gabbard_votes)

yes_votes <- grep("\\<yes\\>", results_votes)
num_votes <- nrow(results_votes)
