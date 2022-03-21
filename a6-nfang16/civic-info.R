
library("httr")
library("jsonlite")
source("api-keys.R")
library("knitr")
library("kableExtra")
library("tidyr")
library("splitstackshape")
library("dplyr")
library("lintr")

base_uri <- "https://www.googleapis.com/civicinfo/v2/representatives"
query_params <- list("key" = api_key, "address" = "96817")
response <- GET(paste0(base_uri), query = query_params)
body <- fromJSON(content(response, "text"))

class(body)
is.data.frame(body)
names(body)
names(body$officials)

results <- flatten(body$officials)

officials <- body$officials
offices <- body$offices

num_to_rep <- unlist(lapply(body$offices$officialIndices, length))
expanded <- offices[rep(row.names(offices), num_to_rep), ]

officials <- officials %>% mutate(index = row_number() -1)
expanded <- expanded %>% mutate(index = row_number() -1) %>%
  rename(position = name)

table_data <- merge(expanded, officials, by = "index")
final_table <- table_data %>%
  select(name, position, party, emails, phones, photoUrl)

final_table[5, 6] <- "https://upload.wikimedia.org/wikipedia/commons/a/ad/Ed_Case%2C_official_portrait%2C_116th_Congress.jpg"
final_table[7, 6] <- "http://mauinotices.com/wp-content/uploads/2017/08/Senator-Josh-Green.png"
final_table[8, 6] <- "http://www.hawaiiislandrealtors.org/wp-content/uploads/2013/09/WKA_suit_130317.jpg"
final_table[9, 6] <- "http://elections.staradvertiser.com/resources/candidates/images/John_s%20Photo%20-%20John%20Waihee%20IV.jpg"
final_table[10, 6] <- "http://elections.staradvertiser.com/resources/candidates/images/12FCDA08-0694-4F05-939B-8F18F1F25739%20-%20Brendon%20Kalei_aina%20Lee.jpeg.jpg"
final_table[11, 6] <- "https://i1.wp.com/www.hawaiianaffairs.org/wp-content/uploads/2018/06/ahu-isa.jpeg"
final_table[13, 6] <- "https://i0.wp.com/honoluluprosecutor.org/wp-content/uploads/2016/12/KANESHIRO_20121-001.jpg?resize=216%2C300&ssl=1"

final_table$img <- paste0("![Img](", final_table$photoUrl, ")")
table_with_picture <- final_table %>%
  select(name, position, party, emails, phones, img)


display_table <- table_with_picture %>%
  kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                position = "center", full_width = TRUE, font_size = 12)
