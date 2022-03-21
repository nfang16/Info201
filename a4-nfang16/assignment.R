# a4-data-wrangling

# Before you get started, set your working directory using the Session menu.
# While we (mostly) don't require specific variable names, we will be checking
# your code (structure + style) as well as your output. The .csv files you save 
# must have the described format/column names, and the file name provided. 
# For all .csv file, make sure to exclude rownames, and write them to the 
# a folder called `output/` which you will create below.
library(dplyr)
################################### Set up ###################################

# Install (if not installed) + load dplyr package 

# Read in `any_drinking.csv` data using a *relative path*
any_drinking <- read.csv("data/any_drinking.csv", stringsAsFactors = FALSE)
# Read in `binge.drinking.csv` data using a *relative path*
binge_drinking <- read.csv("data/binge_drinking.csv", stringsAsFactors = FALSE)
# Create a directory (using R) called "output" in your project directory
# Make sure to *suppress any warnings*, in case the directory already exists
# You must save all .csv files in this directory (last reminder!)
dir.create("output", showWarnings = FALSE)
setwd(file.path("~/Desktop/Info Assignments/a4-nfang16/output"))
############################# Any drinking in 2012 #############################

# For this first section, you will work only with the *any drinking* dataset.
# In particular, we'll focus on data from 2012. All output should include only 
# the relevant 2012 columns (as well as `state` + `location`), described below. 


# Create a new data.frame that has the `state` and `location` columns, 
# and all columns with data from 2012. you will use this dataframe throughout 
# the rest of this section.
anydrinking <- select(any_drinking, state, location, both_sexes_2012, 
                      females_2012, males_2012)

# Using the (new) 2012 data, create a column `diff` that has 
# the difference in male and female drinking rates
anydrinking <- mutate(
  anydrinking,
  difference = males_2012 - females_2012,
  abs_difference = abs(difference)
)

# Write your data to a file `diff_2012.csv` (in your `output/` directory)
# Make sure to exclude rownames (for all .csv files! -- last reminder).
write.csv(anydrinking, "diff_2012.csv", row.names = FALSE)

# To answer "Are there any locations where females drink more than males"?
# Create a new dataframe by filtering the 2012 dataframe to the rows that 
# meet the criterion. Keep only the `state`, `location`, and column of interest. 
# Write your answer to `more_f_than_m.csv`.
anydrinking_1 <- select(anydrinking, state, location, difference)
more_f_than_male <- filter(anydrinking_1, difference < 0)
write.csv(more_f_than_male, "more_f_than_male.csv", row.names = FALSE)

# To answer the question: "What is the location in which male and female 
# drinking rates are most similar", create a new dataframe by filtering the 2012 
# dataframe to the rows that meet the criterion. Keep only the `state`, 
# `location`, and column of interest.Write your answer to `most_similar.csv`.
anydrinking_2 <- select(anydrinking, state, location, males_2012, females_2012,
                        difference)
most_similar <- filter(anydrinking_2, difference < 1)
write.csv(most_similar, "most_similar.csv", row.names = FALSE)

# As you've (hopefully) noticed, the `location` column includes national, 
# state, and county level estimates. However, many audiences may only be 
# interested in the *state* level data. Given that, you should do the following:
# Create a new data frame that is only the *state level* observations in 2012.
# For the sake of this analysis, you should treat Washington D.C. as a *state*
# Write this data frame to `state_only.csv`.
states_only <- anydrinking %>% 
  group_by(state) %>%
  filter(state != "National") %>% 
  summarize(
    state_both_mean = round(mean(both_sexes_2012), digits = 2), 
    state_male_mean = round(mean(males_2012), digits = 2),
    state_female_mean = round(mean(females_2012), digits = 2),
    state_difference_mean = round(mean(difference), digits = 2)
  )
write.csv(states_only, "states_only.csv", row.names = FALSE)

# Which state had the **highest** drinking rate for both sexes combined? 
# Your answer should be a *dataframe* of the state and value of interest
# Write this data frame to `highest_state.csv`.
highest_state <- states_only %>% 
  filter(state_both_mean == max(state_both_mean))
write.csv(highest_state, "highest_state.csv", row.names = FALSE)

# Which state had the **lowest** drinking rate for both sexes combined?
# Your answer should be a *dataframe* of the state and value of interest
# Write this data frame to `lowest_state.csv`.
lowest_state <- states_only %>% 
  filter(state_both_mean == min(state_both_mean))
write.csv(lowest_state, "lowest_state.csv", row.names = FALSE)

# What was the difference in prevalence between the state with the highest level
# of consumption,and the state with the lowest level of consumption?
# Your answer should be a single value (a dataframe storing one value is fine)
# Store your answer in a variable called `biggest_state_diff`.

combined_highest_lowest <- full_join(highest_state, lowest_state)
biggest_state_diff <- combined_highest_lowest %>% 
  summarize(
    difference_btwn_high_low = mean(state_both_mean)
  )

# Write a function called `get_state_data` that allows you to specify a state, 
# then saves a .csv file (`STATE_data.csv`) with observations from that state 
# This includes data about the state, as well as the counties in the state
# You should use the full any.drinking dataset in this function (not just 2012)
get_state_data <- function(state_name) {
  state_data <- filter(any_drinking, state == state_name)
  write.csv(state_data, paste(state_name, "data.csv"), row.names = FALSE)
}

# Demonstrate that you function works by passing "Utah" to the function
get_state_data("Utah")

############################ Binge drinking Dataset ############################

# In this section, you will ask a variety of questions regarding the 
# `binge_drinking.csv` dataset. More specifically, you will analyze a subset of 
# the observations of *just the counties* (exclude state/national estimates!).
# You will store your answers in a *named list*, and at the end of the section, 
# Convert that list to a data frame, and write the data frame to a .csv file.
# Pay close attention to the *names* to be used in the list.


# Create a dataframe with only the county level observations from the 
# `binge_driking.csv` dataset. You should (again) think of Washington D.C. as 
# a state, and therefore *exclude it here*.
# However, you should include "county-like" areas such as parishes and boroughs
binge.drinking <- filter(binge_drinking, location != state, location != 
                           "United States", location != "District of Columbia")

# Create an empty list in which to store answers to the questions below.
a <- list()

# What is the average county level of binge drinking in 2012 for both sexes?
# Store the number in your list as `avg_both_sexes`.

  avg_both_sexes <- binge.drinking %>% 
    select(state, location, both_sexes_2012) %>% 
    summarize(avg_both_sexes = mean(both_sexes_2012))
  a$avg_both_sexes <- 17.96036

# What is the name of the county with the largest increase in male binge 
# drinking between 2002 and 2012?
# Store the county name in your list as `largest_male_increase`.

  largest_male_increase <- binge.drinking %>% 
    mutate(diff_2002_2012 = males_2012 - males_2002) %>% 
    filter(diff_2002_2012 == max(diff_2002_2012)) %>% 
    select(location) 
  a$largest_male_increase <- "Loving County"
  
  # a$largest_male_increase <- largest_male_increase
# How many counties experienced an increase in male binge drinking between
# 2002 and 2012?
# Store the number in your list as `num_male_increase`.

  num_male_increase <- binge.drinking %>% 
    mutate(diff_2002_2012 = males_2012 - males_2002) %>% 
    filter(diff_2002_2012 > 0)
 
  num_male_increase %>% 
    summarize(nrow(num_male_increase))
  a$num_male_increase <- 1992   

# What fraction of counties experienced an increase in male binge drinking 
# between 2002 and 2012?
# Store the fraction (num/total) in your list as `frac_male_increase`.

  frac_male_increase <- nrow(num_male_increase) / nrow(binge.drinking)
  a$frac_male_increase <- frac_male_increase
                    
# How many counties experienced an increase in female binge drinking between
# 2002 and 2012?
# Store the number in your list as `num_female_increase`.

  num_female_increase <- binge.drinking %>% 
    mutate(diff_females_2002_2012 = females_2012 - females_2002) %>% 
    filter(diff_females_2002_2012 > 0)
   
  num_female_increase %>% 
    summarize(nrow(num_female_increase))
  
  a$num_female_increase <- 2580

# What fraction of counties experienced an increase in female binge drinking 
# between 2002 and 2012?
# Store the fraction (num/total) in your list as `frac_female_increase`.

  frac_female_increase <- nrow(num_female_increase) / nrow(binge.drinking)
  a$frac_female_increase <- frac_female_increase

# How many counties experienced a rise in female binge drinking *and* 
# a decline in male binge drinking?
# Store the number in your list as `num_f_increase_m_decrease`.
  
  num_f_increase_m_decrease <- binge.drinking %>% 
    mutate(diff_females_2002_2012 = females_2012 - females_2002) %>% 
    filter(diff_females_2002_2012 > 0) %>% 
    mutate(diff_2002_2012 = males_2012 - males_2002) %>% 
    filter(diff_2002_2012 < 0)
    
  num_f_increase_m_decrease %>% 
    summarize(nrow(num_f_increase_m_decrease))
  a$num_f_increase_m_decrease <- nrow(num_f_increase_m_decrease)
  
# Convert your list to a data frame, and write the results 
# to the file `binge_info.csv`

  binge_info <- data.frame(a)
  write.csv(binge_info, "binge._info.csv", row.names = FALSE)

# The next questions return *data frames as results*:

# What is the *minimum* level of binge drinking in each state in 2012 for 
# both sexes (across the counties)? Your answer should contain roughly 50 values
# (one for each state), unless there are two counties in a state with the 
# same value. Your answer should be a *dataframe* with the location, state, and 
# 2012 binge drinking rate. Write this to a file called `min_binge.csv`.
  
min_binge_2012 <- binge.drinking %>% 
  group_by(state) %>% 
  select(state, location, both_sexes_2012) %>% 
  summarize(min_binge_drinking = min(both_sexes_2012)) 

  write.csv(min_binge_2012, "min_binge.csv", row.names = FALSE)

# What is the *maximum* level of binge drinking in each state in 2012 for 
# both sexes (across the counties)? Your answer should contain roughly 50 values
# (one for each state), unless there are two counties in a state with the 
# same value. Your answer should be a *dataframe* with the location, state, and 
# 2012 binge drinking rate. Write this to a file called `max_binge.csv`.

max_binge_2012 <- binge.drinking %>% 
  group_by(state) %>% 
  select(state, location, both_sexes_2012) %>% 
  summarize(max_binge_drinking = max(both_sexes_2012)) 

write.csv(max_binge_2012, "max_binge.csv", row.names = FALSE)
                                  
################################# Joining Data #################################
# You'll often have to join different datasets together in order to ask more 
# involved questions of your dataset. In order to join our datasets together, 
# you'll have to rename their columns to differentiate them. 


# First, rename all prevalence columns in the any_drinking dataset to the 
# have prefix "any_" (i.e., `males_2002` should now be `any_males_2002`)
# Hint: you can get (and set!) column names using the colnames function. 
# This may take multiple lines of code.

colnames(any_drinking) <- gsub("females", "any_females", colnames(any_drinking))

colnames(any_drinking) <- gsub("both", "any_both", colnames(any_drinking))

colnames(any_drinking) <- sub("^males_2002$", 
                              "any_males_2002", colnames(any_drinking))
colnames(any_drinking) <- sub("^males_2003$", 
                              "any_males_2003", colnames(any_drinking))
colnames(any_drinking) <- sub("^males_2004$", 
                              "any_males_2004", colnames(any_drinking))
colnames(any_drinking) <- sub("^males_2005$", 
                              "any_males_2005", colnames(any_drinking))
colnames(any_drinking) <- sub("^males_2006$", 
                              "any_males_2006", colnames(any_drinking))
colnames(any_drinking) <- sub("^males_2007$", 
                              "any_males_2007", colnames(any_drinking))
colnames(any_drinking) <- sub("^males_2008$", 
                              "any_males_2008", colnames(any_drinking))
colnames(any_drinking) <- sub("^males_2009$", 
                              "any_males_2009", colnames(any_drinking))
colnames(any_drinking) <- sub("^males_2010$", 
                              "any_males_2010", colnames(any_drinking))
colnames(any_drinking) <- sub("^males_2011$", 
                              "any_males_2011", colnames(any_drinking))
colnames(any_drinking) <- sub("^males_2012$", 
                              "any_males_2012", colnames(any_drinking))

# Then, rename all prevalence columns in the binge_drinking dataset to the have
# the prefix "binge_" (i.e., `males_2002` should now be `binge_males_2002`)
# This may take multiple lines of code.

colnames(binge_drinking) <- gsub("females", "binge_females", 
                                 colnames(binge_drinking))
colnames(binge_drinking) <- gsub("both", "binge_both", colnames(binge_drinking))

colnames(binge_drinking) <- sub("^males_2002$", 
                              "binge_males_2002", colnames(binge_drinking))
colnames(binge_drinking) <- sub("^males_2003$", 
                                "binge_males_2003", colnames(binge_drinking))
colnames(binge_drinking) <- sub("^males_2004$", 
                                "binge_males_2004", colnames(binge_drinking))
colnames(binge_drinking) <- sub("^males_2005$", 
                                "binge_males_2005", colnames(binge_drinking))
colnames(binge_drinking) <- sub("^males_2006$", 
                                "binge_males_2006", colnames(binge_drinking))
colnames(binge_drinking) <- sub("^males_2007$", 
                                "binge_males_2007", colnames(binge_drinking))
colnames(binge_drinking) <- sub("^males_2008$", 
                                "binge_males_2008", colnames(binge_drinking))
colnames(binge_drinking) <- sub("^males_2009$", 
                                "binge_males_2009", colnames(binge_drinking))
colnames(binge_drinking) <- sub("^males_2010$", 
                                "binge_males_2010", colnames(binge_drinking))
colnames(binge_drinking) <- sub("^males_2011$", 
                                "binge_males_2011", colnames(binge_drinking))
colnames(binge_drinking) <- sub("^males_2012$", 
                                "binge_males_2012", colnames(binge_drinking))

# Then, create a dataframe by joining together the both datasets. 
# Think carefully about the *type* of join you want to do, and what the 
# *identifying columns* are. You will use this (joined) data to answer the 
# questions below.

joined_drinking <- left_join(any_drinking, binge_drinking, by = "location")

# Create a column `diff_2012` storing the difference between `any` and `binge` 
# drinking for both sexes in 2012

joined_drinking$diff_2012 <- abs(joined_drinking$any_both_sexes_2012 - 
  joined_drinking$binge_both_sexes_2012)


# Which location has the greatest *absolute* difference between `any` and 
# `binge` drinking? Your answer should be a one row data frame with the state, 
# location, and column of interest (diff_2012). 
# Write this dataframe to `biggest_abs_diff_2012.csv`.

biggest_abs_diff_2012 <- joined_drinking %>% 
  select(state.x, location, diff_2012) %>% 
  filter(diff_2012 == max(diff_2012))
write.csv(biggest_abs_diff_2012, "biggest_abs_diff_2012.csv", row.names = FALSE)

# Which location has the smallest *absolute* difference between `any` and 
# `binge` drinking? Your answer should be a one row data frame with the state, 
# location, and column of interest (diff_2012). 
# Write this dataframe to `smallest_abs_diff_2012.csv`.

smallest_abs_diff_2012 <- joined_drinking %>% 
  select(state.x, location, diff_2012) %>% 
  filter(diff_2012 == min(diff_2012))
write.csv(smallest_abs_diff_2012, "smallest_abs_diff_2012.csv", 
          row.names = FALSE)

############## Write a function to ask your own question(s) ####################
# Even in an entry level data analyst role, people are expected to come up with 
# their own questions of interest (not just answer the questions that other 
# people have). For this section, you should *write a function* that allows you 
# to ask the same question on different subsets of data. For example, you may 
# want to ask about the highest/lowest drinking level given a state or year. 
# The purpose of your function should be evident given the input parameters and 
# function name. After writing your function, *demonstrate* that the function 
# works by passing in different parameters to your function.

# **year** and **state**, returns the county and data with the highest drinking
year_and_state_data <- function(input_year, input_state, data_set) {
  year_data <- data_set %>% 
    select(state, location, contains(input_year)) %>% 
    filter(state == input_state) %>% 
    arrange_at(3, desc)
  write.csv(year_data, (paste("any_drinking", input_state, input_year, ".csv")))
}
  
year_and_state_data("2010", "Alabama", any_drinking)
################################### Challenge ##################################

# Using your function from part 1 that wrote a .csv file given a state name, 
# write a separate file for each of the 51 states (including Washington D.C.)
# The challenge is to do this in a *single line of (very concise) code*

lapply(unique(states_only$state), get_state_data)  

# Write a function that allows you to pass in a *dataframe* (i.e., in the format 
# of binge_drinking or any_drinking) *year*, and *state* of interest. The 
# function should saves a .csv file with observations from that state's counties
# (and the state itself). It should only write the columns `state`, `location`, 
# and data from the specified year. Before writing the .csv file, you should 
# *sort* the data.frame in descending order by the both_sexes drinking rate in 
# the specified year. The file name should have the format:
# `DRINKING_STATE_YEAR.csv` (i.e. `any_Utah_2005.csv`).
# To write this function, you will either have to use a combination of dplyr 
# and base R, or confront how dplyr uses *non-standard evaluation*
# Hint: https://github.com/tidyverse/dplyr/blob/34423af89703b0772d59edcd0f3485295b629ab0/vignettes/nse.Rmd
# Hint: https://www.r-bloggers.com/non-standard-evaluation-and-standard-evaluation-in-dplyr/

get_dataframe_data <- function(name, state_name, chosen_year) {
  filename <- gsub("_.*$", "", deparse(substitute(name)))  
  filename <- paste0(filename, state_name, chosen_year) 
  
  no <- select(name, "state", "location", contains(chosen_year))
  state_data <- filter(no, state == state_name)
  descending <- paste0("desc(", filename, ")")  
  final_table <- arrange_(no, descending)
  write.csv(final_table,".csv", row.names = F)
}

# Create the file `binge_Colorado_2007.csv` using your function.
get_dataframe_data(binge_drinking, "Colorado", "2007")
