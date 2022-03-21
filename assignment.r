# a3-using-data

# Before you get started, set your working directory using the Session menu

###################### DataFrame Manipulation (20 POINTS) ######################

# Create a vector `first_names` with 5 names in it
first_names <- c("Nick", "Josh", "K", "Matt", "Sophie")

# Create a vector `math_grades` with 5 hypothetical grades (0 - 100)
# in a math course (that correspond to the 5 names above)
math_grades <- c(88, 78, 99, 93, 82)

# Create a vector `spanish_grades` with 5 hypothetical grades (0 - 100)
# in a Spanish course (that correspond to the 5 names above
spanish_grades <- c(75, 85, 95, 80, 90)
# Create a data.frame variable `students` by combining
# the vectors `first_names`, `math_grades`, and `spanish_grades`
students <- data.frame(first_names, math_grades, spanish_grades)

# Create a variable `num_students` that contains the
# number of rows in your dataframe `students`
num_students <- nrow(students)

# Create a variable `num_courses` that contains the number of columns
# in your dataframe `students` minus one (b/c of their names)
num_courses <- ncol(students) - 1

# Add a new column `grade_diff` to your dataframe, which is equal to
# `students$math_grades` minus `students$spanish_grades`
students$grade_diff <- students$math_grades - students$spanish_grades

# Add another column `better_at_math` as a boolean (TRUE/FALSE) variable that
# indicates that a student got a better grade in math
students$better_at_math <- math_grades > spanish_grades

# Create a variable `num_better_at_math` that is the number
# (i.e., one numeric value) of students better at math
num_better_at_math <- nrow(students[students$better_at_math == TRUE, ])

# Write your `students` dataframe to a new .csv file inside your data/ directory
# with the filename `grades.csv`. Make sure *not* to write row names.
write.csv(students, "grades.csv")


########################### Loading R Data (30 points) #########################

# In this section, you'll work with the `Titanic` data set
# Which is built into the R environment. You should be able to `View()` it
# Pay *close attention* to what each column means. Use ?Titanic to read more.


# This data set actually loads in a format called a *table*
# See https://cran.r-project.org/web/packages/data.table/data.table.pdf
# Use the `is.data.frame()` function to test if it is a table.
is.data.frame(Titanic)

# You should convert the `Titanic` variable into a data frame;
# you can use the `data.frame()` function or `as.data.frame()`
# Be sure to **not** treat strings as factors!
titanic_data <- data.frame(Titanic, stringsAsFactors = FALSE)

# Create a variable `children` that contains *only* the rows of the data frame 
# with information about the children on the Titanic.
children <- titanic_data[titanic_data$Age == "Child", ]

# Create a variable `num_children` that is the total number of children.
# Hint: remember the `sum()` function!
num_children <- sum(titanic_data$Age == "Child" )

# Create a variable `most_lost` which has the *row* with the
# largest absolute number of losses (people who did not survive).
# Tip: if you want, you can use multiple statements (lines of code)
# if you find that helpful to create this variable.
most_lost <- titanic_data[titanic_data$Freq == max(titanic_data$Freq), ]

# Define a function called `survival_rate()` that takes in a ticket class
# (e.g., "1st", "2nd") as an argument.This function should return the following
# sentence that compares the *total survival rate* of adult men vs.
# "women and children" in that ticketing class. It should read (for example):
# "Of Crew class, 87% of women and children survived and 22% of men survived.".
# The approach you take to generating the sentence to return is up to you.
# A good solution will likely utilize filtering to produce the required data.
survival_rate <- function(ticket_class) {
  ticket_class_data <- titanic_data[titanic_data$Class == ticket_class, ]
  class_survivors <- ticket_class_data[ticket_class_data$Survived == "Yes", ]
  
  total_male_survivors <- class_survivors[class_survivors$Sex == "Male", ]
  male_adult_survivors <- sum(total_male_survivors[total_male_survivors$Age == "Adult", "Freq"])
  male_child_survivors <- sum(total_male_survivors[total_male_survivors$Age == "Child", "Freq"])
  
  total_female_survivors <- class_survivors[class_survivors$Sex == "Female", ]
  female_adult_survivors <- sum(total_female_survivors[total_female_survivors$Age == "Adult", "Freq"])
  female_child_survivors <- sum(total_female_survivors[total_female_survivors$Age == "Child", "Freq"])

  total_female_and_child_survivors <- female_child_survivors + female_adult_survivors + male_child_survivors
  
  total_male <- ticket_class_data[ticket_class_data$Sex == "Male", ]
  total_male_adult <- sum(total_male[total_male$Age == "Adult", "Freq"])
  total_male_child <- sum(total_male[total_male$Age == "Child", "Freq"])
  total_female <- ticket_class_data[ticket_class_data$Sex == "Female", ]
  total_female_adult <- sum(total_female[total_female$Age == "Adult", "Freq"])
  total_female_child <- sum(total_female[total_female$Age == "Child", "Freq"])
  
  total_female_and_child <- total_male_child + total_female_child + total_female_adult
  
  p_male_survivors <- round((male_adult_survivors / total_male_adult) * 100, digits = 0)
  p_female_and_child_survivors <- round((total_female_and_child_survivors / total_female_and_child) * 100, 
                                        digits = 0)
  
  return(paste("Of", ticket_class, "Class", p_female_and_child_survivors, "% of women and children survived and",
               p_male_survivors, "% of men survived."))
}

# Create variables `first_survived`, `second_survived`, `third_survived` and
# `crew_survived` by passing each class to your function above.
first_survived <- survival_rate("1st")
second_survived <- survival_rate("2nd")
third_survived <- survival_rate("3rd")
crew_survived <- survival_rate("Crew")
########################### Reading in Data (40 points)#########################
# In this section, we'll read in a .csv file with a tabular row/column layout
# This is like Microsoft Excel or Google Docs, but without the formatting.
# The .csv file we'll be working with has the life expectancy
# for each country in 1960 and 2013. We'll ask real-world questions about the
# data by writing the code that answers our question.


# Using the `read.csv` function, read the life_expectancy.csv file into
# a variable called `life_expectancy`. Makes sure not to read strings as factors
life_expectancy <- read.csv("data/life_expectancy.csv", stringsAsFactors = FALSE)

# Determine if `life_expectancy` is a data.frame by using
# the is.data.frame function. You may also want to View() it.
View(life_expectancy)
is.data.frame(life_expectancy)

# Create a column `life_expectancy$change` that is the change
# in life expectancy from 1960 to 2013
life_expectancy$change <- life_expectancy$le_2013 - life_expectancy$le_1960

# Create a variable `most_improved` that is the *name* of the country
# with the largest gain in life expectancy
most_improved <- life_expectancy[life_expectancy$change == max(life_expectancy$change), "country"]


# Create a variable `num_small_gain` that has the *number* of countries
# whose life expectance has improved fewer than 5 years between 1960 and 2013
num_small_gain <- nrow(life_expectancy[life_expectancy$change < 5, ])

# Write a function `country_change()` that takes in a country's name
# as a parameter, and returns it's change in life expectancy from 1960 to 2013
country_change <- function(country) {
  country_name <- life_expectancy[life_expectancy$country == country, ]
  return(change_in_le <- country_name[, "change"])
}

# Using your `country_change()` function, create a variable `sweden_change`
# that is the change in life expectancy from 1960 to 2013 in Sweden
sweden_change <- country_change("Sweden")

# Define a function `lowest_life_exp_in_region()` that takes in a **region**
# as an argument, and returns the **name of the country**
# with the lowest life expectancy in 2013 (in that region)
lowest_life_exp_in_region <- function(region) {
  region_name <- life_expectancy[life_expectancy$region == region, ]
  return(region_lowest_le <- region_name[region_name$le_2013 == min(region_name$le_2013), "country"])
}

# Using the function you just wrote, create a variable `lowest_in_south_asia`
# that is the country with the lowest life expectancy in 2013 in South Asia
lowest_in_south_asia <- lowest_life_exp_in_region("South Asia")

# Write a function `bigger_change()` that takes in two country names
# as parameters, and returns a sentence that describes which country experienced
# a larger gain in life expectancy (and by how many years).
# For example, if you passed the values "China", and "Bolivia" to you function,
# It would return this:
# "The country with the bigger change in life expectancy was China (gain=31.9),
#  whose life expectancy grew by 7.4 years more than Bolivia's (gain=24.5)."
# Make sure to round your numbers to one digit.

bigger_change <- function(country_1, country_2) {
  country_1_gain <- life_expectancy[life_expectancy$country == country_1, "change"]
  country_2_gain <- life_expectancy[life_expectancy$country == country_2, "change"]
  
  if(country_1_gain > country_2_gain) {
    return(paste("The country with the bigger change in life expectancy was",country_1,", gain=",country_1_gain,
                 ", whose life expectancy grew by",country_1_gain - country_2_gain,"years more than",
                 country_2,",gain=",country_2_gain,"."))
  } else {
    return(paste("The country with the bigger change in life expectancy was",country_2,", gain=",country_2_gain,
                 ", whose life expectancy grew by",country_2_gain - country_1_gain,"years more than",
                 country_1,", gain=",country_1_gain,"."))
  }
}

# Using your `bigger_change()` function, create a variable `usa_or_france`
# that describes who had a larger gain in life expectancy (the U.S. or France)
usa_or_france <- bigger_change("United States", "France") 

# Write your `life_expectancy` data.frame to a new .csv file to your
# data/ directory with the filename `life_expectancy_with_change.csv`.
# Make sure not to write row names.
write.csv(life_expectancy, "life_expectancy_with_change.csv")

############################## Challenge (10 points) ###########################
# Create a variable `highest_avg_change` that has the name of the region with
# the highest *average change* in life expectancy between the two time points.
# To do this, you'll need to *compute the average* change across the countries
# in each region, and then compare the averages across regions.
# Feel free to use any library of your choice, or base R functions.
library("dplyr")

mean_changes_by_region <- life_expectancy %>%
  group_by(region) %>%
  summarize(
    mean_of_region = mean(change)
  )

highest_avg_change <- mean_changes_by_region[mean_changes_by_region$mean_of_region == 
                                               max(mean_changes_by_region$mean_of_region), ]


# Create a *well labeled* plot (readable title, x-axis, y-axis) showing
# Life expectancy in 1960 v.s. Change in life expectancy
# Programmatically save (i.e., with code, not using the Export button)
# your graph as a .png file in your repo
# Then, in a comment below, *provide an interpretation* of the relationship
# you observe. Feel free to use any library of your choice, or base R functions.
install.packages("ggplot2")
library("ggplot2")
png(filename = "~/Desktop/a3-nfang16/Life Expectancy in 1960 vs. Change in Life Expectancy.png")
ggplot() +
  geom_point(
    mapping = aes(x = le_1960, y = change),
    data = life_expectancy
  ) +
  labs(
    title = "Life Expectancy in 1960 vs. Change in Life Expectancy",
    x = "Change in LE",
    y = "LE in 1960"
  )

dev.off()

# Put your interpretation here!
The life expectancy increased more for poeple who had a lower initial LE. 

