# How Journalists Ask Questions of Data
# Howard Center Data Journalism Training Program
# Philip Merrill College of Journalism


# Lesson 3: Grouping and summarizing 

# With these slides: https://docs.google.com/presentation/d/1eyK_fFIDrlgBXVzL7dGJwUfQKzj_2j2pEMn-P3Fueik/edit?usp=sharing


#This lesson builds on the work from Lesson 2. 
# Load tidyverse library 
library(tidyverse)
# Import Life Expectancy Data
life_expect <- read.csv("./assets/lex.csv") 

#Create variables for lesson below
life2025 <- life_expect |> 
  summarize(mean(X2025)) |> 
  pull() 

life1975 <- life_expect |> 
  summarise(mean(X1975, na.rm = TRUE)) |> 
  pull() 

life1925 <- life_expect |> 
  summarise(mean(X1925, na.rm = TRUE)) |> 
  pull() 

# Grouping and Summing.


# Counting 
# We know the average life expectancy for 2025, 1975 and 1925.
# Let's count the number of countries above these averages

life_expect |> 
  select(country, X2025) |> 
  filter(X2025 > life2025) |> 
  count()

# There are 115 countries above the mean life expectancy of 73.2 years 
# in 2025


#YOUR TURN Lesson 3 Your Turn: Avg Life Expectancy 1975

# Using the example above, determine the the number of countries above the
# 1975 average life expectancy


#this portion is blank for the student. 
# The answer is 113
# life_expect |>
#   select(country, X1975) |>
#   filter(X1975 > life1975) |>
#   count()

# Do the same, determine the the number of countries above the
# 1925 average life expectancy


#this portion is blank for the student. 
# The answer is 64
# life_expect |>
#   select(country, X1925) |>
#   filter(X1925 > life1925) |>
#   count()


### Mutate

# We are now going to categorize countries as above or below the mean life
# expectancy by using the mutate command. Mutate creates a new column and
# there are various strategies to fill in new results. Here we will assign
# **above** to all counties in 1925 above the mean life expectancy of
# 38.11 years. We are using case_when which sets up a logical 'if/else'
# statement to apply the category. The results will be put into a new
# dataframe, life_1925


life_1925 <- life_expect |> 
  select(country, X1925) |> 
  filter(!is.na(X1925)) |>
  mutate(category = case_when(
         X1925 > life1925 ~ "above",
          X1925 < life1925 ~ "below")
) 


# Let's count the results

life_1925 |> 
  count(category)


# YOUR TURN

# Using the example above, categorize countries as being above or below
# the 1975 average life expectancy. put results in a new dataframe, "life_1975"


#this portion is blank for the student. 
# life_1975 <- life_expect |>
#   select(country, X1975) |>
#   mutate(category = case_when(
#          X1975 > life1975 ~ "above",
#           X1975 < life1975 ~ "below")
# )
#
# Now do the same, categorizing countries as being above or below
# the 2025 average life expectancy. put results in a new dataframe, "life_2025"


#this portion is blank for the student. 
# life_2025 <- life_expect |>
#   select(country, X2025) |>
#   mutate(category = case_when(
#          X2025 > life2025 ~ "above",
#           X2025 < life2025 ~ "below")
# )


# We will now combine the three results. But first, we rename a column 
# to the same name for all three time periods. This is a good
# example of using mutate.


# Transform the dataframes, provide year column, rename value
life_1925 <- life_1925 |> 
  rename(value = X1925) |> 
  mutate(year = 1925)

life_1975 <- life_1975|>
  rename(value = X1975)|>
  mutate(year = 1975)

life_2025 <- life_2025|>
  rename(value = X2025)|>
  mutate(year = 2025)


# Joins
# We use a basic R command, rbind, that combines dataframes with 
# identical column names and formats

# Combine the dataframes
total <- rbind(life_1925, life_1975, life_2025)

# Notice how the "total" dataframe now holds 588 rows, the addition of
# three dataframes with 196 values each. It's a good idea to observe the
# row count in a newly combined dataframe and make sure all adds up.

# Count totals by time period
# Here we are grouping the dataframe by year (three options, 1925, 1975,
# 2025) and by category (above below) and then counting the categories by
# year. Pretty slick!

total |> 
  group_by(year, category) |> 
  count(category)


