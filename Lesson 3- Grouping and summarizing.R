# How Journalists Ask Questions of Data
# Digital Certificate Program
# Philip Merrill College of Journalism


# Lesson 3: Grouping and summarizing 

#This lesson builds on the work from Lesson 2. 
# Load tidyverse library 
library(tidyverse)
# Import Life Expectancy Data
life_expect <- read.csv("./assets/lex.csv") 

#Create variables for lesson below
life2025 <- life_expect |> 
  summarize(mean(X2025)) |> 
  pull() 

# Grouping and Summing.


# Counting 
# We know the average life expectancy for 2025, 1975 and 1925.
# Let's count the number of countries above these averages

life_expect |> 
  select(country, X2025) |> 
  filter(X2025 > life2025) |> 
  count()



# YOUR TURN

# Using the example above, determine the the number of countries above the
# 1975 average life expectancy


#this portion is blank for the student. 
# The answer is 113
# life_expect |>
#   select(country, X1975) |>
#   filter(X1975 > life1975) |>
#   count()



# YOUR TURN

# Using the example above, determine the the number of countries above the
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
# there are various stratgies to fill in new results. Here we will assign
# **above** to all counties in 1925 above the mean life expectancy of
# 38.11 years. We are using case_when which sets up a logical 'if/else'
# statement to apply the category


life_1925 <- life_expect |> 
  select(country, X1925) |> 
  mutate(category = case_when(
         X1925 > life1925 ~ "above",
          X1925 < life1925 ~ "below")
)


# Let's count the results


life_1925 |> 
  count(category)

# YOUR TURN

# Using the example above, categorize countries as being above or below
# the 1975 average life expectancy


#this portion is blank for the student. 
# life_1975 <- life_expect |>
#   select(country, X1975) |>
#   mutate(category = case_when(
#          X1975 > life1975 ~ "above",
#           X1975 < life1975 ~ "below")
# )


# YOUR TURN

# Using the example above, categorize countries as being above or below
# the 2025 average life expectancy


#this portion is blank for the student. 
# life_2025 <- life_expect |>
#   select(country, X2025) |>
#   mutate(category = case_when(
#          X2025 > life2025 ~ "above",
#           X2025 < life2025 ~ "below")
# )


# Now we combine the three results --we rename a column to the same name
# for all three time periods so they can be combined. This is a good
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
We are going to use a basic R command, rbind, that combines dataframes with identical column names and formats

# Combine the dataframes
total <- rbind(life_1925, life_1975, life_2025)

# Count totals by time period

total |> 
  group_by(year, category) |> 
  count(category)


