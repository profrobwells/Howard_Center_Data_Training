# How Journalists Ask Questions of Data
# Digital Certificate Program
# Philip Merrill College of Journalism


# Lesson 4: Statistics 

# With these slides: https://docs.google.com/presentation/d/1owa9YQigOodjx0nrdXz43V8QJovE5PviUqurZFDDaro/edit?usp=sharing


#This lesson builds on the work from Lesson 2 & 3. 
# Load tidyverse library 
library(tidyverse)
# Import Life Expectancy Data
life_expect <- read.csv("./assets/lex.csv") 

#Create variables
life1925 <- life_expect |> 
  summarise(mean_life = mean(X1925, na.rm = TRUE)) |> 
  pull() 

life_1925 <- life_expect |> 
  select(country, X1925) |> 
  mutate(category = case_when(
    X1925 > life1925 ~ "above",
    X1925 < life1925 ~ "below")
  )

life_1925 <- life_1925 |> 
  rename(value = X1925) |> 
  mutate(year = 1925)

life1975 <- life_expect |> 
  summarise(mean_life = mean(X1975, na.rm = TRUE)) |> 
  pull() 

life_1975 <- life_expect |> 
  select(country, X1975) |> 
  mutate(category = case_when(
    X1975 > life1975 ~ "above",
    X1975 < life1975 ~ "below")
  )

life_1975 <- life_1975 |> 
  rename(value = X1975) |> 
  mutate(year = 1975)

life2025 <- life_expect |> 
  summarise(mean_life = mean(X2025, na.rm = TRUE)) |> 
  pull() 

life_2025 <- life_expect |> 
  select(country, X2025) |> 
  mutate(category = case_when(
    X2025 > life2025 ~ "above",
    X2025 < life2025 ~ "below")
  )


life_2025 <- life_2025|>
  rename(value = X2025)|>
  mutate(year = 2025)

total <- rbind(life_1925, life_1975, life_2025)


# Percentages

# Let's determine the percent above, below the mean
# 
# Step 1: Count the categories by year by grouping by year, we can count up the 
# above and below average countries. The results are in the colummn n
# 

total |> 
   group_by(year) |> 
  count(category) 


# Step Two: Percentage of Whole
# 
# We add two lines of code to calculate the percentage. Note how mutate
# creates a new column that stores the value. And we format it at the end
# to make it look pretty.
# 
# The percent calculation divides the category total into the full number
# denominator. Because 1925 has 10 null values, we filter those out.
# Remember, when dealing with historical data, nations and language can
# change over time.


total |> 
  group_by(year) |> 
  count(category, name = "total") |> 
  filter(!is.na(category)) |> #drops the counties without data
  mutate(percent = total/sum(total)) |> 
  mutate(percent = round(percent*100, 1))

# Percentage change This is a great tool for understanding trends,
# especially when dealing with disparate values. The calculation is (new
# value - old value)/old value \*100 We'll calculate the percentage change
# for Afghanistan, 1925-2025

life_expect |> 
  select(country, X1925, X2025) |> 
  filter(country =="Afghanistan") |> 
  mutate(life_change = ((X2025-X1925) / X1925)*100) 


# So life expectancy improved 67% in Afghanistan from 1925-2025
# 
# To improve on the display, we use the package formattable that adds the
# percentage sign


install.packages(formattable)
library(formattable)
life_expect |> 
  select(country, X1925, X2025) |> 
  filter(country =="Afghanistan") |> 
  mutate(life_change = ((X2025-X1925) / X1925)) |> 
  mutate(life_change = formattable::percent(life_change, 1))


# Biggest / smallest improvements in life expectancy 
# Now we will calculate
# percentage change life expectancy for all countries And we use slice_max
# to extract the top 10 results


top <- life_expect |> 
  select(country, X2025, X1925) |> 
  # filter(country =="Afghanistan") |> 
  mutate(pct_change = round((X2025-X1925) / X1925*100,1)) |> 
  slice_max(pct_change, n= 10) 
top


# And slice_min to extract the bottom 10 results


bottom <- life_expect |> 
  select(country, X2025, X1925) |> 
  mutate(pct_change = round(((X2025-X1925) / X1925)*100,1)) |> 
  filter(!is.na(pct_change)) |> #filter out NA values
  filter(X2025 > 1) |> # filter out a random 0 value for Hong Kong in 2025
  slice_min(pct_change, n= 10)
bottom


# YOUR TURN
# Calculate the percentage change from 1800 to 1900 and from 1900 to 2000
# Create two tables with the top 10 biggest increases for each period


#this portion is blank for the student. 
# change <- life_expect |>
#   select(country, X1800, X1900, X2000) |>
#   mutate(pct_1900_1800 = ((X1900-X1800) / X1800)*100) |>
#   mutate(pct_2000_1900 = ((X2000-X1900) / X1900)*100)
# 
# pct_1900_1800 <- change |>
#   select(country, X1900, X1800, pct_1900_1800) |>
#   slice_max(pct_1900_1800, n=10)
# 
# pct_2000_1900 <- change |>
#   select(country, X2000, X1900, pct_2000_1900) |>
#   slice_max(pct_2000_1900, n=10)

# Distribution of the data How are countries grouped? Were there many
# below the average?
# summary() is a powerful command
  
life_1925 |> 
  summary(value)


# We see the lowest value is 21 years life expectancy, the first quartile
# is up to 32 years, the median value was 36 years, the mean or average
# was 38 years, the third quartile was 40 years and the maximum value was
# 64 years.
# 
# Let's explore the distribution by using a tool called standard deviation


life_1925_sd <- life_1925 |> 
  summarize(standard_deviation = sd(value, na.rm = TRUE))
life_1925_sd


# So we know the mean life expectancy is 38 years. Tells us that 68% of
# the countries in 1925 fall within one standard deviation, or 9.3 years,
# of the mean. In other words, 68% of the countries had a life expectancy
# between 28.3 years and 46.3 years.
# 
# Just about all countries, or 95%, fall within two standard deviations
# from the mean, or 18.6 years; that would be between 19.4 years and 56.6
# years of age.
# 
# And nearly all counties are within three standard deviations, or 27.9
# years, or 10.1 years and 65.9 years.
# 
# This means that any value within 9.3 percentage points of the mean (38
# years), either above or below, is within the first group. This provides
# a sense of outliers and how dispersed the data may be in the dataset.


# Another way to examine data is by calculating results in percentiles. We
# use the command ntile

life_1925 |> 
  mutate(percentile = ntile(value, 100)) 

# It lists each country by its percentile ranking. Nice!

# YOUR TURN
# List all countries in the top 90th percentile for life expectancy in
# 1925
