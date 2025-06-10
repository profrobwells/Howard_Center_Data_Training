# How Journalists Ask Questions of Data
# Howard Center Data Journalism Training Program
# Philip Merrill College of Journalism


# Lesson 2: Manipulating Data 

# With these slides: https://docs.google.com/presentation/d/1zZK-WJ0hDS6myQsWA7BbFoxdnjhELlAMxjwvDXA5cJg/edit?usp=sharing

# What is R?

#INSERT VIDEO 1
# With this script: https://docs.google.com/document/d/19eEI9RDLh0crU-TkSz_n8pGHZjx4cufdeF1tGQjAhF0/edit?usp=sharing

# The basic R programming language comes equipped with the bread and
# butter math and visualization functions. You can perform basic
# mathematical operations, generate data visualizations, produce
# probability distributions, filter and transform data using the "base R"
# commands.

# What makes R so dynamic is the thousands of external libraries (often called 
# "packages") that allow you to build websites, publish books,
# generate interactive graphics and produce topic models.

# Many journalists use the RStudio software when working with R, which makes 
# it easy to visualize what has been created and to perform desktop publishing
# and other functions.

# We will be using the Swiss Army knife of R packages, tidyverse.
# Tidyverse comes with nine basic packages loaded with these strange nerd
# names such as dplyr and readr and ggplot2. We'll use parts of this
# package in each lesson.

# By loading this one package, you can:
# - Load data into R (readr)
# - Clean and reshape data before analysis (tidyr and dplyr)
# - Perform data analysis (dplyr)
# - Create data visualizations(ggplot2)

# To install any external packages, we use the function install.packages().
# Here's a little tricky part: You only need to install a library once, 
# the first time you set up your computer. Below, we will install Tidyverse 
# with the function install.packages('tidyverse'). You can type it directly 
# in the console.

# Install tidyverse (only need to run once)
install.packages("tidyverse")

# Here's what you need to remember: you need to load libraries into the 
# program's memory each time you use them. That's pretty standard for 
# programming languages in general; the same applies in Python, for example.

# Load tidyverse library (run each time you start R)
library(tidyverse)

# The tricky part: 
# - install.packages - once
# - load the library - each time you need it

# INSERT VIDEO 2
script: https://docs.google.com/document/d/1y4rAIoT2VGLgfRXHFoRENk5WyQaz_PVjP4um5pxWRdY/edit?tab=t.0
slides: https://docs.google.com/presentation/d/1OdsE65uJLBgxk0XxTaVsufdlWICow9eldM4CwnYkmm8/edit?slide=id.g35ef985f458_0_152#slide=id.g35ef985f458_0_152

# We'll explore Life Expectancy Data by country, 1800 to present with
# projections to 2100
# Source is gapminder.org: https://www.gapminder.org/data/
# Documentation: https://www.gapminder.org/data/documentation/gd004/

# Import Life Expectancy Data
# For your convenience, I have downloaded a slice of the data into a 
# comma separated values file, "lex.csv"

life_expect <- read.csv("./assets/lex.csv") 

# Exploratory data analysis
# Examine Data
glimpse(life_expect)

# File types:
# - <chr> = character, text data
# - <dbl> = numeric data
# 302 columns - Each year life expectancy data 
# 196 rows - each row is a country (195 counties)

# Dplyr verbs
# The authors of tidyverse describe dplyr as follows: "dplyr is a 
# grammar of data manipulation, providing a consistent set of verbs 
# that help you solve the most common data manipulation challenges
# 
# - Select - pick variables
# - Arrange - change order of rows
# - Filter - picks items based on values
# - Group_by - organizes data based on shared characteristics 
# - Summarize - used with addition, counting, average,  
# - Mutate - create column with new variables

# We will deploy these tools in the problems below

# Highest life expectancy 
# Which country has the highest life expectancy in 2025?

life_expect |> 
  select(country, X2025) |> 
  filter(X2025 == max(X2025, na.rm = TRUE))

# Japan and Singapore, both at 86 years!

# Pipe command
# You noticed an odd set of characters in the previous code: |> is a "pipe" 
# command, which links the code lines in logical order. (Your program may 
# display it as %>% - it's the same thing. You can change it to |> in R Studio 
# preferences: Tools | Global Options | Code | Use Native Pipe Operator). 
# 
# Think of the pipe command as telling R, "now add this command." So in the 
# previous code, we started with the data frame life_expect and told R to add 
# another command, select two columns, country and the year 2025. And then add 
# another command, filter 2025 by the maximum value.

# Using filters
# Filters can help us find needles in data haystacks. Let's use a filter
# to determine counties with the lowest life expectancy in 2025.

life_expect |> 
  select(country, X2025) |> 
  filter(X2025 == min(X2025, na.rm = TRUE))

# Obviously, we have a data problem, with Hong Kong and Liechtenstein
# registering at zero. Let's adjust our query for results greater than zero

life_expect |> 
  select(country, X2025) |> 
  # First filter out zeros and NA values
  filter(X2025 > 0, !is.na(X2025)) |>
  # Then find the minimum among remaining values
  filter(X2025 == min(X2025, na.rm = TRUE)) 

# What's the story here? Lesotho residents tend to live about 53 years,
# according to 2025 data from Gapminder.org, which is 61% as long as the
# highest life expectancy in Japan and Singapore of 86 years.

# Figure the percentage:
52.8/86

# Using Arrange
# Order the data in ascending, descending by specified variable.
# Here it is sorting alphabetically
life_expect |> 
  select(country, X2025) |> 
  arrange(country)

# Here is is sorting by 2025 life expectancy, descending
life_expect |> 
  select(country, X2025) |> 
  arrange(desc(X2025))

# Using means
# 
# Average life expectancy in 2025 The result, 73.1551, is stored in the
# variable "life2025"
life2025 <- life_expect |> 
  summarize(mean(X2025)) |> 
  pull() # this extracts the result as single numeric value

life2025

# Get all the data in one place

life_expect |> 
  summarize(mean_life = mean(X2025),
            median_life = median(X2025),
            min_life = min(X2025),
            max_life = max(X2025)
  )
#Average life expectancy in 1925


#Here we added na.rm = TRUE to allow the calculation to skip blank values
life1925 <- life_expect |> 
  summarise(mean_life = mean(X1925, na.rm = TRUE)) |> 
  pull() 

life1925



# **38 years was the average life expectancy in 1925!** Wow.

# Percentage change in life expectancy, 1925 v 2025 The numbers are stored
# in these variables, so just do the math


(life2025-life1925)/life1925 *100


# Average life expectancy has increased by about 92% since 1925.

#YOUR TURN

# Using the examples above, determine the life expectancy percentage
# change from 1975 to 2020.

#this portion is blank for the student. 
# The answer is 14.88097
 # life1975 <- life_expect |>
 #     summarise(mean(X1975, na.rm = TRUE)) |> 
 #    pull()
# 
# 
# life2020<- life_expect |>
#    summarise(mean(X2020, na.rm = TRUE)) |>
#     pull()
# #
# #
# (life2020-life1975)/life1975 *100



