# How Journalists Ask Questions of Data
# Digital Certificate Program
# Philip Merrill College of Journalism


# Lesson 5: Data Visualization Intro

#This lesson builds on the work from Lesson 2, 3 & 4. 
# Load tidyverse library 
library(tidyverse)
# Import Life Expectancy Data
life_expect <- read.csv("./assets/lex.csv") 

top <- life_expect |> 
  select(country, X2025, X1925) |> 
  # filter(country =="Afghanistan") |> 
  mutate(pct_change = round((X2025-X1925) / X1925*100,1)) |> 
  slice_max(pct_change, n= 10) 
top


# R has a variety of excellent data visualization tools. You can build maps, interactive graphics and many different types of static charts. You can also directly output to external tools such as Datawrapper, which you will see in a subsequent lesson.
# 
# To start, you're going to work with the "top" dataframe that we built earlier, that shows the top 10 countries by percentage change in life expectancy from 1925 to 2025.

ggplot(data=top) +
  geom_col(mapping=aes(x=pct_change, y=country)) 



# **That's ugly. Add some color: fill=n**
  
  
ggplot(data=top) +
  geom_col(mapping=aes(x=pct_change, y=country, fill=pct_change)) 



# **Where's the "pipe"?**
# In ggplot2, the command lines are linked by a + sign and not the pipe operator |>
# It's a quirk in R and we hope they fix it at some point. Don't hold your breath, however. Until then, use the + operator to link lines in ggplot2
# 
# 
# **Ditch the legend: theme(legend.position = "none")**
# 


ggplot(top,aes(x = pct_change, y = country,
             fill = pct_change)) +
  geom_col(position = "dodge") + 
  theme(legend.position = "none")



# **Add Headlines, Annotations and Credits: labs(title =**



ggplot(top,aes(x = pct_change, y = country,
             fill = pct_change)) +
  geom_col(position = "dodge") + 
  theme(legend.position = "none") +
#This is your title sequence
  labs(title = "Countries with biggest life expectancy gains, 1925-2025",
       subtitle = "Top 10 countries by percentage change",
       caption = "Source: Gapminder - https://www.gapminder.org/data/ Graphic by Rob Wells, 5-27-2025",
       y="Country",
       x="Pct Change")


# **Sorting a chart**
# The bars need to be sorted, highest to lowest. Use reorder()

ggplot(top, aes(x = pct_change, y = reorder(country, pct_change),
                fill = pct_change)) +
  geom_col(position = "dodge") + 
  theme(legend.position = "none") +
#This is your title sequence
  labs(title = "Countries with biggest life expectancy gains, 1925-2025",
       subtitle = "Top 10 countries by percentage change",
       caption = "Source: Gapminder - https://www.gapminder.org/data/ Graphic by Rob Wells, 5-27-2025",
       y="Country",
       x="Pct Change")