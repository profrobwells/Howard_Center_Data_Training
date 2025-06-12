# How Journalists Ask Questions of Data
# Howard Center Data Journalism Training Program
# Philip Merrill College of Journalism


# Lesson 5: Data Visualization Intro

# Slides for this talk: https://docs.google.com/presentation/d/1ZCEnbwKs7IoXahUuRi6k_lJDo0RE8kRFbgR2Yvxv1Q0/edit?slide=id.g35ef985f458_0_84#slide=id.g35ef985f458_0_84


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


Your Turn
Create a new dataframe called bottom with the 10 lowest life expectancy rates 
by percentage change for 1950 and 2020. Filter out any results that have zero values in 2000.
Then create a ggplot chart with a proper headline
)

bottom <- life_expect |> 
  select(country, X1950, X2020) |> 
  mutate(pct_change = round((X2020-X1950) / X1950*100,1)) |> 
  filter(pct_change > 0) |> 
  slice_min(pct_change, n= 10) 


ggplot(bottom, aes(x = pct_change, y = reorder(country, pct_change),
                fill = pct_change)) +
  geom_col(position = "dodge") + 
  theme(legend.position = "none") +
  #This is your title sequence
  labs(title = "Countries with lowest life expectancy gains, 1950-2020",
       subtitle = "10 countries with lowest percntage change in life expectancy",
       caption = "Source: Gapminder - https://www.gapminder.org/data/ Graphic by Rob Wells, 5-27-2025",
       y="Country",
       x="Pct Change")