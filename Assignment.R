# How Journalists Ask Questions of Data
# Digital Certificate Program
# Philip Merrill College of Journalism


# ASSIGNMENT

# Examine life expectancy for 1800, 1900, 2000. Determine the mean values
# for each of those three centuries. Using mutate, create a new category
# column for each decade assigning the counties that are above and below
# the median value. Compile the three decade into a single dataframe
# Produce a single dataframe that counts the number above and below the
# median, and calculate a percentage of the whole for only those countries
# with reported data
# 
# Part 1: Calculate median values by time period


#this portion is blank for the student. 
# life1800 <- life_expect |>
#    summarise(mean(X1800, na.rm = TRUE)) |> 
#   pull() # this extracts the result as single numeric value
# 
# life1900 <- life_expect |>
#    summarise(mean(X1900, na.rm = TRUE)) |> 
#   pull()
# 
# life2000 <- life_expect |>
#    summarise(mean(X2000, na.rm = TRUE)) |> 
#   pull()

# Part 2: Categorize by time period above / below median


#this portion is blank for the student. 
# 
# life_1800 <- life_expect |>
#   select(country, X1800) |>
#   mutate(category = case_when(
#          X1800 > life1800 ~ "above",
#           X1800 < life1800 ~ "below")
# )
# 
# life_1900 <- life_expect |>
#   select(country, X1900) |>
#   mutate(category = case_when(
#          X1900 > life1900 ~ "above",
#           X1900 < life1900 ~ "below")
# )
# 
# life_2000 <- life_expect |>
#   select(country, X2000) |>
#   mutate(category = case_when(
#          X2000 > life2000 ~ "above",
#           X2000 < life2000 ~ "below")
# )

# Part 3: Compile

#this portion is blank for the student. 
# life_1800 <- life_1800|>
#   rename(value = X1800)|>
#   mutate(year = 1800)
# 
# life_1900 <- life_1900|>
#   rename(value = X1900)|>
#   mutate(year = 1900)
# 
# life_2000 <- life_2000|>
#   rename(value = X2000)|>
#   mutate(year = 2000)
# 
# total1800_2000 <- rbind(life_1800, life_1900, life_2000)

# Part 4: Calculate totals, percentages

#this portion is blank for the student. 
# total1800_2000 |> 
#   group_by(year) |> 
#  count(category) |> 
#  filter(!is.na(category)) |> #drops the counties without data
#  mutate(percent = n/sum(n)) |> 
#  mutate(percent = round(percent*100, 1))


# Answer:
# year. category n %
# 1800	above	95	51.1	
# 1800	below	91	48.9	
# 1900	above	81	43.5	
# 1900	below	105	56.5	
# 2000	above	115	58.7	
# 2000	below	81	41.3	

#END OF ASSIGNMENT
