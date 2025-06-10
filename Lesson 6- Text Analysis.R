# How Journalists Ask Questions of Data
# Howard Center Data Journalism Training Program
# Philip Merrill College of Journalism


# Lesson 6: Text Analysis

# Slides for this lesson: https://docs.google.com/presentation/d/15TMr1oak6tYda-MA0-y4h340DtCuS6ImwHrYNnTiOjI/edit?usp=sharing


# Text Analysis
# 
# R can also help you find gems in big stacks of documents. We will work with a lengthy document to learn common phrases. In this case, it's a 62-page transcript of the year-end news conference by Russian President Vladimir Putin, held of Dec. 26, 2024.
# To do this work, you will install tidytext, a powerful program that prepares text for machine learning and natural language processing.


install.packages("tidytext")
library(tidytext)
# Load tidyverse library 
library(tidyverse)

# To speed thing up, I downloaded the transcript from the BBC and processed it into a raw text file, called putin.txt
# Processing the text data

#This reads the text file into R.
putin <- read_lines("./assets/putin.txt") 
#This converts the file into a dataframe, one column, 3,624 rows. Man, that guy can talk!
putin_df <- tibble(putin,) 


# **Tokenize data**
# This process takes a sentence and makes it one row per word. Using the previous sentence, tokenization will do the following:
# 1 This
# 2 process
# 3 takes
# 4 a
# 5 sentence
# 6 and 
# 7 makes
# 8 it
# 9 one
# 10 row
# 11 per
# 12 word.

putin_tokenized <- putin_df %>%
  unnest_tokens(word,putin)

head(putin_tokenized, 20)

# So tokenizing this 62-page transcript yields a dataframe with 37,007 rows, one line per word. Putting the transcript into such a structured data format allows R to perform many computations on the text such as counting the frequency of word pairs, or bigrams. 
# 
# **Word Count**
#
putin_word_ct <- putin_tokenized %>%
  count(word, sort=TRUE)

head(putin_word_ct, 20)
#

# **Remove stopwords**
#   We can see from the results above that the program correctly counted "the" as the most frequently used word. Great, but that doesn't give us much insight into what Putin said.
# 
# We can cut the articles, pronouns and other words to get to the meat of the text.
# The tidytext package includes the stop_words dataset. It contains, as of this writing, 1,149 words that data scientists and linguistic nerds felt could be removed from sentences because they don't add meaning. Filtering out these words can help focus on the more meaningful content, making it easier to uncover trends, themes, and key information in large amounts of text. Obviously, we have different priorities and we may or may not want to use stop_words or we have want to provide a customized list of stop words.
# 
# The stop_words list is derived from three separate lists, or lexicons: SMART (571 words), onix (404 words), and snowball (174 words)
# 
# The ONIX lexicon comes from the Open Information Exchange and is often used in text mining and natural language processing. 
# 
# The Snowball lexicon is part of a broader project that has algorithms that simplify words in different languages by reducing them to their root form. It's best known for the Porter stemming algorithm, which, for example, changes "running" to "run." 
# 
# Lastly, the SMART lexicon is a set of common words, like "and," "the," and "is," and it comes from the SMART Information Retrieval System, created at Cornell University in the 1960s.
# 
# #
data(stop_words)

test <- stop_words %>% 
  as.data.frame()

head(test)
#

# **Remove stopwords**
#

data(stop_words)

putin_tokenized_clean <- putin_tokenized %>%
  anti_join(stop_words, by = c("word" = "word")) %>%
  filter(word != "temp_file") %>%
  filter(word != "stories_corpus") %>%
  filter(!grepl('[0-9]', word))

# Word Count

putin_word_ct_clean <- putin_tokenized_clean %>%
  count(word, sort=TRUE)

#

#YOUR TURN
# Compare the putin_word_ct_clean to the putin_word_ct. Write three sentences comparing the two and provide examples



# **Bigrams**
# 
# Now we will count the most common two-word phrases, or bigrams, in the Putin press conference transcript. 
# 
#
bigrams <- putin_df  |> 
  unnest_tokens(bigram, putin, token="ngrams", n=2) |> 
    separate(bigram, c("word1", "word2"), sep = " ")

#Filter out stop words.
bigrams_cleaned <- bigrams |> 
  anti_join(stop_words, by = c("word1" = "word"))  |> #filters out words based on the stop_word list
  anti_join(stop_words, by = c("word2" = "word")) |> 
  filter(!is.na(word1)) |> #eliminates NA values
  count(word1, word2, sort = TRUE)
#

# A little formatting for later
#
bigrams_cleaned <- bigrams_cleaned |> 
  mutate(bigram = (paste0(word1, " ", word2))) |> #rejoins the phrases into a single column
  select(bigram, n, -word1, -word2) #eliminates the separate columns, reorders

head(bigrams_cleaned, 20)
#

# We can see from the results that Vladimir Putin is the most common phrase in the transcript, which isn't surprising. But look through the results and you can see the War Against Ukraine is the most prominent topic: "special military" and "military operation" are in the top 10 results.

<br>
  
  # YOUR TURN
  
  # Construct a ggplot chart with the top 25 phrases from the Putin press conference. Display the results in descending order using reorder()

#
#this portion is blank for the student. 
#putin_25 <- head(bigrams_cleaned,25)

# ggplot(putin_25,aes(x = n, y = reorder(bigram, n), 
#              fill = n)) +
#   geom_col(position = "dodge") + 
#   theme(legend.position = "none") +
# #This is your title sequence
#   labs(title = "Top phrases from Putin's 2024 press conference",
#        subtitle = "Bigrams from 62-page transcript",
#        caption = "Source: BBC Graphic by Rob Wells, 5-27-2025",
#        y="Bigram",
#        x="Count")

#
