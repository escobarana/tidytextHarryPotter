library(devtools)
devtools::install_github("bradleyboehmke/harrypotter")
library(harrypotter)
library(tidytext)
library(plyr)
library(tidyverse)
library(wordcloud)
library(stringr)
library(dplyr)
library(reshape2)
library(igraph)
library(ggraph)
library(textdata)


# Data Shaping ------------------------------------------------------------

# Create a vector with each title of the Harry Potter series
titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

# Create a list containing the books, each book is an array in which each value
# in the array is a chapter
books <- list(harrypotter::philosophers_stone, harrypotter::chamber_of_secrets, harrypotter::prisoner_of_azkaban,
              harrypotter::goblet_of_fire, harrypotter::order_of_the_phoenix, harrypotter::half_blood_prince,
              harrypotter::deathly_hallows)

# Place all of the books in the Harry Potter series into a tibble
series <- tibble()

for(i in seq_along(titles)) {
  
  temp <- tibble(chapter = seq_along(books[[i]]),
                 text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    # Tokenize each chapter into words, strip away all punctuation and capitalization
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  # Add columns to the tibble for the book and chapter
  series <- rbind(series, temp)
}
# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))
series

# Get simple counts for each word using the count function
series %>% count(word, sort = TRUE)

# Discard 'stop words' (the, and, to, ...)
series %>% 
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


# Sentiment Analysis ------------------------------------------------------

# Continue working with the text that contains stop-words
# assess how positive or negative text is, based on a dictionary of words that
# have been previously classified as positive or negative.
series %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

# Using the 'bing' lexicon to only classify words as positive or negative
series %>%
  right_join(get_sentiments("bing")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

# Make a comparison cloud using the 'bing' lexicon
series %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 50)
# Comparison cloud with stop-words removed temporarily
series %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 50)

# Let’s look at pairs of words (bigrams). A bigram is a pair of words that 
# appear consecutively in a text.
series <- tibble()
for(i in seq_along(titles)) {
  
  temp <- tibble(chapter = seq_along(books[[i]]),
                 text = books[[i]]) %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    ##Here we tokenize each chapter into bigrams
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, temp)
}
# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))
series
# Use the count function to find the most common bigrams in the series
series %>% count(bigram, sort = TRUE)

# Remove stop-words from the bigram tibble
bigrams_separated <- series %>% separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>% filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
# new bigram counts:
bigrams_united <- bigrams_filtered %>% unite(bigram, word1, word2, sep = " ")
bigrams_united %>% count(bigram, sort = TRUE)

# Display the ten bigrams with the highest tf-idf scores among the seven books in the series.
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

# graphs
plot_potter<- bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram))))
plot_potter %>% 
  top_n(20) %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()
