# https://www.tidytextmining.com/tfidf.html

### DOING SOME TF-IDF ANALYSIS OF DEMOCRATIC DEBATE ####

# libraries ---------------------------------------------------------------
library(tidyverse)
library(ggpage)
library(tidytext)
library(stringr)

# read in transcript ---------------------------------------------
miami_2nd_night_text <- read_csv("miami_2nd_night_text.csv")

selectedcols <- miami_2nd_night_text %>%
  select(speaker, text)


speaker_words <- selectedcols %>%
  unnest_tokens(word, text) %>%
  count(speaker, word, sort = TRUE)

total_words <- speaker_words %>% 
  group_by(speaker) %>% 
  summarize(total = sum(n))

speaker_words <- left_join(speaker_words, total_words)

speaker_words
