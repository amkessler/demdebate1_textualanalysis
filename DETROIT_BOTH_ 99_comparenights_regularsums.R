### DOING TEXT ANALYSIS OF CNN DEMOCRATIC DEBATE ####

# libraries ---------------------------------------------------------------
library(tidyverse)
library(ggpage)
library(tidytext)
library(stringr)
library(plotly)


# read in transcript ---------------------------------------------
detroit_text <- read_csv("data/detroit_COMBINED_text.csv")

selectedcols <- detroit_text %>%
  select(night, speaker, text)


#list the individual speakers
selectedcols %>% 
  count(speaker)


#exclude moderators
selectedcols <- selectedcols %>% 
  filter(!speaker %in% c("ANNOUNCER",
                        "BASH",
                        "LEMON",
                        "TAPPER",
                        "PROTESTOR"))


#vector of selected candidates to include in final charts/analysis
# mycands <- c("GILLIBRAND",
#   "BIDEN",
#   "BUTTIGIEG",
#   "HARRIS",
#   "SANDERS",
#   "WILLIAMSON")



#begin the text analysis for NIGHTLY words---------------------------------------------

speaker_words <- selectedcols %>%
  unnest_tokens(word, text) %>%
  count(night, word, sort = TRUE)

total_words <- speaker_words %>% 
  group_by(night) %>% 
  summarize(total = sum(n))

speaker_words <- left_join(speaker_words, total_words)

speaker_words

# remove stop words
data(stop_words)

speaker_words <- speaker_words %>%
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]")) # remove numbers


#pull top ones for each night into table
top_word_per_speaker <- speaker_words %>%
  select(-total) %>% 
  group_by(night) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup

#save to file
write_csv(top_word_per_speaker, "output/detroit_nightvsnight_top_singlewords.csv")


speaker_words %>% 
  filter(word == "trump")


### NOW BI-GRAMS VERSION -------------------------------

speaker_bigrams <- selectedcols %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(night, bigram, sort = TRUE)

total_words <- speaker_bigrams %>% 
  group_by(night) %>% 
  summarize(total = sum(n))

speaker_bigrams <- left_join(speaker_bigrams, total_words)

speaker_bigrams

#remove stop words as either of the two words
bigrams_separated <- speaker_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!str_detect(word1, "[0-9]")) %>% 
  filter(!str_detect(word2, "[0-9]")) # remove numbers# remove numbers

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(night, word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

#saved back to original name
speaker_bigrams <- bigrams_united


#pull top ones for each speaker into table
top_bigrams_per_speaker <- speaker_bigrams %>%
  group_by(night) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup

#save to file
write_csv(top_bigrams_per_speaker, "output/detroit_nightvsnight_top_bigrams.csv")
