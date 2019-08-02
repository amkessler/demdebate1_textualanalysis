### DOING TF-IDF ANALYSIS OF CNN DEMOCRATIC DEBATE ####

# libraries ---------------------------------------------------------------
library(tidyverse)
library(ggpage)
library(tidytext)
library(stringr)
library(plotly)


# read in transcript ---------------------------------------------
detroit_text <- read_csv("data/detroit_COMBINED_text.csv")

selectedcols <- detroit_text %>%
  select(speaker, text)


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



#begin the text analysis ---------------------------------------------

speaker_words <- selectedcols %>%
  unnest_tokens(word, text) %>%
  count(speaker, word, sort = TRUE)

total_words <- speaker_words %>% 
  group_by(speaker) %>% 
  summarize(total = sum(n))

speaker_words <- left_join(speaker_words, total_words)

speaker_words

# remove stop words
data(stop_words)

speaker_words <- speaker_words %>%
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[0-9]")) # remove numbers



### NOW WE'LL DO THE TF-IDF ---------------------------------------------

speaker_words <- speaker_words %>%
  bind_tf_idf(word, speaker, n)

speaker_words

# idf and thus tf-idf are zero for these extremely common words, so the idf term (which will then be the
# natural log of 1) is zero. The inverse document frequency (and thus tf-idf) is very low (near zero) for words that 
# occur in many of the documents in a collection; this is how this approach decreases the weight for common words. 
# The inverse document frequency will be a higher number for words that occur in fewer of the documents in the 
# collection.

speaker_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))


#pull top ones for each speaker into table
top_tfidf_per_speaker <- speaker_words %>%
  group_by(speaker) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup

#save to file
write_csv(top_tfidf_per_speaker, "output/detroit_combined_tfidf_singleword_percandidate.csv")



### NOW BI-GRAMS VERSION -------------------------------

speaker_bigrams <- selectedcols %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(speaker, bigram, sort = TRUE)

total_words <- speaker_bigrams %>% 
  group_by(speaker) %>% 
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
  count(speaker, word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

#saved back to original name
speaker_bigrams <- bigrams_united


# TF-IDF #### ---

speaker_bigrams_tfidf <- speaker_bigrams %>%
  bind_tf_idf(bigram, speaker, n)

speaker_bigrams_tfidf

speaker_bigrams_tfidf %>%
  select(-total) %>%
  arrange(desc(tf_idf)) 

#limit number of candidates
# speaker_bigrams_selectedcands <- speaker_bigrams_tfidf %>% 
#   filter(speaker %in% mycands)

#pull top ones for each speaker into table
top_tfidf_per_speaker_bigrams <- speaker_bigrams_tfidf %>%
  group_by(speaker) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup

#save to file
write_csv(top_tfidf_per_speaker_bigrams, "output/detroit_combined_tfidf_bigrams_percandidate.csv")


