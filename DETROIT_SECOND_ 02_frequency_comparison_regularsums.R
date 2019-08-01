### DOING TEXT ANALYSIS OF CNN DEMOCRATIC DEBATE ####

# libraries ---------------------------------------------------------------
library(tidyverse)
library(ggpage)
library(tidytext)
library(stringr)
library(plotly)


# read in transcript ---------------------------------------------
detroit_text <- read_csv("data/detroit_2nd_night_text.csv")

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


#pull top ones for each speaker into table
top_word_per_speaker <- speaker_words %>%
  select(-total) %>% 
  group_by(speaker) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup

#save to file
write_csv(top_word_per_speaker, "output/detroit_night2_top_singleword_percandidate.csv")


speaker_words %>% 
  filter(word == "russia")


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


#pull top ones for each speaker into table
top_bigrams_per_speaker <- speaker_bigrams %>%
  group_by(speaker) %>%
  top_n(15) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup

#save to file
write_csv(top_bigrams_per_speaker, "output/detroit_night2_top_bigrams_percandidate.csv")





#visualizing bi-grams ####
speaker_tfidf_chart_bigrams <- speaker_bigrams_tfidf %>%
  group_by(speaker) %>%
  top_n(10) %>%
  slice(1:10) %>% #added to limit to 10 records per cand, even with ties 
  ungroup %>%
  mutate(speaker = as.factor(speaker),
         bigram = reorder_within(bigram, tf_idf, speaker)) %>% #use the new reorder_within() func
  ggplot(aes(bigram, tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~speaker, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered() # +
# scale_y_continuous(expand = c(0,0))

speaker_tfidf_chart_bigrams 

#save chart images to file
ggsave("img/detroit1_speaker_tfidf_chart_bigrams.jpg", speaker_tfidf_chart_bigrams)
ggsave("img/detroit1_speaker_tfidf_chart_bigrams.pdf", speaker_tfidf_chart_bigrams)
