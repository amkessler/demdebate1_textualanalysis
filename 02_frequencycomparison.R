### DOING TF-IDF ANALYSIS OF DEMOCRATIC DEBATE ####

# libraries ---------------------------------------------------------------
library(tidyverse)
library(ggpage)
library(tidytext)
library(stringr)
library(plotly)


# read in transcript ---------------------------------------------
miami_2nd_night_text <- read_csv("data/miami_2nd_night_text.csv")

selectedcols <- miami_2nd_night_text %>%
  select(speaker, text)

#exclude moderators
selectedcols <- selectedcols %>% 
  filter(!speaker %in% c("ANNOUNCER",
                        "DIAZ-BALART",
                        "GUTHRIE",
                        "HOLT",
                        "TODD",
                        "MADDOW"))


#vector of selected candidates to include in final charts/analysis
mycands <- c("GILLIBRAND",
  "BIDEN",
  "BUTTIGIEG",
  "HARRIS",
  "SANDERS",
  "WILLIAMSON")



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

# Notice that idf and thus tf-idf are zero for these extremely common words, so the idf term (which will then be the
# natural log of 1) is zero. The inverse document frequency (and thus tf-idf) is very low (near zero) for words that 
# occur in many of the documents in a collection; this is how this approach decreases the weight for common words. 
# The inverse document frequency will be a higher number for words that occur in fewer of the documents in the 
# collection.

speaker_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#filter for only certain candidates
speaker_words_selectedcands <- speaker_words %>% 
  filter(speaker %in% mycands) 

#visualizing 
speaker_tfidf_chart <- speaker_words_selectedcands %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(speaker) %>% 
  top_n(10) %>% 
  slice(1:10) %>% #added SLICE to limit to 10 records per cand, even with ties 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~speaker, ncol = 2, scales = "free") +
  coord_flip()

speaker_tfidf_chart 

ggsave("img/speaker_tfidf_chart.jpg", speaker_tfidf_chart)
ggsave("img/speaker_tfidf_chart.pdf", speaker_tfidf_chart)



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


# TF-IDF 

speaker_bigrams <- speaker_bigrams %>%
  bind_tf_idf(bigram, speaker, n)

speaker_bigrams

speaker_bigrams %>%
  select(-total) %>%
  arrange(desc(tf_idf)) 

#visualizing 
speaker_tfidf_top <- speaker_bigrams %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(speaker) %>% 
  top_n(10) %>% 
  slice(1:10) %>% #added SLICE to limit to 10 records per cand, even with ties 
  ungroup() 

#check count per speaker
speaker_tfidf_top %>% 
  count(speaker)

#limit number of candidates
speaker_tfidf_top <- speaker_tfidf_top %>% 
  filter(speaker %in% mycands)

speaker_tfidf_chart_bigrams <- speaker_tfidf_top %>% 
  ggplot(aes(bigram, tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~speaker, ncol = 2, scales = "free") +
  coord_flip() 

speaker_tfidf_chart_bigrams 

#save chart images to file
ggsave("img/speaker_tfidf_chart_bigrams.jpg", speaker_tfidf_chart_bigrams)
ggsave("img/speaker_tfidf_chart_bigrams.pdf", speaker_tfidf_chart_bigrams)
