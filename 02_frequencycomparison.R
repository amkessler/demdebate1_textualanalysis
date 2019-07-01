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


#graph result
ggplot(speaker_words, aes(n/total, fill = speaker)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~speaker, ncol = 2, scales = "free_y")


#zipf's law
freq_by_rank <- speaker_words %>% 
  group_by(speaker) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

#plot
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = speaker)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

#broken power law
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = speaker)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()



### NOW WE'LL DO TF-IDF #####
speaker_words <- speaker_words %>%
  bind_tf_idf(word, speaker, n)

speaker_words

# Notice that idf and thus tf-idf are zero for these extremely common words. These are all words that appear in all six of 
# Jane Austen’s novels, so the idf term (which will then be the natural log of 1) is zero. The inverse document frequency 
# (and thus tf-idf) is very low (near zero) for words that occur in many of the documents in a collection; this is how 
# this approach decreases the weight for common words. The inverse document frequency will be a higher number for words 
# that occur in fewer of the documents in the collection.

speaker_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))


#visualizing 
speaker_tfidf_chart <- speaker_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(speaker) %>% 
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~speaker, ncol = 2, scales = "free") +
  coord_flip()

speaker_tfidf_chart

ggsave("img/speaker_tfidf_chart.jpg", speaker_tfidf_chart)
