# libraries ---------------------------------------------------------------
library(tidyverse)
library(ggpage)
library(tidytext)
library(stringr)

# read in transcript ---------------------------------------------
miami_2nd_night_text <- read_csv("miami_2nd_night_text.csv")


# select only one candidate as first candidate
cand1 <- miami_2nd_night_text %>%
  filter(speaker == "HARRIS") %>% 
  select(speaker, text)


# tidy texting ------------------------------------------------------------
tidy_cand1 <- cand1 %>%
  unnest_tokens(word, text)

# remove stop words
data(stop_words)

tidy_cand1 <- tidy_cand1 %>%
  anti_join(stop_words)

cand1_word_count <- tidy_cand1 %>%
  count(word, sort = TRUE) %>%
  filter(!str_detect(word, "[0-9]")) # remove numbers


# CHOSE SECOND CANDIDATE ######### 
cand2 <- miami_2nd_night_text %>%
  filter(speaker == "BIDEN") %>% 
  select(speaker, text)


# tidy texting ------------------------------------------------------------
tidy_cand2 <- cand2 %>%
  unnest_tokens(word, text)

# remove stop words
# data(stop_words)

tidy_cand2 <- tidy_cand2 %>%
  anti_join(stop_words)

cand2_word_count <- tidy_cand2 %>%
  count(word, sort = TRUE) %>%
  filter(!str_detect(word, "[0-9]")) # remove numbers



### GRAPHING OUT DIFFERENCES ####

# following: https://www.tidytextmining.com/tidytext.html#word-frequencies

tidy_reports <- bind_rows(tidy_cand1, tidy_cand2) %>%
  filter(!str_detect(word, "[0-9]")) %>% 
  rename(report = speaker)

# following: https://www.tidytextmining.com/twitter.html#word-frequencies-1
raw_frequency <- tidy_reports %>%
  group_by(report) %>%
  count(word, sort = TRUE) %>%
  left_join(tidy_reports %>%
              group_by(report) %>%
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency <- raw_frequency %>%
  select(report, word, freq) %>%
  spread(report, freq) #%>%
  # arrange(Mueller, Watergate)


# plot relative frequencies -----------------------------------------------
library(scales)

freq_plot <- ggplot(frequency, aes(HARRIS, BIDEN)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  hrbrthemes::theme_ipsum_rc() +
  labs(title = "Word frequencies in Democratic Debate",
       subtitle = "Harris vs. Biden",
       caption = "by akessler")

freq_plot

ggsave("img/test1.jpg", freq_plot)
