library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
options(scipen = 999)

data <- read_excel("data/debate_tweets_hashtag_demdebatesowhite.xlsx", 
                                                     col_types = c("text", "date", "text", 
                                                                   "text", "numeric", "date", "text", 
                                                                   "text"))

data <- data %>% 
  clean_names() %>% 
  remove_empty(c("cols", "rows"))

data

data %>% 
  count(retweeted)

data %>% 
  count(screen_name) %>% 
  arrange(desc(n))
