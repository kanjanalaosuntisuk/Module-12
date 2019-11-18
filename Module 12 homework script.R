##### BAE590 Module 12 homework
##### Author: Kanjana Laosuntisuk
##### Date created: Nov 17, 2019
##### Last modified: Nov 17, 2019

# clear workspace and load packages 
rm(list=ls(all=TRUE))
install.packages("tidytext")
install.packages("wordcloud")
install.packages("textdata")

library(tidyverse)
library(tidytext)
library(wordcloud)
library(textdata)
library(lubridate)

# import tweets data
harvey_tweets <- read.csv("data/hurricane-harvey-tweets.csv")
str(harvey_tweets)
summary(harvey_tweets)

##### Question 1 -------------------
# change dataframe to tibble
harvey_tweets %>%
  as_tibble() -> harvey_tweets
str(harvey_tweets)
head(harvey_tweets)

# change date and datetime to date-time type
harvey_tweets$date <- as_date(harvey_tweets$date)
harvey_tweets$datetime <- ymd_hms(harvey_tweets$datetime)

# count number of tweets per datetime
harvey_tweets %>%
  group_by(datetime) %>%
  count() %>%
  ggplot(mapping = aes(x = datetime, y = n)) +
  geom_point(alpha = 0.5) +
  geom_vline(aes(xintercept = as.integer(as.POSIXct("2017-08-26 03:00:00"))), color = "red", size = 1) +
  labs(x = NULL,
       y = "Number of Tweets") +
  theme_bw()

##### Question 2 -------------------
# tokenize tweets to words
harvey_tweets %>%
  select(tweet) %>%
  mutate(line = 1:398777) %>%
  mutate(tweet = as.character(tweet)) %>%
  unnest_tokens(output = word, input = tweet, token = "words") -> harvey_tweets_words
str(harvey_tweets_words)

# remove stop words
data("stop_words")
custom_stop_words <- tibble(word = c("hurricane", "harvey", "hurricaneharvey", "http", "https", 
                                     "html", "ift.tt", "pic.twitter.com", "twitter.com", "fb.me", "bit.ly", 
                                     "dlvr.it", "youtube", "youtu.be"))
harvey_tweets_words %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) -> harvey_tweets_words_tidy

# count words and visualize the 20 most commonly used words in tweets
harvey_tweets_words_tidy %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  top_n(word, n = 20) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme_bw()

##### Question 3 -------------------
# subset tweets about refinery and refineries
refinery <- c("refinery", "refineries", "Refinery", "Refineries", "REFINERY", "REFINERIES")
refinery_match <- str_c(refinery, collapse = "|")
harvey_refinery <- str_subset(harvey_tweets$tweet, refinery_match)

# make a tibble of tweets about refinery
harvey_refinery_tb <- tibble(line = 1:1488, tweet = harvey_refinery)
head(harvey_refinery_tb)

# tokenize tweets to words
harvey_refinery_tb %>%
  unnest_tokens(output = word, input = tweet, token = "words") -> harvey_refinery_words
str(harvey_refinery_words)

# remove stop words
harvey_refinery_words %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) -> harvey_refinery_words_tidy

# make a wordcloud of refinery
harvey_refinery_words_tidy %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
##### From the wordcloud, most words from the tweets involving in refinery are related to economics (e.g price, oil, gas, gasoline, shut, business, cancellations) and there are a few words involving in environment (e.g. coast, gulf). However, there are some unclear words like web addresses in the cloud and people who tweeted them might put the links to the news related to refineries.

##### Question 4 -------------------
# tokenize tweets from each day to words
harvey_tweets %>%
  select(date, tweet) %>%
  mutate(tweet = as.character(tweet)) %>%
  unnest_tokens(output = word, input = tweet, token = "words") -> harvey_tweets_day_words

# remove stop words
harvey_tweets_day_words %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) ->  harvey_tweets_day_words_tidy

# join sentiments to words from each day
afinn <- get_sentiments("afinn")
harvey_tweets_day_words_tidy %>%
  inner_join(afinn) -> harvey_tweets_day_words_sentiments
head(harvey_tweets_day_words_sentiments)

# calculate mean of sentiments of words from each day
harvey_tweets_day_words_sentiments %>%
  group_by(date) %>%
  mutate(average = mean(x = value)) %>%
  select(date, average) %>%
  distinct() %>%
  ggplot(mapping = aes(x = date, y = average)) +
  geom_col() +
  scale_x_date(date_breaks = "day", date_labels = "%d") +
  labs(x = "Day in August 2017",
       y = "Average sentiment") +
  theme_bw()
