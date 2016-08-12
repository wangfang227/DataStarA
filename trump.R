#install.packages(c("purrr","twitteR"))

library(dplyr)
library(purrr)
library(twitteR)

setup_twitter_oauth(getOption("twitter_consumer_key"))

getOption("twitter_consumer_secret")
getOption("twitter_access_token")
getOption("twitter_access_token_secret"))

trump_tweets <- userTimeline("realDonaldTrump", n = 3200)
trump_tweets_df <- tbl_df(map_df(trump_tweets, as.data.frame))

load(url("http://varianceexplained.org/files/trump_tweets_df.rda"))

install.packages("tidyr")
library(tidyr)

tweets <- trump_tweets_df %>%
  select(id, statusSource, text, created) %>%
  extract(statusSource, "source", "Twitter for (.*?)<") %>%
  filter(source %in% c("iPhone", "Android"))

#install.packages(c("lubridate","scales","ggplot2"))

library(lubridate)
library(scales)
library(ggplot2)

tweets %>%
  count(source, hour = hour(with_tz(created, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

