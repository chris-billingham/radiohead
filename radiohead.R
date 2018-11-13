library(tidyverse)
library(stringr)
library(magrittr)
library(tidytext)
library(textstem)

load(file = "radiohead_songs.rda")
radiohead_songs$lyrics %<>% iconv("UTF-8", sub = '')
radiohead_songs$lyrics %<>% tolower()
radiohead_songs$lyrics <- gsub("[^[A-Za-z] ]", "", radiohead_songs$lyrics)
radiohead_songs$lyrics <- lemmatize_strings(radiohead_songs$lyrics)

# keep albums and songs in order
radiohead_songs$album %<>% factor(., as.character(unique(.)))
radiohead_songs$song %<>% factor(., as.character(unique(.)))

# split out unigrams
radio_uni <- radiohead_songs %>%
  unnest_tokens(word, lyrics)

# sentiment but leave gaps for the zeros
radio_uni_sent <- radio_uni %>% 
  left_join(get_sentiments("afinn"))

# replace NA with zero
radio_uni_sent$score <- ifelse(is.na(radio_uni_sent$score),0,radio_uni_sent$score)

# group by album then add a position in song number and rolling sum
radio_uni_sent %<>% 
  group_by(album) %>% 
  mutate(pos = row_number()) %>% 
  mutate(rollsum=cumsum(score))

# let's have a look
radio_uni_sent %>% ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ album, scales = "free") +
  ggtitle("Radiohead Cumulative Sentiment per Album") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album") +
  ggsave("plots/radiohead_all.png")


# what's going on with Kid A
radio_uni_sent %>% 
  filter(album == "Kid A") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ song, scales = "free") +
  ggtitle("Kid A Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album") +
  ggsave("plots/kid_a.png")

# let's check the rest of the albums
radio_uni_sent %>% 
  filter(album == "Pablo Honey") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ song, scales = "free") +
  ggtitle("Pablo Honey Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album") + 
  ggsave("plots/pablo_honey.png")

radio_uni_sent %>% 
  filter(album == "The Bends") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ song, scales = "free") +
  ggtitle("The Bends Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album") +
  ggsave("plots/the_bends.png")

radio_uni_sent %>% 
  filter(album == "OK Computer") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ song, scales = "free") +
  ggtitle("OK Computer Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album") +
  ggsave("plots/ok_computer.png")

radio_uni_sent %>% 
  filter(album == "Amnesiac") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ song, scales = "free") +
  ggtitle("Amnesiac Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album") +
  ggsave("plots/amnesiac.png")

radio_uni_sent %>% 
  filter(album == "Hail To The Thief") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ song, scales = "free") +
  ggtitle("Hail to the Thief Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album") +
  ggsave("plots/hail_to_the_thief.png")

radio_uni_sent %>% 
  filter(album == "In Rainbows") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ song, scales = "free") +
  ggtitle("In Rainbows Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album") +
  ggsave("plots/in_rainbows.png")

radio_uni_sent %>% 
  filter(album == "The King of Limbs") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ song, scales = "free") +
  ggtitle("The King of Limbs Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album") +
  ggsave("plots/king_of_limbs.png")

radio_uni_sent %>% 
  filter(album == "A Moon Shaped Pool") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ song, scales = "free") +
  ggtitle("A Moon Shaped Pool Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album") +
  ggsave("plots/moon_shaped_pool.png")

