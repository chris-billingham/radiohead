options(stringsAsFactors = FALSE)
library(tidyverse)

# first let's scrape wikipedia for the list of Radiohead Studio albums
library(rvest)
url <- "https://en.wikipedia.org/wiki/Radiohead_discography#Studio_albums"

# get the list of full studio albums for radiohead from wikipedia
radiohead_albums <- read_html(url) %>% 
  html_nodes("table.wikitable.plainrowheaders") %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>%
  slice(2:(n()-1)) %>%
  .[,1] %>%
  tolower()

# now we're going to use the geniusR package and get all the lyrics in a list
library(geniusR)

# annoyingly genius_album doesn't return the album name so i need to supplement this
genius_album_plus <- function(artist, album) {
  df <- genius_album(artist, album) %>%
    mutate(album = album)
  return(df)
}

# let's use this amended function to create a single datafame
radiohead_lyrics <- radiohead_albums %>%
  map2_dfr("radiohead", ., genius_album_plus)

# we'll need this for later
radiohead_albums_df <- data_frame(album = radiohead_albums) %>%
  mutate(album_no = seq(1, length(radiohead_albums)))

# let's look at rolling sentiment
library(tidytext)

radiohead_uni_sent <- radiohead_lyrics %>% 
  unnest_tokens(word, lyric) %>% 
  left_join(get_sentiments("afinn")) %>%
  mutate(score = ifelse(is.na(score), 0, score)) %>%
  left_join(radiohead_albums_df) %>%
  group_by(album) %>%
  mutate(pos = row_number()) %>% 
  mutate(rollsum = cumsum(score))

# initial visual on total album sentiment
radiohead_uni_sent %>%
  group_by(album_no, album) %>%
  summarise(album_sentiment = sum(score)) %>%
  ggplot(aes(fct_reorder(album, album_no), album_sentiment)) +
    geom_col() +
    ggtitle("Radiohead Sentiment per Album") +
    xlab("Album name ordered by release date") +
    ylab("Cumulative Sentiment per Album") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# tres interesting, definitely more negs than pos
# let's look at how the sentiment builds through the albums
radiohead_uni_sent %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ fct_reorder(album, album_no), scales = "free") +
  ggtitle("Radiohead Cumulative Sentiment per Album") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album")

# some odd stuff going on with the bends, kid a and hail to the thief
# let's look at total sentiment for all songs and order
all_songs <- radiohead_uni_sent %>%
  group_by(album_no, album, track_n, track_title) %>%
  summarise(total_sentiment = sum(score)) %>%
  arrange(desc(total_sentiment))

head(all_songs)
tail(all_songs)

# let's look at nice dream
radiohead_uni_sent %>% 
  filter(track_title == "(Nice Dream)") %>%
  group_by(word) %>%
  summarise(n = n(),
            total_sent = sum(score)) %>%
  arrange(desc(total_sent))

# by using the lyric "nice dream" 17 times it adds 68 to total sentiment for the song
# seems the clue is in the track title!
# is it a similar story with "optimistic" in kid a?
radiohead_uni_sent %>% 
  filter(track_title == "Optimistic") %>%
  group_by(word) %>%
  summarise(n = n(),
            total_sent = sum(score)) %>%
  arrange(desc(total_sent))

# yes broadly, using best 17 times and good 5 gave the song 66
# both very optimisitic words!
# so for each of these outliers we have repeated "good" sentiment words being used
# let's look at the bottom two
radiohead_uni_sent %>% 
  filter(track_title == "A Punchup at a Wedding" | track_title == "A Wolf at the Door") %>%
  group_by(track_title, word) %>%
  summarise(n = n(),
            total_sent = sum(score)) %>%
  arrange(total_sent)

# interestingly both tracks are driven by repeated instances of the word "no"

# let's have a look at hail to the thief visually
radiohead_uni_sent %>% 
  filter(album == "hail to the thief") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ fct_reorder(track_title, track_n), scales = "free_x") +
  ggtitle("Hail to the Thief Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album")

# so actually you could argue hail to the thief is pretty neutral
# but those two tracks really bring the sentiment down
# let's play a game, let's stop the repeated lyric "trick"
# what happens if we remove the top 2 and bottom 2 to overall radiohead 
# sentiments across the albums
radiohead_uni_sent %>%
  filter(track_title != "(Nice Dream)" & track_title != "Optimistic" & 
           track_title != "A Punchup at a Wedding" & track_title != "A Wolf at the Door") %>%
  group_by(album_no, album) %>%
  summarise(album_sentiment = sum(score)) %>%
  ggplot(aes(fct_reorder(album, album_no), album_sentiment)) +
  geom_col() +
  ggtitle("Radiohead Sentiment per Album",
          subtitle = "Minus top 2 and bottom 2 sentiment tracks") +
  xlab("Album name ordered by release date") +
  ylab("Cumulative Sentiment per Album") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# so overall pretty gloomy since their first album

# now, let's brighten things up a bit
