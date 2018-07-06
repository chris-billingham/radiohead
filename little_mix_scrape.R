options(stringsAsFactors = FALSE)
library(tidyverse)
library(geniusR)

DNA_lyrics <- genius_album("little mix", "DNA")
salute_lyrics <- genius_album("little mix", "salute")
getweird_lyrics <- genius_album("little mix", "get weird")
glorydays_lyrics <- genius_album("little mix", "glory days")

DNA_lyrics_all <- DNA_lyrics %>% 
  group_by(track_title) %>% 
  mutate(lyrics = paste(lyric, collapse = " ")) %>%
  select(track_title, lyrics) %>%
  unique()
DNA_lyrics_all$album <- "DNA"

salute_lyrics_all <- salute_lyrics %>% 
  group_by(track_title) %>% 
  mutate(lyrics = paste(lyric, collapse = " ")) %>%
  select(track_title, lyrics) %>%
  unique()
salute_lyrics_all$album <- "Salue"

getweird_lyrics_all <- getweird_lyrics %>% 
  group_by(track_title) %>% 
  mutate(lyrics = paste(lyric, collapse = " ")) %>%
  select(track_title, lyrics) %>%
  unique()
getweird_lyrics_all$album <- "Get Weird"

glorydays_lyrics_all <- glorydays_lyrics %>% 
  group_by(track_title) %>% 
  mutate(lyrics = paste(lyric, collapse = " ")) %>%
  select(track_title, lyrics) %>%
  unique()
glorydays_lyrics_all$album <- "Glory Days"

little_mix_lyrics <- bind_rows(DNA_lyrics_all, salute_lyrics_all, getweird_lyrics_all, glorydays_lyrics_all)

saveRDS(little_mix_lyrics, "littlemix.rds")
