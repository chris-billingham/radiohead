options(stringsAsFactors = FALSE)
require(rvest)
library(tidyverse)
library(splashr)
library(XML)
library(magrittr)
library(stringr)
library(tidytext)
library(pbapply)

rh <- render_html(url = "https://www.azlyrics.com/r/radiohead.html", wait = 10)

li <- html_nodes(rh, "a") %>% html_attr("href")
li_text <- html_nodes(rh, "a") %>% html_text()
li_df <- data.frame(links = li_text, urls = li, stringsAsFactors = FALSE)
li_df <- unique(li_df)
li_df <- li_df[33:163,]
  
li_df$missing_url <- ifelse(substr(li_df$urls,1,2)=="..",paste(c("http://www.azlyrics.com"),li_df$urls,sep = ""),li_df$urls)
li_df$missing_url <- gsub("\\.{2}","http://www.azlyrics.com",li_df$urls)

read_lyrics <- function(url) {
page <- render_html(url = url, wait = 7)
links <- html_nodes(page, "a") %>%
  html_text()
album <- links[35] %>% as.tibble
div <- html_nodes(page, "div") %>% 
  html_text () %>% 
  as.tibble()
div$row <- row.names(div)
# find anchor
anchor <- div %>% 
  filter(str_detect(value, fixed("\n\nif  ( /"))) %>% 
  select(row) %>% 
  filter(row == max(row)) %>%
  as.numeric() %>%
  -1
words <- div[anchor,1]
lyrics <- bind_cols(album, words)
return(lyrics)
}

radiohead <- pblapply(li_df$missing_url[121:131], read_lyrics)

radiohead_new <- bind_rows(radiohead)
radiohead_all <- bind_rows(radiohead_all, radiohead_new)

