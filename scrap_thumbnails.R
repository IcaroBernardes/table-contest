################################################################################
########### Code for (politely) scrapping the thumbnails of the games ##########
################################################################################
 
# 0. Loads packages
library(tidyverse)
library(readr)
library(polite)
library(rvest)
library(xml2)
library(stringr)

# 1. Downloads the images and their IDs
## Salutes both BGG sites and asks them for permission to scrap
sessionBGG <- polite::bow("https://boardgamegeek.com/")
sessionGEEKDO <- polite::bow("https://cf.geekdo-images.com/")

## The rank presently has 1319 pages
for (page in 1:1319) {
  
  ## Asks the BGG site for permission to change page
  sessionBGG <- sessionBGG %>%
    polite::nod(path = paste0("browse/boardgame/page/",page))
  
  ## Scrapes the html of the page
  result <- polite::scrape(sessionBGG)
  
  ## Gets the links that contain the games IDs
  ids <- result %>%
    rvest::html_elements(".primary")
  
  ## Extracts the links that contain the games IDs
  ids <- purrr::map(ids, ~xml2::xml_attr(., "href"))
  
  ## Extracts the IDs
  ids <- purrr::map(ids, ~stringr::str_sub(.,12L,-1L))
  ids <- purrr::map(ids, ~stringr::str_sub(.,1L, stringr::str_locate(.,"/")[1]-1))
  
  ## Gets the links to the thumbnails
  img <- result %>%
    rvest::html_elements(".collection_thumbnail img")
  
  ## Extracts the links to the image sources
  img <- purrr::map(img, ~xml2::xml_attr(., "src"))
  
  ## Keeps only the link branches
  img <- purrr::map(img, ~stringr::str_remove(., "https://cf.geekdo-images.com/"))
  
  ## Downloads the thumbnails naming them after the game ID and the thumbnail extension
  for (game in 1:length(img)) {
    
    sessionGEEKDO %>%
      polite::nod(path = img[[game]]) %>% 
      polite::rip(
        destfile = paste0(ids[[game]],str_sub(img[[game]],-5L,-1L)),
        path = paste0(getwd(),"/www/images")
        )
    
  }
  
}
