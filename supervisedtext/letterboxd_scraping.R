library(rvest)
library(tidyverse)

# 12 reviews per page
# 10 possible ratings
# 6 pages each
# n = 720 reviews total

base_url1 <- "https://letterboxd.com/film/asteroid-city/reviews/rated/"
base_url2 <- "/by/activity/page/"
ratings <- seq(.5, 5, .5)
pages <- 6

# people often say to use purrr::map or an apply statement instead of a for
# loop. but if it fails, I want to know where, and I want to keep the data
# we already collected. so I'm initializing an empty data frame and then
# slotting the data in one page at a time
dat <- tibble(rating = vector("double"), text = vector("character"))

for (r in ratings) {
  pg <- 1
  
  while (pg < 7) {
    url <- paste0(base_url1, r, base_url2, pg)
    
    txt <- url %>% 
      read_html() %>% 
      html_nodes(".collapsible-text") %>% 
      html_text2() %>% 
      map_chr(str_replace_all, fixed("\n"), " ")
    
    dat <- bind_rows(dat, tibble(rating = r, text = txt))
    
    cat("finished page", pg, "of", r, "stars\n")
    
    pg <- pg + 1
    Sys.sleep(runif(1, 0, 10))
  }
}

write_csv(dat, "ratings_uncoded.csv")
