library(rvest)
library(RSelenium)
library(tidyverse)

# global params ----------------------------------------------------------------
# url <- "https://www.rottentomatoes.com/m/star_wars_episode_i_the_phantom_menace/reviews?type=user"
# url <- "https://www.rottentomatoes.com/m/star_wars_episode_ii_attack_of_the_clones/reviews?type=user"
# url <- "https://www.rottentomatoes.com/m/star_wars_episode_iii_revenge_of_the_sith/reviews?type=user"
pages_to_scrape <- 500

# funs -------------------------------------------------------------------------
get_stars <- function(the_html) {
  stars_text <- the_html %>% 
    html_nodes(".audience-reviews__score") %>% 
    as.character()
  
  stars <- map_dbl(stars_text, function(x) {
    score <- 0
    score <- score + str_count(x, "star-display__filled")
    score <- score + (str_count(x, "star-display__half") / 2)
  })
  
  return(stars)
}

get_dates <- function(the_html) {
  date_text <- the_html %>% 
    html_nodes(".audience-reviews__duration") %>% 
    as.character()
  
  dates <- map_chr(date_text, function(x) {
    str_split(x, ">") %>% 
      `[[`(1) %>% 
      `[`(2) %>% 
      str_remove("</span")
  })
  
  return(dates)
}

get_texts <- function(the_html) {
  text_text <- the_html %>% 
    html_nodes(".js-clamp") %>% 
    as.character() %>% 
    `[`(seq(1, length(.), by = 2))
  
  texts <- map_chr(text_text, function(x) {
    str_split(x, ">") %>% 
      `[[`(1) %>% 
      `[`(2) %>% 
      str_remove("[<].*")
  })
  
  return(texts)
}

get_reviews <- function(the_html) {

  reviews <- tibble(
    date = get_dates(the_html),
    stars = get_stars(the_html),
    text = get_texts(the_html)
  )
  
  return(reviews)
}

# scrape -----------------------------------------------------------------------
res <- tibble()
driver <- rsDriver(browser = "firefox", port = 1839L)
driver$client$navigate(url)

for (i in seq_len(pages_to_scrape)) {
  cat("page", i, "\n")
  
  res <- res %>% 
    bind_rows(get_reviews(read_html(driver$client$getPageSource()[[1]])))
  
  more <- driver$client$findElement(
    "css selector", 
    ".prev-next-paging__button-right"
  )
  
  more$clickElement()
  Sys.sleep(2)
}

driver$client$closeall()

# write out --------------------------------------------------------------------
# res %>% 
#   unique() %>% 
#   write_csv("ep1.csv")

# res %>%
#   unique() %>%
#   write_csv("ep2.csv")

# res %>%
#   unique() %>%
#   write_csv("ep3.csv")
