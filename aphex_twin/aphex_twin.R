library(tidyverse)

# get data ---------------------------------------------------------------------
library(spotifyr)

get_related_recursive <- function(id, iter) {
  out <- get_related_artists(id)
  done_iter <- 0
  while (done_iter < iter) {
    tmp <- unique(do.call(bind_rows, lapply(out$id, get_related_artists)))
    out <- unique(bind_rows(out, tmp))
    done_iter <- done_iter + 1
  }
  return(out)
}

dat <- get_related_recursive("6kBDZFXuLrZgHnvmPu9NsG", 3)

dat <- dat %>% 
  filter(followers.total >= 80000) %>% 
  select_if(function(x) !is.list(x))

write_csv(dat, "aphex_twin_related.csv")

dat2 <- lapply(dat$name, function(x) {
  cat(x, "\n")
  tryCatch(
    get_artist_audio_features(x),
    error = function(x) return(NA)
  )
})

dat2 <- lapply(dat2, function(x) {
  if (is.data.frame(x)) {
    x %>% 
      select_if(function(x) !is.list(x))
  } else {
    NA
  }
})

dat2 <- dat2[!is.na(dat2)] %>% # 3 artists failed to parse
  do.call(bind_rows, .)

write_csv(dat2, "aphex_twin_related_features.csv")

# analyze data -----------------------------------------------------------------
dat <- read_csv("aphex_twin_related_features.csv")

gen_var <- sapply(unique(dat$artist_name), function(x) {
  dat %>% 
    filter(artist_name == x) %>% 
    select(danceability, energy, valence) %>% 
    cov() %>% 
    det()
})

tibble(artist_name = unique(dat$artist_name), gen_var) %>%
  top_n(30, gen_var) %>% 
  arrange(gen_var) %>% 
  mutate(artist_name = factor(artist_name, artist_name)) %>% 
  ggplot(aes(x = artist_name, y = gen_var)) +
  geom_bar(stat = "identity", fill = "#4B0082") +
  coord_flip() +
  labs(x = "Artist", y = "Generalized Variance", caption = "@markhw_") +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text())
