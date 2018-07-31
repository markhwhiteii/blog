library(tidyverse)
library(rvest)
raw <- "https://projects.fivethirtyeight.com/2018-nba-predictions/games/" %>% 
  read_html() %>% 
  html_table(fill = TRUE)

playoffs <- raw[2:83] # ignore first entry, it's a playoff prediction table
regular <- raw[84:length(raw)]

# tidy playoffs ----------------------------------------------------------------
playoffs <- lapply(playoffs, function(x) {
  x %>% 
    `[`(2:3, c(3, 5, 6)) %>% 
    `colnames<-`(c("team", "win_prob", "score")) %>% 
    mutate(
      team = ifelse(
        substring(team, nchar(team), nchar(team)) %in% as.character(0:4),
        substring(team, 1, nchar(team) - 3),
        team
      ),
      win_prob = sub("%", "", win_prob),
      home_away = c("away", "home")
    ) %>% 
    gather("k", "v", -home_away) %>% 
    unite("k", k, home_away) %>% 
    spread(k, v) %>% 
    mutate_at(vars(1:2, 5:6), as.numeric)
})
playoffs <- do.call(rbind, playoffs)

# acuracy:
mean(with(playoffs, (score_away > score_home & win_prob_away > win_prob_home) |
            score_away < score_home & win_prob_away < win_prob_home))

# tidy regular -----------------------------------------------------------------
regular <- lapply(regular, function(x) {
  x %>% 
    `[`(2:3, c(3, 5, 6)) %>% 
    `colnames<-`(c("team", "win_prob", "score")) %>% 
    mutate(
      win_prob = sub("%", "", win_prob),
      home_away = c("away", "home")
    ) %>% 
    gather("k", "v", -home_away) %>% 
    unite("k", k, home_away) %>% 
    spread(k, v) %>% 
    mutate_at(vars(1:2, 5:6), as.numeric)
})
regular <- do.call(rbind, regular)

# write external ---------------------------------------------------------------
write_csv(regular, "regular_season_18.csv")
write_csv(playoffs, "playoffs_18.csv")
