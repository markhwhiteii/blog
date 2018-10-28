library(tidyverse)
library(rvest)
years <- 1980:2018
per <- lapply(years, function(x) {
  paste0(
    "https://www.basketball-reference.com/leagues/NBA_",
    x,
    "_leaders.html"
  ) %>% 
    read_html() %>% 
    html_table() %>% 
    getElement(30) %>% 
    transmute(
      team = substr(X2, nchar(X2) - 2, nchar(X2)),
      per = X3
    )
})
names(per) <- years
  
standings <- lapply(years, function(x) {
  tmp <- paste0(
    "https://www.basketball-reference.com/leagues/NBA_",
    x,
    "_ratings.html"
  ) %>% 
    read_html() %>% 
    html_table() %>% 
    getElement(1) %>% 
    `[`(-1, 2:3)
  colnames(tmp) <- c("team", "conf")
  tmp
})
names(standings) <- years

key <- lapply(years, function(x) {
  tmp <- paste0(
    "https://www.basketball-reference.com/leagues/NBA_", 
    x, 
    "_standings.html"
  ) %>% 
    read_html() %>% 
    html_node(xpath = '//*[@id="all_team_vs_team"]/comment()') %>% 
    html_text() %>% 
    read_html() %>% 
    html_table() %>% 
    getElement(1) %>% 
    as.data.frame()
  
  suppressWarnings(
    tmp <- data.frame(
      team = tmp$Team,
      abbr = colnames(tmp)[-1:-2]
    ) %>% 
      full_join(standings[[as.character(x)]], by = "team") %>% 
      select("abbr", "conf")
  )
  
  colnames(tmp)[[1]] <- "team"
  tmp
})
names(key) <- years

results_per <- lapply(as.character(years), function(x) {
  tmp <- suppressWarnings(left_join(per[[x]], key[[x]], by = "team"))
  tmp$year <- x
  tmp
}) %>% 
  do.call(rbind, .)

results_allnba <- lapply(years, function(x) {
  tmp <- paste0(
    "https://www.basketball-reference.com/leagues/NBA_",
    x,
    "_per_game.html"
  ) %>% 
    read_html() %>% 
    html_table() %>% 
    getElement(1) %>% 
    mutate(Player = gsub("*", "", Player, fixed = TRUE)) %>% 
    group_by(Player) %>% 
    slice(1) %>% 
    ungroup() %>% 
    transmute(player = Player, team = Tm)
  
  suppressWarnings(
    paste0(
      "https://www.basketball-reference.com/leagues/NBA_",
      x,
      ".html#all_all-nba"
    ) %>% 
      read_html() %>% 
      html_node(xpath = '//*[@id="all_all-nba"]/comment()') %>% 
      html_text() %>% 
      read_html() %>% 
      html_table() %>% 
      do.call(rbind, .) %>% 
      separate(X1, paste0("p", 1:5), sep = "\\s{2}") %>% 
      gather() %>% 
      transmute(player = value) %>% 
      left_join(tmp, by = "player") %>% 
      left_join(key[[as.character(x)]], by = "team") %>% 
      count(conf) %>% 
      mutate(prop = n / sum(n), year = x)
  )
}) %>% 
  do.call(rbind, .)

write_csv(results_per, "results_per.csv")
write_csv(results_allnba, "results_allnba.csv")
