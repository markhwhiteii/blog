library(rvest)
library(tidyverse)

clean_table <- function(x) {
  # clean colum names
  col_names <- unname(unlist(x[1, , drop = TRUE]))
  col_names[1] <- "Player"
  x <- x[-1, ]
  colnames(x) <- col_names
  
  # get rid of row for reserves and DNPs and totals, clean MP
  x <- x %>% 
    as_tibble() %>% 
    filter(
      !Player %in% c("Reserves", "Team Totals") &
        !MP %in% c("Did Not Play", "Not With Team", "Did Not Dress",
                   "Player Suspended")
    ) %>% 
    mutate(MP = round(as.numeric(lubridate::ms(MP)) / 60)) %>% 
    mutate_at(vars(everything(), -Player, -MP), as.numeric)
    
  # output it
  return(x)
}

get_boxscore <- function(url) {
  # pull in html
  tmp1 <- url %>% 
    read_html()
  
  tmp2 <- tmp1 %>% 
    html_table()
  
  # more tables if overtimes
  inc <- 7
  if (length(tmp2) == 18) inc <- 8 # 1OT
  if (length(tmp2) == 20) inc <- 9 # 2OT
  if (length(tmp2) == 22) inc <- 10 # 3OT
  if (length(tmp2) == 24) inc <- 11 # 4OT
  if (length(tmp2) == 26) inc <- 12 # 5OT
  if (length(tmp2) == 28) inc <- 13 # 6OT
  
  # get the tables we need
  # NOTE: I got home and away backwards; flip home and away in post
  tables <- list(
    home_basic = tmp2[[1]], 
    home_advanced = tmp2[[1 + inc]],
    away_basic = tmp2[[2 + inc]],
    away_advanced = tmp2[[2 + 2 * inc]]
  )
  
  # clean and add meta
  teams <- tmp1 %>% 
    html_node("h1") %>% 
    html_text() %>% 
    stringr::str_split(" at ") %>% 
    unlist()
  tmp3 <- stringr::str_split(teams[2], " Box Score")
  teams[2] <- tmp3[[1]][[1]]
  
  # NOTE: I got home and away backwards; flip home and away in post
  tables <- lapply(tables, clean_table)
  tables$home_basic$Team <- tables$home_advanced$Team <- teams[1]
  tables$home_basic$Home <- tables$home_advanced$Home <- "Home"
  tables$away_basic$Team <- tables$away_advanced$Team <- teams[2]
  tables$away_basic$Home <- tables$away_advanced$Home <- "Away"
  
  # join
  out <- tables$home_basic %>% 
    full_join(
      tables$home_advanced, 
      by = c("Player", "MP", "Team", "Home")
    ) %>% 
    bind_rows({
      tables$away_basic %>% 
        full_join(
          tables$away_advanced, 
          by = c("Player", "MP", "Team", "Home")
        )
    })
  
  # add the game meta
  out$Date <- tmp1 %>% 
    html_nodes(".scorebox_meta div:nth-child(1)") %>% 
    html_text() %>% 
    lubridate::parse_date_time(orders = "%H:%M %p, %B %d, %Y")
  
  out$GameID <- gsub(
    "https://www.basketball-reference.com/boxscores/", 
    "", 
    url
  ) %>% 
    gsub(".html", "", .)
  
  return(out)
}

get_urls <- function(years) {
  # make template for url
  tmp <- "https://www.basketball-reference.com/leagues/NBA_{y}_games-{m}.html"
  
  # get every combo of year and month
  grids <- expand.grid(
    c("october", "november", "december", "january", "february", "march",
      "april", "may", "june"), years
  )
  
  # remove lockout season months
  grids <- grids %>% 
    filter(!(Var1 %in% c("october", "november") & Var2 == 2012))
  
  tmp <- sapply(1:nrow(grids), function(x) {
    glue::glue(tmp, y = grids$Var2[x], m = grids$Var1[x])
  })
  
  # get box score stubs
  tmp <- lapply(tmp, function(x) {
    x %>% 
      read_html() %>% 
      html_nodes(".center a") %>% 
      as.character() %>% 
      stringr::str_sub(21, 32)
  }) %>% 
    unlist()
  
  # staple stubs
  out <- paste0(
    "https://www.basketball-reference.com/boxscores/", 
    tmp, 
    ".html"
  )
  
  return(out)
}

get_everything <- function(years) {
  urls <- get_urls(years)
  out <- map_dfr(urls, function(x) {
    cat(which(x == urls), "\n")
    get_boxscore(x)
  })
  return(out)
}

# do it year by year at a time in case it gets cut off
for (i in 2010:2019) {
  cat(i, "\n")
  tmp <- get_everything(i)
  if (i == 2010) write_csv(tmp, "indiv_boxscores_2010-2019.csv")
  else write_csv(tmp, "indiv_boxscores_2010-2019.csv", append = TRUE)
  Sys.sleep(10)
}
