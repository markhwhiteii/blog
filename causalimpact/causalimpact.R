# prep
x <- c("rvest", "dplyr", "lubridate", "stringr", "CausalImpact")
purrr::walk(x, library, character.only = TRUE)

# fetch, clean data
url1 <- "http://www.basketball-reference.com/teams/MEM/2013_games.html" # griz 12-13
url2 <- "http://www.basketball-reference.com/teams/TOR/2014_games.html" # raps 13-14

griz <- read_html(url1) %>% 
  html_table() %>% 
  `[[`(1) %>% # don't want playoffs
  `[`(,c("Date", "Tm", "Opp")) %>% #select() didn't work because duplicate col names
  filter(Date!="Date") %>% # b-r dupes headers
  mutate(
    Date=mdy(str_replace(Date, "^.{0,5}", "")), # getting rid of weekday
    Tm=as.numeric(Tm),
    Opp=as.numeric(Opp),
    PtDiff=Tm-Opp,
    Result=factor(ifelse(PtDiff > 0, "Win", "Lose")),
    GayTrade=factor(ifelse(Date < "2013-01-30", "Before", "After")) # date gay was traded
  )

raps <- read_html(url2) %>% 
  html_table() %>% 
  `[[`(1) %>%
  `[`(,c("Date", "Tm", "Opp")) %>%
  filter(Date!="Date") %>%
  mutate(
    Date=mdy(str_replace(Date, "^.{0,5}", "")),
    Tm=as.numeric(Tm),
    Opp=as.numeric(Opp),
    PtDiff=Tm-Opp,
    Result=factor(ifelse(PtDiff > 0, "Win", "Lose")),
    GayTrade=factor(ifelse(Date < "2013-12-09", "Before", "After")) # date gay was traded
  )

# causal impact
set.seed(1839)
grizmodel <- CausalImpact(
  griz$PtDiff, 
  c(min(which(griz$GayTrade=="Before")), max(which(griz$GayTrade=="Before"))), 
  c(min(which(griz$GayTrade=="After")), max(which(griz$GayTrade=="After"))),
  model.args=list(niter=5000)
)

rapsmodel <- CausalImpact(
  raps$PtDiff, 
  c(min(which(raps$GayTrade=="Before")), max(which(raps$GayTrade=="Before"))), 
  c(min(which(raps$GayTrade=="After")), max(which(raps$GayTrade=="After"))),
  model.args=list(niter=5000)
)

# results, plots
grizmodel
summary(grizmodel, "report")
plot(grizmodel) + ggplot2::theme_light()
rapsmodel
summary(rapsmodel, "report")
plot(rapsmodel) + ggplot2::theme_light()

# basic tables
with(griz, table(Result, GayTrade))
with(raps, table(Result, GayTrade))
