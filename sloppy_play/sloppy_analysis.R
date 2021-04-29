# prep -------------------------------------------------------------------------
library(lme4)
library(lmerTest)
library(emmeans)
library(tidyverse)

days <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
          "Friday", "Saturday", "Sunday")

dat <- read_csv("indiv_boxscores_2010-2019.csv") %>% 
  mutate(
    Home = ifelse(Home == "Home", "Away", "Home"), # fix mistake
    day_of_week = factor(weekdays(Date), days),
    time_of_day = lubridate::hour(Date) + (lubridate::minute(Date) / 60),
    time_of_day = ifelse(time_of_day < 11, time_of_day + 12, time_of_day),
    season_team = paste0(lubridate::year(Date), "_", tolower(Team)),
    season_team = gsub("[ ]", "_", season_team)
  ) %>% 
  janitor::clean_names(replace = c("%" = "_pct"))

# eda --------------------------------------------------------------------------
# what's the distribution of times by day of week?
dat %>% 
  select(game_id, day_of_week, time_of_day) %>% 
  unique() %>% 
  ggplot(aes(x = time_of_day)) +
  geom_histogram() +
  facet_wrap(~ day_of_week)

# it looks like before 6 is the natural cutoff

dat$afternoon <- dat$time_of_day < 18
dat$sunday_afternoon <- dat$day_of_week == "Sunday" & dat$afternoon

# group by afternoon
dat %>% 
  group_by(afternoon) %>% 
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  gather("stat", "value", -afternoon) %>% 
  mutate(value = round(value, 2)) %>% 
  spread(afternoon, value) %>% 
  as.data.frame()
# really doesn't look like much changes

# group by sunday afternoon
dat %>% 
  group_by(sunday_afternoon) %>% 
  summarise(
    across(where(is.numeric), mean, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  gather("stat", "value", -sunday_afternoon) %>% 
  mutate(value = round(value, 2)) %>% 
  spread(sunday_afternoon, value) %>% 
  as.data.frame()
# again, really doesn't look like much changes

# grouping by...
# - player
# - season_team
# - game_id

# how many unique values for each?
length(unique(dat$player))
length(unique(dat$season_team))
length(unique(dat$game_id))

# minutes--where should cutoff be?
hist(dat$mp, breaks = length(unique(dat$mp))); summary(dat$mp)
# dropoff is 12, so one quarter

dat <- dat %>% 
  filter(mp > 11)

table(dat$mp)

# turnovers --------------------------------------------------------------------
# this is the better stat, but its gonna be a hierarchical betareg
#     with zero and one inflation...
hist(dat$tov_pct)
hist(dat$tov) # try this first, even though pace is a confounder

m_tov <- glmer(
  tov ~ sunday_afternoon * home +
    (1 | player) + 
    (1 | season_team) + 
    (1 | game_id), 
  dat,
  poisson
)

summary(m_tov)

emmeans(m_tov, ~ sunday_afternoon | home, type = "response")

m_tov2 <- lmer(
  tov_pct ~ sunday_afternoon * home +
    (1 | player) + 
    (1 | season_team) + 
    (1 | game_id), 
  dat
)

summary(m_tov2)

emmeans(m_tov2, ~ sunday_afternoon | home)

# ts pct -----------------------------------------------------------------------
hist(dat$ts_pct)

m_ts <- lmer(
  ts_pct ~ sunday_afternoon * home +
    (1 | player) + 
    (1 | season_team) + 
    (1 | game_id), 
  dat
)

summary(m_ts)

emmeans(m_ts, ~ sunday_afternoon | home)
