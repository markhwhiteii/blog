library(tidyverse)
out <- read_csv("fivethirtyeight_results.csv")

# make most recent projection final data set -----------------------------------
sen <- read_csv("senate_seat_forecast.csv") %>% 
  filter(forecastdate == max(forecastdate) & model == "deluxe") %>%
  group_by(state, class) %>% 
  top_n(1, win_probability) %>% 
  mutate(race_type = "senate") %>% 
  select(-forecastdate, -special, -model)

house <- read_csv("house_district_forecast.csv") %>% 
  filter(forecastdate == max(forecastdate) & model == "deluxe") %>% 
  group_by(state, district) %>% 
  top_n(1, win_probability) %>% 
  mutate(race_type = "house") %>% 
  select(-forecastdate, -special, -model)

gov <- read_csv("governor_state_forecast.csv") %>% 
  filter(forecastdate == max(forecastdate) & model == "deluxe") %>%
  group_by(state) %>% 
  top_n(1, win_probability) %>% 
  mutate(race_type = "governor") %>% 
  select(-forecastdate, -special, -model, -district)

dat <- do.call(bind_rows, list(sen, house, gov))

# dat %>% 
#   full_join(out, by = c("state", "candidate", "race_type")) %>% 
#   write_csv("fivethirtyeight_final.csv")

# get correlation at each day --------------------------------------------------
sen <- read_csv("senate_seat_forecast.csv")
house <- read_csv("house_district_forecast.csv")
gov <- read_csv("governor_state_forecast.csv")

results <- lapply(sort(unique(sen$forecastdate)), function(x) {
  tmp1 <- sen %>% 
    filter(forecastdate == x & model == "deluxe") %>%
    group_by(state, class) %>% 
    top_n(1, win_probability) %>% 
    mutate(race_type = "senate") %>% 
    select(-forecastdate, -special, -model)
  
  tmp2 <- house %>% 
    filter(forecastdate == x & model == "deluxe") %>% 
    group_by(state, district) %>% 
    top_n(1, win_probability) %>% 
    mutate(race_type = "house") %>% 
    select(-forecastdate, -special, -model)
  
  tmp3 <- gov %>% 
    filter(forecastdate == x & model == "deluxe") %>%
    group_by(state) %>% 
    top_n(1, win_probability) %>% 
    mutate(race_type = "governor") %>% 
    select(-forecastdate, -special, -model, -district)
  
  tmp <- do.call(bind_rows, list(tmp1, tmp2, tmp3)) %>% 
    left_join(out, by = c("state", "candidate", "race_type")) %>% 
    ungroup() %>% 
    select(win_probability, won) %>% 
    filter(complete.cases(.) & win_probability < 1L)
  
  slope <- coef(lm(won ~ win_probability, tmp))[[2]]
  
  data.frame(slope = slope, correct = mean(tmp$won), date = x)
})

# write_csv(do.call(rbind, results), "fivethirtyeight_overtime.csv")
