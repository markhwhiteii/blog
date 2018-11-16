library(tidyverse)
dat <- read_csv("fivethirtyeight_final.csv") %>% 
  filter(win_probability < 1L)

with(
  dat[!is.na(dat$won), ],
  list(
    mean(win_probability),
    median(win_probability),
    mean(won)
  )
)

table(dat$won, useNA = "always")

dat %>% 
  filter(!won & !is.na(won)) %>% 
  arrange(desc(win_probability)) %>% 
  select(state, race_type, candidate, party, incumbent, win_probability)

ggplot(dat, aes(x = win_probability, y = as.numeric(won))) +
  # geom_point() +
  geom_smooth(
    method = "glm", 
    method.args = list(family = binomial), 
    se = FALSE,
    color = "gold"
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "purple"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  labs(x = "538 Predicted Win Probability", y = "Actual Win Probability") +
  theme_minimal() +
  theme(text = element_text(size = 16))

# what if all outstanding races are wrong?
dat %>% 
  mutate(won = ifelse(is.na(won), FALSE, won)) %>% 
  ggplot(aes(x = win_probability, y = as.numeric(won))) +
  geom_point() +
  geom_smooth(
    method = "glm", 
    method.args = list(family = binomial),
    se = FALSE,
    color = "purple"
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "gold"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  labs(x = "538 Predicted Win Probability", y = "Actual Win Probability") +
  theme_minimal() +
  theme(text = element_text(size = 16))

# what if correct?
dat %>% 
  mutate(won = ifelse(is.na(won), TRUE, won)) %>% 
  ggplot(aes(x = win_probability, y = as.numeric(won))) +
  geom_point() +
  geom_smooth(
    method = "glm", 
    method.args = list(family = binomial), 
    se = FALSE,
    color = "purple"
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "gold"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
  labs(x = "538 Predicted Win Probability", y = "Actual Win Probability") +
  theme_minimal() +
  theme(text = element_text(size = 16))

# bucketed
dat %>% 
  filter(win_probability <= .75 & !is.na(won)) %>% 
  summarise(count = n(),
            win_probability_mean = mean(win_probability),
            win_probability_median = median(win_probability),
            correct_rate = mean(won))

dat %>% 
  filter(win_probability <= .65 & !is.na(won)) %>% 
  summarise(count = n(),
            win_probability_mean = mean(win_probability),
            win_probability_median = median(win_probability),
            correct_rate = mean(won))

dat %>% 
  filter(win_probability <= .55 & !is.na(won)) %>% 
  summarise(count = n(),
            win_probability_mean = mean(win_probability),
            win_probability_median = median(win_probability),
            correct_rate = mean(won))

# bucketed, worst-case scenario
dat %>% 
  mutate(won = ifelse(is.na(won), FALSE, won)) %>% 
  filter(win_probability <= .75 & !is.na(won)) %>% 
  summarise(win_probability_mean = mean(win_probability),
            win_probability_median = median(win_probability),
            correct_rate = mean(won))

dat %>% 
  mutate(won = ifelse(is.na(won), FALSE, won)) %>% 
  filter(win_probability <= .65 & !is.na(won)) %>% 
  summarise(win_probability_mean = mean(win_probability),
            win_probability_median = median(win_probability),
            correct_rate = mean(won))

dat %>% 
  mutate(won = ifelse(is.na(won), FALSE, won)) %>% 
  filter(win_probability <= .55 & !is.na(won)) %>% 
  summarise(win_probability_mean = mean(win_probability),
            win_probability_median = median(win_probability),
            correct_rate = mean(won))

# over time
dattime <- read_csv("fivethirtyeight_overtime.csv")
ggplot(dattime, aes(x = date, y = correct)) +
  geom_point()
