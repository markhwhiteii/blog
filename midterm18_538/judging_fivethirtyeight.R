library(tidyverse)
dat <- read_csv("fivethirtyeight_final.csv") %>% 
  filter(win_probability < 1L)

results <- with(
  dat[!is.na(dat$won), ],
  list(
    mean_win_prob = mean(win_probability),
    correct_rate = mean(won)
  )
) %>% 
  lapply(function(x) paste0(round(x, 3) * 100, "%"))

outstanding_races <- dat %>% 
  filter(is.na(won)) %>% 
  select(state, race_type, candidate, party, incumbent, win_probability)

wrong_predictions <- dat %>% 
  filter(!won & !is.na(won)) %>% 
  arrange(desc(win_probability)) %>% 
  select(state, race_type, candidate, party, incumbent, win_probability)

figure1 <- ggplot(dat, aes(x = win_probability, y = as.numeric(won))) +
  geom_point() +
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
figure2 <- dat %>% 
  mutate(won = ifelse(is.na(won), FALSE, won)) %>% 
  ggplot(aes(x = win_probability, y = as.numeric(won))) +
  geom_point() +
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

# bucketed
results_thresholds <- dat %>% 
  filter(win_probability <= .75 & !is.na(won)) %>% 
  summarise(threshold = .75,
            count = n(),
            win_probability_mean = mean(win_probability),
            correct_rate = mean(won)) %>% 
  bind_rows({
    dat %>% 
      filter(win_probability <= .65 & !is.na(won)) %>% 
      summarise(threshold = .65,
                count = n(),
                win_probability_mean = mean(win_probability),
                correct_rate = mean(won))
  }) %>% 
  bind_rows({
    dat %>% 
      filter(win_probability <= .55 & !is.na(won)) %>% 
      summarise(threshold = .55,
                count = n(),
                win_probability_mean = mean(win_probability),
                correct_rate = mean(won))
  })
