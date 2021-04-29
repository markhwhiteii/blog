# TODO: transition this to workflowsets instead of two workflows
# prep -------------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
set.seed(1839)

dat <- list.files(pattern = "^ep") %>% 
  map_dfr(~read_csv(.x) %>% mutate(film = str_sub(.x, 1, 3))) %>% 
  transmute(film, date = lubridate::mdy(date), stars) %>% 
  na.omit() %>% 
  filter(date >= "2012-04-09") %>% 
  mutate(date = as.numeric(date - lubridate::ymd("2012-04-09")))

dat_train <- training(dat)
dat_test <- testing(dat)

# set model --------------------------------------------------------------------
lm_mod <- linear_reg() %>% 
  set_engine("lm")

# make recipes -----------------------------------------------------------------
rec_bs <- recipe(stars ~ date + film, dat_train) %>% 
  step_bs(date, deg_free = tune(), degree = tune()) %>% 
  step_dummy(film) %>% 
  step_interact(~ starts_with("date"):starts_with("film"))

rec_ns <- recipe(stars ~ date + film, dat_train) %>% 
  step_ns(date, deg_free = tune()) %>% 
  step_dummy(film) %>% 
  step_interact(~ starts_with("date"):starts_with("film"))

wf_bs <- workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(rec_bs)

wf_ns <- workflow() %>% 
  add_model(lm_mod) %>% 
  add_recipe(rec_ns)

# cross validation -------------------------------------------------------------
# param grids
grid_bs <- tibble(deg_free = rep(4:10, 3), degree = rep(2:4, each = 7))
grid_ns <- tibble(deg_free = 2:10)

# define folds
folds <- vfold_cv(dat_train, v = 10)

# do cv
cv_bs <- wf_bs %>% 
  tune_grid(resamples = folds, grid = grid_bs)

cv_ns <- wf_ns %>% 
  tune_grid(resamples = folds, grid = grid_ns)

# compare models and params ----------------------------------------------------
ests <- cv_bs %>% 
  collect_metrics() %>% 
  filter(.metric == "rsq") %>% 
  mutate(recipe = "step_bs") %>%
  bind_rows({
    cv_ns %>% 
      collect_metrics() %>% 
      filter(.metric == "rsq") %>% 
      mutate(recipe = "step_ns")
  }) %>% 
  mutate(model = 1:n())

ggplot(ests, aes(x = model, y = mean, color = recipe)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err))

best_bs <- select_by_one_std_err(cv_bs, deg_free, degree, metric = "rsq")
best_ns <- select_by_one_std_err(cv_ns, deg_free, metric = "rsq")

# predict ----------------------------------------------------------------------
wf_bs %>% 
  finalize_workflow(best_bs) # erroring here

fit_ns <- wf_ns %>% 
  finalize_workflow(best_ns) %>% 
  fit(dat_train)

fit_ns %>% 
  pluck("fit")
  
dat_test <- bind_cols(dat_test, predict(fit_ns, dat_test))

# plotting ---------------------------------------------------------------------
tfa <- as.numeric(lubridate::ymd("2015-12-18") - lubridate::ymd("2012-04-09"))
tlj <- as.numeric(lubridate::ymd("2017-12-15") - lubridate::ymd("2012-04-09"))
tros <- as.numeric(lubridate::ymd("2019-12-20") - lubridate::ymd("2012-04-09"))

ggplot(dat_test, aes(x = date, y = stars)) +
  facet_wrap(~ film) +
  geom_count(alpha = .3) +
  geom_line(aes(x = date, y = .pred...5), color = "blue") +
  geom_vline(aes(xintercept = tfa)) +
  geom_vline(aes(xintercept = tlj)) +
  geom_vline(aes(xintercept = tros))

# plot both --------------------------------------------------------------------
ggplot(dat_test, aes(x = date, y = stars)) +
  geom_count(alpha = .3) +
  geom_smooth(se = FALSE, method = lm, formula = y ~ splines::ns(x, df = 4)) +
  geom_smooth(
    se = FALSE,
    method = lm,
    formula = y ~ splines::bs(x, df = 4, degree = 2),
    color = "red"
  ) +
  movie_verticals() +
  facet_wrap(~ film)
