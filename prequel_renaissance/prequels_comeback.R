# prep -------------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(workflowsets)
set.seed(1839)

dat <- list.files(pattern = "^ep") %>% 
  map_dfr(~read_csv(.x) %>% mutate(film = str_sub(.x, 1, 3))) %>% 
  transmute(film, date = lubridate::mdy(date), stars) %>% 
  na.omit() %>% 
  filter(date >= "2012-04-09") %>% 
  mutate(date = as.numeric(date - lubridate::ymd("2012-04-09"))) %>% 
  initial_split()

dat_train <- training(dat)
dat_test <- testing(dat)

# set models -------------------------------------------------------------------
lm_mod <- linear_reg() %>% 
  set_engine("lm")

rf_mod <- rand_forest(
  mode = "regression",
  trees = 1000,
  mtry = tune(),
  min_n = tune()
) %>% 
  set_engine("ranger")

nn_mod <- nearest_neighbor(
  mode = "regression", 
  neighbors = tune(),
  dist_power = tune(), 
  weight_func = tune()
) %>% 
  set_engine("kknn")

# make recipes -----------------------------------------------------------------
rec_base <- recipe(stars ~ date + film, dat_train) %>% 
  step_dummy(film)

rec_ns <- rec_base %>% 
  step_ns(date, deg_free = tune()) %>% 
  step_interact(~ starts_with("date"):starts_with("film"))

rec_zs <- rec_base %>% 
  step_normalize(all_predictors())

# workflowset ------------------------------------------------------------------
wfs <- workflow_set(
  preproc = list(spline = rec_ns, base = rec_base, zscored = rec_zs),
  models = list(lm_mod, rf_mod, nn_mod),
  cross = FALSE
)

# cross validation -------------------------------------------------------------
folds <- vfold_cv(dat_train, v = 10)

# cv_res <- wfs %>% 
#   workflow_map("tune_grid", seed = 1839, grid = 30, resamples = folds)
# 
# saveRDS(cv_res, file = "cv_res.rds")

cv_res <- readRDS("cv_res.rds")

# compare models and params ----------------------------------------------------
cv_res %>% 
  rank_results() %>% 
  filter(.metric == "rmse")

autoplot(cv_res, rank_metric = "rmse", metric = "rmse")

# predict ----------------------------------------------------------------------
best_results <- cv_res %>% 
  pull_workflow_set_result("base_rand_forest") %>% 
  select_best(metric = "rmse")

final_fit <- cv_res %>% 
  pull_workflow("base_rand_forest") %>% 
  finalize_workflow(best_results) %>% 
  fit(dat_train)

dat_test <- bind_cols(dat_test, predict(final_fit, dat_test))

# plotting ---------------------------------------------------------------------
tfa <- as.numeric(lubridate::ymd("2015-12-18") - lubridate::ymd("2012-04-09"))
tlj <- as.numeric(lubridate::ymd("2017-12-15") - lubridate::ymd("2012-04-09"))
tros <- as.numeric(lubridate::ymd("2019-12-20") - lubridate::ymd("2012-04-09"))

ggplot(dat_test, aes(x = date, y = stars)) +
  facet_wrap(~ film) +
  geom_vline(aes(xintercept = tfa)) +
  geom_vline(aes(xintercept = tlj)) +
  geom_vline(aes(xintercept = tros)) +
  geom_count(alpha = .3) +
  geom_line(aes(x = date, y = .pred), color = "blue")

# spline -----------------------------------------------------------------------
best_results <- cv_res %>% 
  pull_workflow_set_result("spline_linear_reg") %>% 
  select_best(metric = "rmse")

final_fit <- cv_res %>% 
  pull_workflow("spline_linear_reg") %>% 
  finalize_workflow(best_results) %>% 
  fit(dat_train)

dat_test$pred_spline <- predict(final_fit, dat_test)$.pred

ggplot(dat_test, aes(x = date, y = stars)) +
  facet_wrap(~ film) +
  geom_vline(aes(xintercept = tfa)) +
  geom_vline(aes(xintercept = tlj)) +
  geom_vline(aes(xintercept = tros)) +
  geom_count(alpha = .3) +
  geom_line(aes(x = date, y = pred_spline), color = "blue")
