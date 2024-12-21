# prep -------------------------------------------------------------------------
library(rsample)
library(yardstick)
library(glmnet)
library(tidyverse)

ms <- metric_set(accuracy, sensitivity, specificity, precision)

# fill mask --------------------------------------------------------------------
dat_mask <- read_csv("ratings_masked.csv") %>% 
  select(-1) %>% # I always forget to set index=False
  filter(visual_style != 9) %>% 
  mutate(
    visual_style = factor(visual_style),
    masked_class = factor(ifelse(masked_class == "yes", 1, 0))
  )

ms(dat_mask, truth = visual_style, estimate = masked_class)
with(dat_mask, prop.table(table(visual_style)))
with(dat_mask, prop.table(table(masked_class)))

# embeddings -------------------------------------------------------------------
dat_embed <- read_csv("ratings_embedded.csv") %>% 
  filter(visual_style != 9)

set.seed(1839)
dat_embed <- dat_embed %>%
  initial_split(.5, strata = visual_style)

X <- dat_embed %>% 
  training() %>% 
  select(starts_with("embed_")) %>% 
  as.matrix()

y <- dat_embed %>% 
  training() %>% 
  pull(visual_style)

mod <- cv.glmnet(X, y, family = "binomial")

X_test <- dat_embed %>% 
  testing() %>% 
  select(starts_with("embed_")) %>% 
  as.matrix()

preds <- predict(mod, X_test, s = mod$lambda.min, type = "response")

preds <- tibble(
  y = factor(testing(dat_embed)$visual_style),
  y_hat_prob = c(preds),
  y_hat_class = factor(as.numeric(y_hat_prob > .5))
)

ms(preds, truth = y, estimate = y_hat_class)
with(preds, prop.table(table(y)))
with(preds, prop.table(table(y_hat_class)))
mean(preds$y_hat_prob)

# generative -------------------------------------------------------------------
dat_gen <- read_csv("ratings_generative.csv") %>% 
  select(-1) %>% # I always forget to set index=False
  filter(visual_style != 9) %>% 
  mutate(
    visual_style = factor(visual_style),
    gen_class = factor(gen_class)
  )

ms(dat_gen, truth = visual_style, estimate = gen_class)
with(dat_gen, prop.table(table(visual_style)))
with(dat_gen, prop.table(table(gen_class)))
