library(tidyverse)
library(grf)
dat <- read_tsv("broockman_kalla_replication_data.tab") %>% 
  filter(respondent_t1 == "1.0" & contacted == "1.0") %>% 
  transmute(
    treatment = factor(ifelse(treat_ind == "1.0", "Treatment", "Control")),
    trans_therm_post = as.numeric(therm_trans_t1),
    trans_therm_pre = as.numeric(therm_trans_t0),
    age = vf_age,
    party = factor(vf_party),
    race = factor(vf_racename),
    voted14 = vf_vg_14,
    voted12 = vf_vg_12,
    voted10 = vf_vg_10,
    sdo = as.numeric(sdo_scale),
    canvass_minutes = as.numeric(canvass_minutes)
  ) %>% 
  filter(complete.cases(.))

set.seed(1839)
cases <- sample(seq_len(nrow(dat)), round(nrow(dat) / 2))
train <- dat[cases, ]
test <- dat[-cases, ]

cf <- causal_forest(
  X = model.matrix(~ ., data = train[, 3:ncol(train)]),
  Y = train$trans_therm_post,
  W = as.numeric(train$treatment) - 1,
  num.trees = 10000,
  seed = 1839
)

preds <- predict(cf, model.matrix(~ ., data = test[, 3:ncol(test)]))
test$preds <- c(preds$predictions)

# get top 1/2 of preds
t1 <- lm(trans_therm_post ~ treatment, test[test$preds > median(test$preds), ])
summary(t1)

# get middle 1/2 of original
tiles <- quantile(test$trans_therm_pre)
lgl <- test$trans_therm_pre >= tiles[[2]] & test$trans_therm_pre <= tiles[[4]]
t2 <- lm(trans_therm_post ~ treatment, test[lgl, ])
summary(t2)
