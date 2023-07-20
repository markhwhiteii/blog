# load, explore ----------------------------------------------------------------
source("funs.R")

dat <- read_csv("ratings_coded.csv") %>% 
  filter(visual_style != 9) # remove non-english

dat %>% 
  count(visual_style) %>% 
  mutate(pct = n / sum(n) * 100)

ggplot(dat, aes(x = rating, y = visual_style)) +
  geom_count() +
  geom_smooth()

summary(glm(visual_style ~ poly(rating, 2), binomial, dat))

# run --------------------------------------------------------------------------
set.seed(1839)
dat <- prep_data(dat, "visual_style", "text", .20)
cv_res <- do_cv(training(dat), 4, 10)

# write out so don't have to do that again
write_rds(cv_res, "visual_style-res.rds")
cv_res <- read_rds("visual_style-res.rds")

# what the objects look like
cv_res$cv_res$result[[1]]$.metrics

# best model
mod <- best_model(dat, cv_res, "roc_auc")

# variable importance
print(mod$var_imp, n = 20)

# holdout metrics
mod$ho_metrics

# the fit, if we want to run a new review through it
new_reviews <- tibble(
  text = c("Kodak film and miniatures make for a cool movie",
           "To me, Wes never misses",
           paste(
             "Visuals and characters were stunning as always but the dual plot",
             "was hard to follow and the whole thing felt a bit hollow"
            ),
           "my head hurts idk how i feel about this yet",
           "Me after asking AI to write me a wes anderson script")
)

# predict on these
predict(mod$fit, new_reviews, type = "prob")
