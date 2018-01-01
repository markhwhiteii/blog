library(tidyverse)
dat <- read_csv("viceroy.csv")
datwide <- read_csv("viceroy_wide.csv")

set.seed(1839)
results <- CausalImpact::CausalImpact(
  data = datwide, 
  pre.period = c(min(which(unique(dat$date) < "2012-10-16")),
                 max(which(unique(dat$date) < "2012-10-16"))),
  post.period = c(min(which(unique(dat$date) > "2012-10-16")),
                  max(which(unique(dat$date) > "2012-10-16"))),
  model.args = list(niter = 5000)
)

plot(results, "original") +
  theme(text = element_text(size = 14)) +
  xlab("Month")
