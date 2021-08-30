library(dglm)
library(tidyverse)

dat <- read_csv("ratings.csv")

m1 <- lm(Rating ~ Year, dat)
summary(m1)

ggplot(dat, aes(x = Year, y = Rating)) +
  geom_count() +
  geom_smooth(method = "lm", formula = y ~ x) +
  theme_light()

m2 <- dglm(Rating ~ Year, ~ Year, data = dat, method = "reml")
summary(m2, dispersion = 2)

ggplot(mapping = aes(x = dat$Year, y = m2$dispersion.fit$fitted.values)) +
  geom_line() +
  theme_light() +
  labs(x = "Year", y = "Predicted Variance")

m3 <- dglm(Rating ~ Year, ~ splines::bs(Year), data = dat, method = "reml")
summary(m3, dispersion = 2)

m2$m2loglik
m3$m2loglik

ggplot(mapping = aes(x = dat$Year, y = m3$dispersion.fit$fitted.values)) +
  geom_line() +
  theme_light() +
  labs(x = "Year", y = "Predicted Variance")
