library(gamlss)
library(tidyverse)

# show with fake data ----------------------------------------------------------
n <- 100000
set.seed(1839)
x <- runif(n)
z <- runif(n)

mu <- 0.5 + 2 * x
sigma <- exp(1.5 + 3 * z)

y <- rnorm(n, mu, sigma)
dat <- data.frame(x, z, y)

m0 <- gamlss(y ~ x + z, ~ x + z, family = NO(), data = dat)
round(coef(m0, "mu"), 1)
round(coef(m0, "sigma"), 1)

# use letterboxd data ----------------------------------------------------------
ratings <- read_csv("ratings.csv") %>% 
  janitor::clean_names()

ggplot(ratings, aes(x = year, y = rating)) +
  geom_count() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, size = 1.5, color = ) +
  theme_light() +
  labs(x = "Year", y = "Rating") +
  scale_x_continuous(breaks = seq(1930, 2020, by = 10)) +
  theme(text = element_text(size = 16))

m1 <- gamlss(rating ~ year, ~ year, family = NO(), data = ratings)  
summary(m1)

plot_data <- data.frame(year = min(ratings$year):max(ratings$year))
pred_mu <- predict(m1, "mu", newdata = plot_data)
pred_sigma <- predict(m1, "sigma", newdata = plot_data, type = "response")
plot_data$mu <- pred_mu
plot_data$sigma <- pred_sigma

ggplot(plot_data, aes(x = year, y = mu)) +
  geom_count(data = ratings, mapping = aes(x = year, y = rating)) +
  geom_line(size = 1.25) +
  geom_line(aes(y = mu + sigma), linetype = 2) +
  geom_line(aes(y = mu - sigma), linetype = 2) +
  theme_light() +
  labs(x = "Year", y = "Rating") +
  scale_x_continuous(breaks = seq(1930, 2020, by = 10)) +
  theme(text = element_text(size = 16)) +
  ylim(c(0.5, 5))
