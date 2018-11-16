library(ggplot2)
set.seed(1839)
n <- 100000
x <- runif(n, .501, .999)
y <- rbinom(n, 1, x)
dat <- data.frame(x, y)

# what we want graph to look like
ggplot(dat, aes(x = x, y = y)) +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  geom_smooth(method.args = list(family = binomial()), 
              se = FALSE, color = "gold", method = "glm") +
  theme_minimal()

# what we want slope to be
coef(lm(y ~ x, dat))["x"]
