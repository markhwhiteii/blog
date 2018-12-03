library(rvest)
library(tidyverse)
library(betareg)
# three point percentage -------------------------------------------------------
years <- 1980:2019
results <- lapply(years, function(y) {
  out <- paste0(
    "https://www.basketball-reference.com/leagues/NBA_", y, ".html"
  ) %>% 
    read_html() %>% 
    html_node(xpath = '//*[@id="all_team-stats-base"]/comment()') %>% 
    html_text() %>% 
    read_html() %>% 
    html_table() %>% 
    getElement(1) %>% 
    as_tibble() %>% 
    mutate(threep = `3PA` / FGA) %>% 
    pull(threep) %>% 
    `[`(-length(.))
  c(out, rep(NA, 30 - length(out))) # to make comformable columns
})
names(results) <- years
results <- as_tibble(results) %>%
  gather("year", "prop", na.rm = TRUE) %>% 
  mutate(year = as.numeric(year))

fig1 <- ggplot(results, aes(x = year, y = prop)) +
  theme_minimal() +
  geom_point(size = 2, color = "#17408B") +
  geom_smooth(
    data = {
      results %>% 
        group_by(year) %>% 
        summarise(prop = min(prop))
    }, 
    mapping = aes(x = year, y = prop), 
    size = 1, color = "#C9082A", linetype = 2,
    method = "loess", se = FALSE
  ) +
  geom_smooth(
    data = {
      results %>% 
        group_by(year) %>% 
        summarise(prop = max(prop))
    }, 
    mapping = aes(x = year, y = prop), 
    size = 1, color = "#C9082A", linetype = 2,
    method = "loess", se = FALSE
  ) +
  theme(
    text = element_text(size = 14, color = "#005858"),
    axis.text = element_text(color = "#005858")
  ) +
  labs(x = "\nYear", y = "FGA / 3PA\n")

mod <- betareg(prop ~ year | year, results)
summary(mod)
pacf(mod$residuals)

results2 <- results %>% 
  group_by(year) %>% 
  summarise(var = var(prop))

mod2 <- lm(var ~ year, results2)
acf(mod2$residuals)
pacf(mod2$residuals)
ar1 <- acf(mod2$residuals)$acf[2]
mod3 <- nlme::gls(var ~ year, results2, nlme::corAR1(ar1))
summary(mod3)
hist(mod3$residuals)
plot(mod3$fitted, mod3$residuals)
mod4 <- nlme::gls(var ~ poly(year, 2), results2, nlme::corAR1(ar1))
summary(mod4)

fig2 <- ggplot(results2, aes(x = year, y = var)) +
  theme_minimal() +
  geom_point(size = 2, color = "#17408B") +
  theme(
    text = element_text(size = 14, color = "#005858"),
    axis.text.y = element_text(angle = 90, hjust = .5, color = "#005858"),
    axis.text.x = element_text(color = "#005858"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(x = "\nYear", y = "3P Shooting Heterogeneity") +
  scale_y_continuous(
    breaks = c(.00001, .0024), 
    labels = c("Less", "More")
  ) +
  geom_line(aes(x = year, y = predict(mod3)), size = 1, color = "#C9082A") +
  geom_line(
    aes(x = year, y = predict(mod4)), 
    size = 1, color = "#C9082A", linetype = 2
  )

fig1; fig2

results %>% 
  group_by(year) %>% 
  mutate(prop = scale(prop)) %>% 
  arrange(prop)

# replicate with pace and assists per 100 possessions
# pace -------------------------------------------------------------------------
results3 <- lapply(years, function(y) {
  out <- paste0(
    "https://www.basketball-reference.com/leagues/NBA_", y, ".html"
  ) %>% 
    read_html() %>% 
    html_node(xpath = '//*[@id="all_misc_stats"]/comment()') %>% 
    html_text() %>% 
    read_html() %>% 
    html_table() %>% 
    getElement(1) %>% 
    `[[`(13) %>% 
    `[`(-c(1, length(.))) %>% 
    as.numeric()
  c(out, rep(NA, 30 - length(out))) # to make comformable columns
})
names(results3) <- years
results3 <- as_tibble(results3) %>%
  gather("year", "pace", na.rm = TRUE) %>% 
  mutate(year = as.numeric(year))

ggplot(results3, aes(x = year, y = pace)) +
  theme_minimal() +
  geom_point(size = 2, color = "#17408B") +
  geom_smooth(
    data = {
      results3 %>% 
        group_by(year) %>% 
        summarise(pace = min(pace))
    }, 
    mapping = aes(x = year, y = pace), 
    size = 1, color = "#C9082A", linetype = 2,
    method = "loess", se = FALSE
  ) +
  geom_smooth(
    data = {
      results3 %>% 
        group_by(year) %>% 
        summarise(pace = max(pace))
    }, 
    mapping = aes(x = year, y = pace), 
    size = 1, color = "#C9082A", linetype = 2,
    method = "loess", se = FALSE
  ) +
  theme(
    text = element_text(size = 14, color = "#005858"),
    axis.text = element_text(color = "#005858")
  ) +
  labs(x = "\nYear", y = "Pace\n")

results4 <- results3 %>% 
  group_by(year) %>% 
  summarise(var = var(pace))

mod5 <- lm(var ~ year, results4)
acf(mod5$residuals)
pacf(mod5$residuals)
ar1_mod5 <- acf(mod5$residuals)$acf[2]
mod6 <- nlme::gls(var ~ year, results4, nlme::corAR1(ar1_mod5))
summary(mod6)
hist(mod6$residuals)
plot(mod6$fitted, mod6$residuals)

ggplot(results4, aes(x = year, y = var)) +
  theme_minimal() +
  geom_point(size = 2, color = "#17408B") +
  theme(
    text = element_text(size = 14, color = "#005858"),
    axis.text.y = element_text(angle = 90, hjust = .5, color = "#005858"),
    axis.text.x = element_text(color = "#005858"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(x = "\nYear", y = "Pace Heterogeneity") +
  scale_y_continuous(
    breaks = c(3, 17.5), 
    labels = c("Less", "More")
  ) +
  geom_line(aes(x = year, y = predict(mod6)), size = 1, color = "#C9082A")

# assists per 100 possessions --------------------------------------------------
results5 <- lapply(years, function(y) {
  out <- paste0(
    "https://www.basketball-reference.com/leagues/NBA_", y, ".html"
  ) %>% 
    read_html() %>% 
    html_node(xpath = '//*[@id="all_team-stats-per_poss"]/comment()') %>% 
    html_text() %>% 
    read_html() %>% 
    html_table() %>% 
    getElement(1) %>% 
    pull(AST)
  c(out, rep(NA, 30 - length(out))) # to make comformable columns
})
names(results5) <- years
results5 <- as_tibble(results5) %>%
  gather("year", "ast", na.rm = TRUE) %>% 
  mutate(year = as.numeric(year))

ggplot(results5, aes(x = year, y = ast)) +
  theme_minimal() +
  geom_point(size = 2, color = "#17408B") +
  geom_smooth(
    data = {
      results5 %>% 
        group_by(year) %>% 
        summarise(ast = min(ast))
    }, 
    mapping = aes(x = year, y = ast), 
    size = 1, color = "#C9082A", linetype = 2,
    method = "loess", se = FALSE
  ) +
  geom_smooth(
    data = {
      results5 %>% 
        group_by(year) %>% 
        summarise(ast = max(ast))
    }, 
    mapping = aes(x = year, y = ast), 
    size = 1, color = "#C9082A", linetype = 2,
    method = "loess", se = FALSE
  ) +
  theme(
    text = element_text(size = 14, color = "#005858"),
    axis.text = element_text(color = "#005858")
  ) +
  labs(x = "\nYear", y = "Assists Per 100 Possessions\n")

results6 <- results5 %>% 
  group_by(year) %>% 
  summarise(var = var(ast))

mod7 <- lm(var ~ poly(year, 3), results6)
acf(mod7$residuals)
pacf(mod7$residuals)
summary(mod7)

ggplot(results6, aes(x = year, y = var)) +
  theme_minimal() +
  geom_point(size = 2, color = "#17408B") +
  theme(
    text = element_text(size = 14, color = "#005858"),
    axis.text.y = element_text(angle = 90, hjust = .5, color = "#005858"),
    axis.text.x = element_text(color = "#005858"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(x = "\nYear", y = "Assist Rate Heterogeneity") +
  scale_y_continuous(
    breaks = c(2, 6), 
    labels = c("Less", "More")
  ) +
  geom_line(aes(x = year, y = predict(mod7)), size = 1, color = "#C9082A")
