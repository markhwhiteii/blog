library(ggplot)
library(betareg)
library(rstan)
library(bayesplot)
dat <- read.csv("betareg_politics.csv")
str(dat)

# ols, frequentist
cor(dat)[[2]]

ggplot(dat, aes(x = rw_pol_id, y = sdo)) +
  geom_jitter() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_light()

ggplot(dat, aes(x = sdo)) +
  geom_density() +
  theme_light()

mod0 <- lm(sdo ~ rw_pol_id, dat)

ggplot(mapping = aes(x = mod0$fitted.values, y = mod0$residuals)) +
  geom_jitter() +
  geom_smooth() +
  theme_light()

# beta, frequentist
beta_normalize <- function(x) {
  x_ <- ((x - min(x)) / (max(x) - min(x)))
  (x_ * (length(x_) - 1) + 0.5) / length(x_)
}
dat$sdo <- beta_normalize(dat$sdo)
range(dat$sdo)

mod1 <- betareg(sdo ~ rw_pol_id | rw_pol_id, dat, link = "logit", link.phi = "log")
summary(mod1)

# beta, bayesian
stan_code <- "
data {
  int n;
  vector[n] x; 
  vector<lower=0, upper=1>[n] y;
}
parameters {
  vector[4] coef;
}
transformed parameters {
  vector[n] mu;
  vector<lower=0>[n] phi;
  vector[n] p;
  vector[n] q;
  for (i in 1:n) {
    mu[i] = inv_logit(coef[1] + coef[2] * x[i]); 
    phi[i] = exp(coef[3] + coef[4] * x[i]);
    p[i] = mu[i] * phi[i];
    q[i] =  phi[i] - mu[i] * phi[i];
  }
}
model {
  y ~ beta(p, q);
  coef ~ normal(0, 2);
}
"

stan_dat <- list(n = nrow(dat), x = dat$rw_pol_id, y = dat$sdo)
set.seed(1839)
mod2 <- stan(model_code = stan_code, data = stan_dat, 
             iter = 1000, chains = 4, cores = 2)
round(summary(mod2)$summary[1:4, ], 3)

draws <- as.matrix(mod2)
draws <- draws[, c("coef[2]", "coef[4]")]
colnames(draws) <- c("mu", "phi")
mcmc_hist(draws, facet_args = list(labeller = label_parsed)) +
  ggtitle("Posterior densities for slope coefficients",
          "SDO regressed on right-wing political identification") +
  theme_light()

all_draws <- extract(mod2, pars = "coef", inc_warmup = TRUE, permuted = FALSE)
attributes(all_draws)$dimnames$parameters <- c(
  "mu[intercept]", "mu[slope]", "phi[intercept]", "phi[slope]"
)
mcmc_trace(all_draws, n_warmup = 500, facet_args = list(labeller = label_parsed)) +
  theme_light() +
  theme(legend.position = "none")

mod2_summary <- summary(mod2)$summary
mus <- mod2_summary[grepl("mu", rownames(mod2_summary)), "mean"]
ggplot() +
  geom_jitter(aes(x = dat$rw_pol_id, y = dat$sdo)) +
  geom_line(aes(x = dat$rw_pol_id, y = mus)) +
  theme_light() +
  labs(x = "Right-Wing Political ID", y = "E(SDO)")

phis <- mod2_summary[grepl("phi", rownames(mod2_summary)), "mean"]
vars <- (mus * (1 - mus)) / (phis + 1)
ggplot() +
  geom_line(aes(x = dat$rw_pol_id, y = vars)) +
  theme_light() +
  labs(x = "Right-Wing Political ID", y = "Var(SDO)")
