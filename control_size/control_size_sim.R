set.seed(1839)
iter <- 1000
alpha <- .05
# n <- seq(5000, 20000, 5000) # from preliminary sim
n <- c(1000, 2500, seq(5000, 20000, 5000))
ctl <- seq(.1, .5, by = .05)
# t0 <- seq(.3, .7, .2) # from preliminary sim
t0 <- .5
lift <- seq(.01, .10, by = .01)
results <- expand.grid(n = n, ctl = ctl, t0 = t0, lift = lift)
results$power <- NA

gen_dat <- function(n, ctl, t0, lift) {
  data.frame(
    y = c(rbinom(n * ctl, 1, t0), rbinom(n * (1 - ctl), 1, t0 + lift)),
    x = c(rep("t0", n * ctl), rep("t1", n * (1 - ctl)))
  )
}

get_pvalue <- function(data) summary(glm(y ~ x, binomial, data))$coef[2, 4]

for (i in seq_len(nrow(results))) {
  ps <- sapply(1:iter, function(zzz) {
    get_pvalue(
      gen_dat(results$n[i], results$ctl[i], results$t0[i], results$lift[i])
    )
  })
  results$power[i] <- mean(ps < alpha)
  print(i)
}
