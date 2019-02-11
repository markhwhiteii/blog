# define functions -------------------------------------------------------------
inv_logit <- function(x) exp(x) / (exp(x) + 1)

sim_pop <- function(N = 5000, b_min = 0, b_max = 1.5) {
  X <- cbind(1, rbinom(N, 1, .5), rbinom(N, 1, .5))
  B <- c(0, runif(1, b_min, b_max), runif(1, b_min, b_max))
  y <- rbinom(N, 1, inv_logit(X %*% B))
  
  return(as.data.frame(cbind(X[, -1], y)))
}

get_sample <- function(pop, p = c(.02, .02, .02, .02)) {
  p <- ifelse(rowSums(pop[, 1:2]) == 2, p[1],
         ifelse(rowSums(pop[, 1:2]) == 0, p[2],
                ifelse(pop[, 1] == 1, p[3], p[4])))
  sampled <- as.logical(rbinom(nrow(pop), 1, p))
  
  return(pop[sampled, ])
}

is_between <- function(x, r) x >= min(r) & x <= max(r)

get_targets <- function(pop, dat) {
  tmp1 <- as.data.frame(prop.table(table(pop$V1)))
  tmp1$Freq <- tmp1$Freq * nrow(dat)
  names(tmp1)[1] <- "V1"
  tmp2 <- as.data.frame(prop.table(table(pop$V2)))
  tmp2$Freq <- tmp2$Freq * nrow(dat)
  names(tmp2)[1] <- "V2"
  
  return(list(tmp1, tmp2))
}

f <- y ~ 1

boot_confint <- function(dat, iter = 1000) {
  est <- suppressWarnings(coef(glm(f, binomial, dat, wts)))[[1]]
  rs <- sapply(seq_len(iter), function(zzz) {
    cases <- sample(seq_len(nrow(dat)), nrow(dat), TRUE)
    suppressWarnings(coef(glm(f, binomial, dat[cases, ], wts)))[[1]]
  })
  c(est - 1.96 * sd(rs), est + 1.96 * sd(rs))
}

run_iter <- function(pop, non_rand_p = c(.065, .005, .005, .005)) {
  pop_est <- summary(glm(f, binomial, pop))$coef[[1]]
  random <- glm(f, binomial, get_sample(pop))
  
  dat <- get_sample(pop, non_rand_p)
  non_random <- glm(f, binomial, dat)
  
  result <- invisible(suppressWarnings(survey::svydesign(~ 1, data = dat)))
  result <- survey::rake(result, list(~V1, ~V2), get_targets(pop, dat))
  dat$wts <- weights(result)
  wtd_glm <- suppressWarnings(glm(f, binomial, dat, wts))
  wtd_svy <- suppressWarnings(survey::svyglm(y ~ 1, result, family = binomial))
  
  result <- survey::as.svrepdesign(result, type = "bootstrap")
  wtd_rep <- suppressWarnings(survey::svyglm(y ~ 1, result, family = binomial))
  
  out <- suppressWarnings(suppressMessages(
    list(
      pop_est = pop_est,
      random_coverage = is_between(pop_est, confint(random)),
      random_est = random$coef[[1]],
      nonrandom_coverage = is_between(pop_est, confint(non_random)),
      nonrandom_est = non_random$coef[[1]],
      wtdglm_coverage = is_between(pop_est, confint(wtd_glm)),
      wtdglm_est = wtd_glm$coef[[1]],
      # uncomment below to try homemade bootstrapping function;
      # very slow, use less iter.
      # wtdboo_coverage = is_between(pop_est, boot_confint(dat)),
      wtdsvy_coverage = is_between(pop_est, confint(wtd_svy)),
      wtdsvy_est = wtd_svy$coef[[1]],
      wtdrep_coverage = is_between(pop_est, confint(wtd_rep)),
      wtdrep_est = wtd_rep$coef[[1]]
    )
  ))
  
  return(out)
}

# get results ------------------------------------------------------------------
set.seed(1839)
iter <- 10000 # 1000 for with my bootstrapping function included
results <- lapply(seq_len(iter), function(i) {
  print(paste("Starting iter", i))
  as.data.frame(run_iter(sim_pop()))
})
results <- do.call(bind_rows, results)
# readr::write_csv(results, "sim_results_withmybs.csv")
readr::write_csv(results, "sim_results.csv")
