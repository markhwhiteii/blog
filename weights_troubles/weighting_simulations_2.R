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

get_targets <- function(pop, dat, wrong = 0) {
  tmp1 <- as.data.frame(prop.table(table(pop$V1)))
  tmp1$Freq <- (tmp1$Freq + c(-wrong, wrong)) * nrow(dat)
  names(tmp1)[1] <- "V1"
  tmp2 <- as.data.frame(prop.table(table(pop$V2)))
  tmp2$Freq <- (tmp2$Freq + c(-wrong, wrong)) * nrow(dat)
  names(tmp2)[1] <- "V2"
  
  return(list(tmp1, tmp2))
}

flip_demo <- function(x, p) ifelse(rbinom(length(x), 1, p), !x, x)

f <- y ~ 1

run_iter <- function(pop, non_rand_p = c(.065, .005, .005, .005),
                     p_error = c(0, .15), p_wrong = c(0, .15)) {
  
  pop_est <- summary(glm(f, binomial, pop))$coef[[1]]
  
  # get some probabilities for measurement error and wrong targets:
  p1 <- runif(1, p_error[1], p_error[2])
  p2 <- runif(1, p_wrong[1], p_wrong[2])
  
  dat <- get_sample(pop, non_rand_p)
  # introduce measurement error to sample:
  dat <- dplyr::mutate_at(dat, dplyr::vars(V1, V2), flip_demo, p1)
  
  result <- invisible(suppressWarnings(survey::svydesign(~ 1, data = dat)))
  result <- survey::rake(result, list(~V1, ~V2), get_targets(pop, dat, p2))
  wtd_svy <- suppressWarnings(survey::svyglm(f, result, family = binomial))
  result <- survey::as.svrepdesign(result, type = "bootstrap")
  wtd_rep <- suppressWarnings(survey::svyglm(f, result, family = binomial))
  
  out <- suppressWarnings(suppressMessages(
    list(
      p_error = p1,
      p_wrong = p2,
      sandwich_coverage = is_between(pop_est, confint(wtd_svy)),
      bootstrap_coverage = is_between(pop_est, confint(wtd_rep))
    )
  ))
  
  return(out)
}

# get results ------------------------------------------------------------------
set.seed(1839)
iter <- 20000
results <- lapply(seq_len(iter), function(i) {
  print(paste("Starting iter", i))
  as.data.frame(run_iter(sim_pop()))
})
results <- do.call(dplyr::bind_rows, results)
# readr::write_csv(results, "sim_results_2.csv")
