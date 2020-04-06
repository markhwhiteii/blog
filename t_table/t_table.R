#' Make a Summary Table of Multiple t-tests
#'
#' Generates a table that includes variable names, means and standard 
#' deviations for each condition, t-value and p-value, degrees of 
#' freedom, and Cohen's d with confidence intervals.
#' 
#' @param data A data.frame.
#' @param dvs String. Variable name(s) of the dependent variables.
#' @param iv String. Variable name for the independent variable.
#' @param var_equal Logical. Assume equal variance across conditions?
#' @param p_adj String. p-value adjustment to be done. Passed to p.adjust. See
#'   ?p.adjust.methods for more details. Note that p-values are adjusted, but
#'   the confidence intervals for Cohen's d are NOT.
#' @examples
#' set.seed(1839)
#' data <- data.frame(
#'   a = c(rnorm(50, 0, 2), rnorm(50, 1, 2)),
#'   b = c(rnorm(50, 0, 2), rnorm(50, 1, 2)),
#'   c = c(rnorm(50, 0, 2), rnorm(50, 1, 2)),
#'   d = factor(c(rep("cond 0", 50), rep("cond 1", 50)))
#' )
#' t_table(data = data, dvs = c("a", "b", "c"), iv = "d")
#' @export
t_table <- function(data, dvs, iv, var_equal = TRUE, p_adj = "none") {
  
  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame")
  }
  
  if (!all(c(dvs, iv) %in% names(data))) {
    stop("at least one column given in dvs and iv are not in the data")
  }
  
  if (!all(sapply(data[, dvs], is.numeric))) {
    stop("all dvs must be numeric")
  }
  
  if (length(unique(na.omit(data[[iv]]))) != 2) {
    stop("independent variable must only have two unique values")
  }
  
  out <- lapply(dvs, function(x) {
    
    tres <- t.test(data[[x]] ~ data[[iv]], var.equal = var_equal)
    
    mns <- tapply(data[[x]], data[[iv]], mean, na.rm = TRUE)
    names(mns) <- paste0(names(mns), "_m")
    
    sds <- tapply(data[[x]], data[[iv]], sd, na.rm = TRUE)
    names(sds) <- paste0(names(sds), "_sd")
    
    es <- MBESS::ci.smd(ncp = tres$statistic, 
                        n.1 = table(data[[iv]])[[1]], 
                        n.2 = table(data[[iv]])[[2]])
    
    c(
      c(mns[1], sds[1], mns[2], sds[2]),
      tres$statistic,
      tres$parameter,
      p = tres$p.value,
      d = unname(es$smd),
      d_lb = es$Lower,
      d_ub = es$Upper
    )
  })
  
  out <- as.data.frame(do.call(rbind, out))
  out <- cbind(variable = dvs, out)
  names(out) <- gsub("[^0-9A-Za-z_]", "", names(out))
  
  out$p <- p.adjust(out$p, p_adj)
  
  return(out)
}
