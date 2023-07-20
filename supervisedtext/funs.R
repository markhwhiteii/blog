# funs -------------------------------------------------------------------------
library(textrecipes)
library(vip)
library(stopwords)
library(tidymodels)
library(workflowsets)
library(tidyverse)

# prepare data for modeling, do train/test split
# default at a holdout set of 15%
# stratify on the outcome variable
prep_data <- function(dat, y, txt, prop) {
  dat %>%
    transmute(text = .data[[txt]], y = factor(.data[[y]])) %>%
    initial_split(prop, strata = y)
}

# do cross-validation with the same, pre-defined, hard-coded engines and recipes
# n_folds is number of folds; grid_size is the size of the grid
do_cv <- function(dat_train, n_folds, grid_size) {
  
  # define recipes -------------------------------------------------------------
  ## base ----------------------------------------------------------------------
  rec_base <- recipe(y ~ text, dat_train)
  
  ## tokenize ------------------------------------------------------------------
  rec_word_nostop <- rec_base %>%
    step_tokenize(
      text,
      token = "words"
    )
  
  rec_word_smart <- rec_base %>%
    step_tokenize(
      text,
      token = "words",
      options = list(stopwords = stopwords(source = "smart"))
    )
  
  rec_word_iso <- rec_base %>%
    step_tokenize(
      text,
      token = "words",
      options = list(stopwords = stopwords(source = "stopwords-iso"))
    )
  
  rec_both_nostop <- rec_base %>%
    step_tokenize(
      text,
      token = "skip_ngrams",
      options = list(
        n = 2,
        k = 0
      )
    )
  
  rec_both_smart <- rec_base %>%
    step_tokenize(
      text,
      token = "skip_ngrams",
      options = list(
        stopwords = stopwords(source = "smart"),
        n = 2,
        k = 0
      )
    )
  
  rec_both_iso <- rec_base %>%
    step_tokenize(
      text,
      token = "skip_ngrams",
      options = list(
        stopwords = stopwords(source = "stopwords-iso"),
        n = 2,
        k = 0
      )
    )
  
  ## stem ----------------------------------------------------------------------
  rec_word_nostop_stemmed <- rec_word_nostop %>%
    step_stem(text)
  
  rec_word_smart_stemmed <- rec_word_smart %>%
    step_stem(text)
  
  rec_word_iso_stemmed <- rec_word_iso %>%
    step_stem(text)
  
  rec_both_nostop_stemmed <- rec_both_nostop %>%
    step_stem(text)
  
  rec_both_smart_stemmed <- rec_both_smart %>%
    step_stem(text)
  
  rec_both_iso_stemmed <- rec_both_iso %>%
    step_stem(text)
  
  ## filter, weight ------------------------------------------------------------
  rec_word_nostop_f2 <- rec_word_nostop %>%
    step_tokenfilter(text, min_times = 2, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_smart_f2 <- rec_word_smart %>%
    step_tokenfilter(text, min_times = 2, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_iso_f2 <- rec_word_iso %>%
    step_tokenfilter(text, min_times = 2, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_nostop_f2 <- rec_both_nostop %>%
    step_tokenfilter(text, min_times = 2, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_smart_f2 <- rec_both_smart %>%
    step_tokenfilter(text, min_times = 2, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_iso_f2 <- rec_both_iso %>%
    step_tokenfilter(text, min_times = 2, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_nostop_stemmed_f2 <- rec_word_nostop_stemmed %>%
    step_tokenfilter(text, min_times = 2, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_smart_stemmed_f2 <- rec_word_smart_stemmed %>%
    step_tokenfilter(text, min_times = 2, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_iso_stemmed_f2 <- rec_word_iso_stemmed %>%
    step_tokenfilter(text, min_times = 2, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_nostop_stemmed_f2 <- rec_both_nostop_stemmed %>%
    step_tokenfilter(text, min_times = 2, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_smart_stemmed_f2 <- rec_both_smart_stemmed %>%
    step_tokenfilter(text, min_times = 2, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_iso_stemmed_f2 <- rec_both_iso_stemmed %>%
    step_tokenfilter(text, min_times = 2, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_nostop_f5 <- rec_word_nostop %>%
    step_tokenfilter(text, min_times = 5, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_smart_f5 <- rec_word_smart %>%
    step_tokenfilter(text, min_times = 5, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_iso_f5 <- rec_word_iso %>%
    step_tokenfilter(text, min_times = 5, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_nostop_f5 <- rec_both_nostop %>%
    step_tokenfilter(text, min_times = 5, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_smart_f5 <- rec_both_smart %>%
    step_tokenfilter(text, min_times = 5, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_iso_f5 <- rec_both_iso %>%
    step_tokenfilter(text, min_times = 5, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_nostop_stemmed_f5 <- rec_word_nostop_stemmed %>%
    step_tokenfilter(text, min_times = 5, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_smart_stemmed_f5 <- rec_word_smart_stemmed %>%
    step_tokenfilter(text, min_times = 5, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_iso_stemmed_f5 <- rec_word_iso_stemmed %>%
    step_tokenfilter(text, min_times = 5, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_nostop_stemmed_f5 <- rec_both_nostop_stemmed %>%
    step_tokenfilter(text, min_times = 5, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_smart_stemmed_f5 <- rec_both_smart_stemmed %>%
    step_tokenfilter(text, min_times = 5, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_iso_stemmed_f5 <- rec_both_iso_stemmed %>%
    step_tokenfilter(text, min_times = 5, max_tokens = 5000) %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_nostop <- rec_word_nostop %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_smart <- rec_word_smart %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_iso <- rec_word_iso %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_nostop <- rec_both_nostop %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_smart <- rec_both_smart %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_iso <- rec_both_iso %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_nostop_stemmed <- rec_word_nostop_stemmed %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_smart_stemmed <- rec_word_smart_stemmed %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_word_iso_stemmed <- rec_word_iso_stemmed %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_nostop_stemmed <- rec_both_nostop_stemmed %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_smart_stemmed <- rec_both_smart_stemmed %>%
    step_tf(text, weight_scheme = "binary")
  
  rec_both_iso_stemmed <- rec_both_iso_stemmed %>%
    step_tf(text, weight_scheme = "binary")
  
  ## define specs --------------------------------------------------------------
  spec_elasticnet <- logistic_reg(
    mode = "classification",
    engine = "glmnet",
    penalty = tune(),
    mixture = tune()
  )
  
  spec_randforest <- rand_forest(
    mode = "classification",
    mtry = tune(),
    min_n = tune(),
    trees = 500
  ) %>%
    set_engine(engine = "ranger", importance = "impurity")
  
  # make workflowset -----------------------------------------------------------
  wfs <- workflow_set(
    preproc = list(
      word_nostop_nostem_f2 = rec_word_nostop_f2,
      word_smart_nostem_f2 = rec_word_smart_f2,
      word_iso_nostem_f2 = rec_word_iso_f2,
      both_nostop_nostem_f2 = rec_both_nostop_f2,
      both_smart_nostem_f2 = rec_both_smart_f2,
      both_iso_nostem_f2 = rec_both_iso_f2,
      word_nostop_stemmed_f2 = rec_word_nostop_stemmed_f2,
      word_smart_stemmed_f2 = rec_word_smart_stemmed_f2,
      word_iso_stemmed_f2 = rec_word_iso_stemmed_f2,
      both_nostop_stemmed_f2 = rec_both_nostop_stemmed_f2,
      both_smart_stemmed_f2 = rec_both_smart_stemmed_f2,
      both_iso_stemmed_f2 = rec_both_iso_stemmed_f2,
      word_nostop_nostem_f5 = rec_word_nostop_f5,
      word_smart_nostem_f5 = rec_word_smart_f5,
      word_iso_nostem_f5 = rec_word_iso_f5,
      both_nostop_nostem_f5 = rec_both_nostop_f5,
      both_smart_nostem_f5 = rec_both_smart_f5,
      both_iso_nostem_f5 = rec_both_iso_f5,
      word_nostop_stemmed_f5 = rec_word_nostop_stemmed_f5,
      word_smart_stemmed_f5 = rec_word_smart_stemmed_f5,
      word_iso_stemmed_f5 = rec_word_iso_stemmed_f5,
      both_nostop_stemmed_f5 = rec_both_nostop_stemmed_f5,
      both_smart_stemmed_f5 = rec_both_smart_stemmed_f5,
      both_iso_stemmed_f5 = rec_both_iso_stemmed_f5,
      word_nostop_nostem_f0 = rec_word_nostop,
      word_smart_nostem_f0 = rec_word_smart,
      word_iso_nostem_f0 = rec_word_iso,
      both_nostop_nostem_f0 = rec_both_nostop,
      both_smart_nostem_f0 = rec_both_smart,
      both_iso_nostem_f0 = rec_both_iso,
      word_nostop_stemmed_f0 = rec_word_nostop_stemmed,
      word_smart_stemmed_f0 = rec_word_smart_stemmed,
      word_iso_stemmed_f0 = rec_word_iso_stemmed,
      both_nostop_stemmed_f0 = rec_both_nostop_stemmed,
      both_smart_stemmed_f0 = rec_both_smart_stemmed,
      both_iso_stemmed_f0 = rec_both_iso_stemmed
    ),
    models = list(
      elasticnet = spec_elasticnet,
      randforest = spec_randforest
    ),
    cross = TRUE
  )
  
  # do cross-validation
  folds <- vfold_cv(dat_train, v = n_folds)
  
  # get result of cross-validation
  cv_res <- wfs %>%
    workflow_map(
      "tune_grid",
      grid = grid_size,
      resamples = folds,
      metrics = metric_set(
        accuracy,
        sensitivity,
        specificity,
        precision,
        f_meas,
        roc_auc
      ),
      verbose = TRUE
    )
  
  # return the entire workflow set and the results of cross-validation
  return(list(wfs = wfs, cv_res = cv_res))
}

# take results from cross-validation, get a final model and held-out metrics
best_model <- function(dat, cv_out, metric = "roc_auc") {
  
  # get the id of the best model, as according to the specified metric
  best_id <- cv_out$cv_res %>%
    rank_results(rank_metric = metric) %>%
    filter(.metric == metric & rank == 1) %>%
    pull(wflow_id)
  
  # get the name of the model
  # if you name the workflow sets differently, this step will change
  # it's based on the hardcoded names I gave them in defining the workflow set
  m <- str_split(best_id, "_")[[1]][[5]]
  
  # get best parameters, do final fit
  best_params <- cv_out$cv_res %>%
    extract_workflow_set_result(best_id) %>%
    select_best(metric = metric)
  
  final_fit <- cv_out$cv_res %>%
    extract_workflow(best_id) %>%
    finalize_workflow(best_params) %>%
    fit(training(dat))
  
  # run it on the holdout data
  dat_test <- testing(dat)
  
  # if glmnet, feed it the right penalty
  # I'm pretty sure this is necessary,
  # because glmnet fits many lambda as it's more efficient
  if (m == "elasticnet") {
    dat_test <- bind_cols(
      dat_test,
      predict(final_fit, dat_test, penalty = best_params$penalty),
      predict(final_fit, dat_test, type = "prob", penalty = best_params$penalty)
    )
  } else {
    dat_test <- bind_cols(
      dat_test,
      predict(final_fit, dat_test),
      predict(final_fit, dat_test, type = "prob")
    )
  }
  
  # define metrics to output
  ms <- metric_set(accuracy, sensitivity, specificity, precision)
  
  # return metrics
  metrics_res <- ms(dat_test, truth = y, estimate = .pred_class) %>%
    select(-.estimator) %>%
    spread(.metric, .estimate) %>%
    mutate(
      est_pct_class = mean(dat_test$.pred_class == 1),
      est_pct_prob = mean(dat_test$.pred_1),
      act_pct = mean(dat_test$y == 1)
    )
  
  # get variable importance, which depends on the model
  if (m == "randforest") {
    var_imp <- final_fit %>%
      extract_fit_parsnip() %>%
      vi() %>%
      mutate(
        Variable = str_remove_all(Variable, "tf_text_"),
        Variable = factor(Variable, Variable)
      )
  } else if (m == "elasticnet") {
    var_imp <- final_fit %>%
      tidy() %>%
      filter(
        penalty == best_params$penalty &
          term != "(Intercept)" &
          estimate > 0
      ) %>%
      arrange(desc(abs(estimate))) %>%
      select(-penalty) %>%
      mutate(
        term = str_remove_all(term, "tf_text_"),
        term = factor(term, term)
      )
  } else {
    # if you add more models, this is where you'd change the code to get
    # variable importance for that specific class of model output
    var_imp <- NA
  }
  
  return(
    list(
      best_id = best_id,
      best_params = best_params,
      var_imp = var_imp,
      fit = final_fit,
      ho_metrics = metrics_res
    )
  )
}
