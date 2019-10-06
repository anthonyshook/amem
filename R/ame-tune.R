#' Bayesian optimization of amem model
#'
#' @param formula As with \code{lme4::lmer}, a two-sided linear formula object describing both the fixed-effects and random-effects part of the model, with the response on the left of a ~ operator and the terms, separated by + operators, on the right. Random-effects terms are distinguished by vertical bars (|) separating expressions for design matrices from grouping factors.
#' @param data a data.frame containing the variables named in \code{formula}
#' @param nfolds Number of folds for cross-validation (Required)
#' @param val_data a data.frame, identical to \code{data}.  Optional, and separate from tuning. If provided, nfolds is ignored.
#' @param tuning_params A list containing the parameters to tune, and the bounds of the search space for xgboost (see \code{?xgb.train} for descriptions. Currently accepts:
#' \itemize{
#'    \item \code{nrounds} Default c(20, 200)
#'    \item \code{max_depth} Default = c(2L, 6L)
#'    \item \code{min_child_weight} Default = c(1L, 10L)
#'    \item \code{eta} Default = c(.1, .5)
#'    \item \code{gamma} Default = 1 (not tuned).
#'    \item \code{colsample_bytree} Default = 1 (not tuned)
#'    \item \code{subsample} Default = 1 (not tuned)
#' }
#' @param seed Used for generating cross-validation folds (Default uses system time)
#'
amem.tune <- function(formula,
                      data,
                      nfolds,
                      val_data = NULL,
                      tuning_params = NULL,
                      seed = as.integer(Sys.time())) {

  # Checks
  if (is.null(val_data) & nfolds <= 1) {
    stop("Tuning requires val_data or nfolds > 1")
  }

  # Check the tuning params
  tuneParams <- check_tuning_params(tparams = tuning_params)


  # Setup the parameter tuning function
  amem_bayes_val <- function(nrounds,
                             max_depth,
                             min_child_weight,
                             eta,
                             colsample_bytree,
                             gamma,
                             subsample) {

    opt_params <- list(eta = eta,
                       max_depth = max_depth,
                       min_child_weight = min_child_weight,
                       subsample = subsample,
                       colsample_bytree = colsample_bytree,
                       gamma = gamma)

    training_mod <- amem(formula = formula, data = data,
                         max_iter = nrounds,
                         fixed_options = list(params = c(opt_params, eval_metric = amem_mae)), chatty = TRUE)

    preds <- training_mod@predictions$validation
    score <- training_mod@model_info@final_score

    return(list(Score = 1/score * 100, Preds = preds))
  }

  if (is.null(val_data)) {
    # Define the folds
    cv_folds <- define_cv_folds(num_rows = nrows(data), nfolds = nfolds, seed = seed)

  } else {
    #
    OPT_Res <- rBayesianOptimization::BayesianOptimization(amem_bayes_val,
                                                           bounds = tuneParams,
                                                           init_grid_dt = NULL, init_points = 10, n_iter = 20,
                                                           acq = "ucb", kappa = 2.576, eps = 0.0,
                                                           verbose = TRUE)
  }

  return(OPT_Res)
}


#
# # Example 2: Parameter Tuning
# cv_folds <- KFold(agaricus.train$label, nfolds = 5,
#                   stratified = TRUE, seed = 0)
# xgb_cv_bayes <- function(max.depth, min_child_weight, subsample) {
#   cv <- xgb.cv(params = list(booster = "gbtree", eta = 0.01,
#                              max_depth = max.depth,
#                              min_child_weight = min_child_weight,
#                              subsample = subsample, colsample_bytree = 0.3,
#                              lambda = 1, alpha = 0,
#                              objective = "binary:logistic",
#                              eval_metric = "auc"),
#                data = dtrain, nround = 100,
#                folds = cv_folds, prediction = TRUE, showsd = TRUE,
#                early.stop.round = 5, maximize = TRUE, verbose = 0)
#   list(Score = cv$dt[, max(test.auc.mean)],
#        Pred = cv$pred)
# }
# OPT_Res <- BayesianOptimization(xgb_cv_bayes,
#                                 bounds = list(max.depth = c(2, 4),
#                                               min_child_weight = c(1L, 10L),
#                                               subsample = c(0.5, 0.8)),
#                                 init_grid_dt = NULL, init_points = 10, n_iter = 20,
#                                 acq = "ucb", kappa = 2.576, eps = 0.0,
#                                 verbose = TRUE)
