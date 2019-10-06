#' Function for fitting adaptable mixed effects models
#'
#' @param formula As with \code{lme4::lmer}, a two-sided linear formula object describing both the fixed-effects and random-effects part of the model, with the response on the left of a ~ operator and the terms, separated by + operators, on the right. Random-effects terms are distinguished by vertical bars (|) separating expressions for design matrices from grouping factors.
#' @param data a data.frame containing the variables named in \code{formula}
#' @param val_data a data.frame, identical to data; If provided, early_stopping uses validation set.
#' @param method The method to use for fixed-effects -- currently not implemented.
#' @param fixed_options List of params to apply to fixed calculation.   See \code{xgb.train}
#' @param min_iter The minimum number of iterations for the optimization procedure (Default = 10)
#' @param max_iter The maximum number of iterations for the optimization procedure (Default = 20).
#' @param stopping_criteria Early stopping criteria, dependent on outcome. (Default = 1e-5)
#' @param keep_training_data Logical. If TRUE, training data is stored in the model object.
#' @param chatty Whether to be verbose or not (Default: TRUE)
#' @param na_action What to do with NAs (Default = \code{na.pass})
#'
#' @details Note that although the ame-fit funtion accepts formulas with complex random-slope effects, this functionality is experimental, and the slope effect of a given feature may not accurately represent the fixed-effect of that same feature. In addition, not every complex method of defining random effects has been fully tested.\code{max_iter} is used to define the value of \code{xgboost::nrounds} - if 'nrounds' is provided in the \code{fixed_options}, it will override the value of max_iter.
#'
#' @return an ameMod object
#'
#' @export
#'
#' @include default-parameters.R
amem <- function(formula,
                 data,
                 val_data = NULL,
                 method = "xgboost",
                 fixed_options = list(),
                 min_iter = 10,
                 max_iter = 20,
                 stopping_criteria = 1e-5,
                 keep_training_data = TRUE,
                 chatty = TRUE,
                 na_action = na.pass) {

  # Require the XGBOOST namespace
  requireNamespace("xgboost", quietly = TRUE)

  # Get the call for later
  matchCall <- match.call(expand.dots = TRUE)
  matchCall$formula <- formula

  # Parse input parameters
  # NB. we don't accept changes to the random options.
  checked_fixed_opts <- parse_fixed_options(fixed_options)
  checked_random_opts <- default_lmer_options

  # if formula argument is class "character", try to coerce
  if (class(formula) == "character") {
    tryCatch(formula <- stats::as.formula(formula),
             error = function(e){
               message("Error coercing 'character' to 'formula':")
               stop(e)
             })
  }

  ##### Parsing formula #####
  # Parsing formula for later
  all_terms    <- attr(terms(formula, data = data), "term.labels")
  fixed_terms  <- all_terms[!grepl(all_terms, pattern = "|", fixed = TRUE)]
  random_terms <- all_terms[grepl(all_terms, pattern = "|", fixed = TRUE)]

  # Getting training data
  training_data <- create_internal_data(dat = data, formula = formula)

  if (!is.null(val_data)) {
    validation_set <- create_internal_data(dat = val_data, formula = formula)
    wtchlst <- list(train = training_data, val = validation_set)
    metric_name <- "val"
  } else {
    wtchlst <- list(train = training_data)
    metric_name = "train"
  }

  ##### Generating predictions #####
  training_mod <- xgboost::xgb.train(data = training_data, nrounds = max_iter,
                                     watchlist = wtchlst,
                                     verbose = as.integer(chatty),
                                     maximize = FALSE,
                                     params = checked_fixed_opts$params,
                                     callbacks = list(cb.amem(),
                                                      cb.early.stop.amem(stopping_criteria = stopping_criteria,
                                                                         min_iter = min_iter,
                                                                         metric_name = metric_name)))
  # Get the model info
  running_ameInfo <- ameInfo(final_score = training_mod$best_score,
                             converged = FALSE,
                             num_iters = as.integer(training_mod$niter),
                             training_info = training_mod$evaluation_log)

  # Get the residuals
  # Fixed effect predictions
  feff_preds <- stats::predict(training_mod, newdata = training_data)
  rand_coefs_Bi <- lme4::getME(attr(training_mod, 'lmerMod'), "mu")

  # This is my residual error (episilon), which should approximate 0
  inbag_epsilon <- attr(training_data, 'origLabel') - feff_preds - rand_coefs_Bi

  # Final model to return
  final_mod <- ameMod(call = matchCall,
                      model_info = running_ameInfo,
                      fixef_model = training_mod,
                      ranef_model = attr(training_mod, 'lmerMod'),
                      fixed_terms = fixed_terms,
                      random_terms = random_terms,
                      resid = inbag_epsilon,
                      train_data = data.frame(),
                      predictions = list())

  # Check convergence
  final_mod@model_info <- set_ameInfo_convergence(running_ameInfo, max_iter = max_iter)

  # Add the training data
  if (keep_training_data) {final_mod@train_data <- data}

  # Finally, add prediction
  train_preds <- predict(final_mod, newdata = data, allow_new_levels = TRUE)
  val_preds   <- if(!is.null(val_data)) {
    predict(final_mod, newdata = val_data, allow_new_levels = TRUE)
  } else {
    NA
  }
  final_mod@predictions <- list(train = train_preds, validation = val_preds)


  return(final_mod)
}
