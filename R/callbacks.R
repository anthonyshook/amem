#' Custom Callback for XGBOOST
#'
#' @details Handles the random-effects portion within xgboost iterations, returning both
#' The xgboost fixed model, as well as the
#'
#' @export
cb.amem <- function () {
  callback <- function(env = parent.frame()) {

    i <- env$iteration

    # Get raw outcome
    #outcome <- xgboost::getinfo(env$dtrain, 'label')
    outcome <- attr(env$dtrain, 'origLabel')

    # Get predictions from current model
    pr <- predict(env$bst, env$dtrain, reshape = TRUE)

    # Random change
    # The fixed preds are fine, but we need to calculate the randoms
    dat <- attr(env$dtrain, 'random_data')
    dat$y <- outcome - pr
    rform <- attr(env$dtrain, 'rform')

    last_theta <- try(lme4::getME(attr(env$bst, 'lmerMod')), silent = TRUE)
    if (class(last_theta) == 'try-error') {
      last_theta <- NULL
    }

    lmer.mod <- lme4::lmer(rform, dat,
                           REML = FALSE,
                           na.action = na.pass,
                           start = last_theta,
                           control = lme4::lmerControl(calc.derivs = FALSE,
                                                       optimizer = "nloptwrap",
                                                       optCtrl = list(maxeval = 1)))


    rand_coefs_Bi <- lme4::getME(lmer.mod, "mu")

    attr(env$bst, 'lmerMod') <- lmer.mod
    for (w in 1:length(env$watchlist)){
      attr(env$watchlist[[w]], 'lmerMod') <- lmer.mod
    }

    invisible(xgboost::setinfo(env$dtrain, 'label', outcome - rand_coefs_Bi))

  }
  attr(callback, 'call') <- match.call()
  attr(callback, 'name') <- 'cb.test'
  callback
}


#' Early Stopping with required minimum
#'
#' @description Modified version of xgboost::cb.early.stop() callback
#' Uses minimum iterations, and with nrounds used as 'maximum'
#'
#' @param stopping_criteria The minimum change value for stopping
#' @param min_iter Minimum number of iterations
#' @param maximize Whether the function is attempting to maximize objective function (default is FALSE)
#' @param metric_name The name of the metric by which to evaluate model performance
#' @param verbose Logical. Whether to be chatty. Default FALSE.
#'
cb.early.stop.amem <- function(stopping_criteria,
                               min_iter,
                               maximize = FALSE,
                               metric_name = NULL, verbose = FALSE) {
  # state variables
  best_iteration <- -1
  best_ntreelimit <- -1
  best_score <- Inf
  best_msg <- NULL
  metric_idx <- 1


  init <- function(env) {
    if (length(env$bst_evaluation) == 0)
      stop("For early stopping, watchlist must have at least one element")

    eval_names <- gsub('-', '_', names(env$bst_evaluation))
    if (!is.null(metric_name)) {
      metric_idx <<- grep(pattern = metric_name, x = eval_names)
      if (length(metric_idx) == 0)
        stop("'metric_name' for early stopping is not one of the following:\n",
             paste(eval_names, collapse = ' '), '\n')
    }
    if (is.null(metric_name) &&
        length(env$bst_evaluation) > 1) {
      metric_idx <<- length(eval_names)
      # if (verbose)
      #   cat('Multiple eval metrics are present. Will use ',
      #       eval_names[metric_idx], ' for early stopping.\n', sep = '')
    }

    metric_name <<- eval_names[metric_idx]

    # maximize is usually NULL when not set in xgb.train and built-in metrics
    if (is.null(maximize))
      maximize <<- grepl('(_auc|_map|_ndcg)', metric_name)

    best_iteration <<- 1
    if (maximize) best_score <<- -Inf

    env$stop_condition <- FALSE

    if (!is.null(env$bst)) {
      if (!inherits(env$bst, 'xgb.Booster'))
        stop("'bst' in the parent frame must be an 'xgb.Booster'")
      if (!is.null(best_score <- xgboost::xgb.attr(env$bst$handle, 'best_score'))) {
        best_score <<- as.numeric(best_score)
        best_iteration <<- as.numeric(xgboost::xgb.attr(env$bst$handle, 'best_iteration')) + 1
        best_msg <<- as.numeric(xgboost::xgb.attr(env$bst$handle, 'best_msg'))
      } else {
        xgboost::xgb.attributes(env$bst$handle) <- list(best_iteration = best_iteration - 1,
                                                        best_score = best_score)
      }
    } else if (is.null(env$bst_folds) || is.null(env$basket)) {
      stop("Parent frame has neither 'bst' nor ('bst_folds' and 'basket')")
    }
  }

  finalizer <- function(env) {
    if (!is.null(env$bst)) {
      attr_best_score = as.numeric(xgboost::xgb.attr(env$bst$handle, 'best_score'))
      if (best_score != attr_best_score)
        stop("Inconsistent 'best_score' values between the closure state: ", best_score,
             " and the xgb.attr: ", attr_best_score)
      env$bst$best_iteration = best_iteration
      env$bst$best_ntreelimit = best_ntreelimit
      env$bst$best_score = best_score
    } else {
      env$basket$best_iteration <- best_iteration
      env$basket$best_ntreelimit <- best_ntreelimit
    }
  }

  callback <- function(env = parent.frame(), finalize = FALSE) {
    if (best_iteration < 0)
      init(env)

    if (finalize)
      return(finalizer(env))

    i <- env$iteration
    score = env$bst_evaluation[metric_idx]

    if (is.infinite(best_score) ||
        (maximize && score > best_score && abs(score - best_score) > stopping_criteria) ||
        (!maximize && score < best_score && abs(score - best_score) > stopping_criteria)) {

      best_msg <<- xgboost:::format.eval.string(i, env$bst_evaluation, env$bst_evaluation_err)
      best_score <<- score
      best_iteration <<- i
      best_ntreelimit <<- best_iteration * env$num_parallel_tree

      # save the property to attributes, so they will occur in checkpoint
      if (!is.null(env$bst)) {
        xgboost::xgb.attributes(env$bst) <- list(
          best_iteration = best_iteration - 1, # convert to 0-based index
          best_score = best_score,
          best_msg = best_msg,
          best_ntreelimit = best_ntreelimit)
      }
    } else if (i >= min_iter) {

      env$stop_condition <- TRUE
      env$end_iteration <- i
      if (verbose && xgboost:::NVL(env$rank, 0) == 0)
        cat("Stopping. Best iteration:\n", best_msg, "\n\n", sep = '')
    }
  }
  attr(callback, 'call') <- match.call()
  attr(callback, 'name') <- 'cb.early.stop'
  callback
}
