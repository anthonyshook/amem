
#' Predict function for ameMod class
#'
#' @param object an ameMod object
#' @param newdata New data. If not provided, predictions will be generated from training data, if available
#' @param type_fixed Type of prediction for fixed effects ("raw" or "prob")
#' @param type_random Type of prediction for random effects ("response" or "link")
#' @param allow_new_levels Logical if new levels (or NA values) in newdata are allowed. If FALSE (default), such new values in newdata will trigger an error; if TRUE, then the prediction will use the unconditional (population-level) values for data with previously unobserved levels (or NAs).
#' @param separate_preds Logical. If TRUE, a data.frame containing separate fixed/random effects columns is produced. If FALSE, only the final combined prediction (as a vector) is returned
#' @param na_action function determining what should be done with missing values for fixed effects in newdata. The default is to predict NA: see na.pass
#' @param ... Other parameters to pass to \code{caret::predict}
#'
#' @export
#'
predict.ameMod <- function(object,
                           newdata,
                           type_fixed,
                           type_random,
                           allow_new_levels,
                           separate_preds,
                           na_action,
                           ...) {


  # Stage data in proper format for fixed effect calculation?
  if (!is.null(newdata)) {

    new_random_terms <- parse_random_effect_terms(object@random_terms, split_nests = TRUE)
    # SEPARATE
    fix_data <- create_fixed_matrix(data = newdata, fixed_terms = object@fixed_terms)$fixed_data
    ran_data <- data.frame(newdata)[, new_random_terms, drop = FALSE ]

  } else {
    # TAKE FROM THE TWO SEPARATE MODELS
    fix_data <- create_fixed_matrix(data = object@train_data, fixed_terms = object@fixed_terms)$fixed_data
    ran_data <- NULL # this is fine because lmer is smarter than xgboost
  }

  # Prediction
  preds <- data.frame(
    fixed_preds = stats::predict(object@fixef_model,
                                 newdata = fix_data,
                                 type = type_fixed,
                                 na.action = na_action,
                                 ...),
    random_preds = stats::predict(object@ranef_model,
                                  newdata = ran_data,
                                  type = type_random,
                                  allow.new.levels = allow_new_levels,
                                  na.action = na_action)
  )

  # Combine, simple linear math when raw/response
  if (type_fixed == "raw" &
      type_random == "response") {

    preds$final_preds <-
      preds$fixed_preds + preds$random_preds
  } else {
    stop("Currently doesn't support combining predictions of different types")
  }

  if (separate_preds) {
    return(preds)
  } else {
    return(preds$final_preds)
  }

}
