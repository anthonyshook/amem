
#' Predict function for ameMod object
#'
#' @param object an ameMod object
#' @param newdata New data. If not provided, predictions will be generated from training data, if available
#' @param type Type of prediction for fixed effects ("raw" or "prob")
#' @param allow_new_levels Logical if new levels (or NA values) in newdata are allowed. If FALSE (default), such new values in newdata will trigger an error; if TRUE, then the prediction will use the unconditional (population-level) values for data with previously unobserved levels (or NAs).
#' @param separate_preds Logical. If TRUE, a data.frame containing separate fixed/random effects columns is produced. If FALSE, only the final combined prediction (as a vector) is returned
#' @param na_action function determining what should be done with missing values for fixed effects in newdata. The default is to predict NA: see na.pass
#' @param ... Other parameters to pass to \code{caret::predict}
#'
#' @include predict-ameMod.R
#'
#' @export
#'
predict.ameMod <- function(object,
                           newdata = NULL,
                           type = "raw",
                           allow_new_levels = FALSE,
                           separate_preds = FALSE,
                           na_action = stats::na.pass,
                           ...) {

  # Input checking and data prep
  if (identical(type, "raw")) {
    type_fixed  <- "raw"
    type_random <- "response"
  } else {
    stop("predict currently only accepts type = 'raw'")
  }

  # Check for object data if newdata is null
  if (is.null(newdata) &
      nrow(object@train_data) == 0) {
    stop("The model object does not contain training data, so newdata cannot be empty.")
  }

  # Check that newdata, if it exists, contains the required columns
  if (!is.null(newdata)) {

    exp_terms <- c(object@fixed_terms,
                   parse_random_effect_terms(object@random_terms, split_nests = TRUE))

    if(!all(exp_terms %in% colnames(newdata))) {
      stop("Missing expected columns in newdata.")
    }
  }

  # Stage data in proper format for fixed effect calculation
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
