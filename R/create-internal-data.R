#' Create Internal Object
#'
#' @param dat The data object
#' @param formula The formula used to identify the fixed and random features
#'
#' @return Object of class xgb.DMatrix object, with additional attributes
#' \describe{
#'   \item{\code{rform}}{Formula for use in the random effect calculations}
#'   \item{\code{random_data}}{data.frame for use in the random-effect calculation}
#'   \item{\code{origLabel}}{The original outcome vector for the callback}
#' }
#'
create_internal_data <- function(dat, formula) {

  # Parsing formula
  all_terms <- attr(terms(formula, data = dat), "term.labels")
  fixed_terms <- all_terms[!grepl(all_terms, pattern = "|", fixed = TRUE)]
  random_terms <- all_terms[grepl(all_terms, pattern = "|", fixed = TRUE)]

  # Generating the fixed-formula matrix
  fixed_form <- create_fixed_matrix(data = dat,
                                    fixed_terms = fixed_terms,
                                    outcome = as.character(formula[[2]]))

  # Now create a formula for generating random-effects using lme4
  re_form <- stats::as.formula(
    paste0(as.character(formula[[2]]), " ~ 0 + ",
           paste0("(", random_terms, ")", collapse = " + "))
  )

  # Generate xgboost object
  trainData <- xgboost::xgb.DMatrix(as.matrix(fixed_form$fixed_data), label = fixed_form$outcome)

  # Hang some additional attributes
  attr(trainData, 'rform')  <- re_form
  attr(trainData, 'random_data') <- dat
  attr(trainData, 'origLabel') <- fixed_form$outcome

  return(trainData)
}



# # try with internal method
# radonDGM.train <- xgboost::xgb.DMatrix(as.matrix(radon_train[, c("Uppm")]), label = radon_train$log_radon)
#
# attr(radonDGM.train, 'rform')  <- 'log_radon ~ 0 + (1 | county)'
# attr(radonDGM.train, 'random') <- radon_train
# attr(radonDGM.train, 'origLabel') <- radon_train$log_radon
