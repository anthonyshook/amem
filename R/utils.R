#' Create fixed effect matrix
#'
#' @param data The set of data to use in creating the model.matrix and getting outcome info
#' @param fixed_terms Terms to use in fixed effect
#' @param outcome Column to use as outcome
#'
#' @return A list, FM, containing
#' \describe{
#'   \item{fixed_data}{the model matrix}
#'   \item{outcome}{a vector of outcome data}
#'   }
#'
create_fixed_matrix <- function(data, fixed_terms, outcome = NULL) {

  out <- list()

  # Generate the model matrix
  out$fixed_data <- stats::model.matrix(
    stats::as.formula(
      paste("~ ",
            paste(fixed_terms, collapse = "+")
      )
    ),
    model.frame(~ ., data, na.action = na.pass))[, -1, drop = FALSE]


  if (!is.null(outcome)){
    out$outcome <- data[[ as.character(outcome) ]]
  } else {
    out$outcome <- ""
  }

  return(out)
}


#' Generalized Log Likelihood function
#'
#' @param epsil a vector of model residuals (Where epsilon = Y - Xb - Zu from trained models)
#' @param b The random-effect betas. Should be the same length as epsil
#'
#' @details Note: Because \code{AMEM} uses \code{lme4::lmer} to generate random-effects betas with a 0-valued fixed effect formula (i.e., \code{outcome ~ 0 + (1 | random_effect)}), running \code{predict} on the lmerMod object provides the betas.
GLL <- function(epsil, b) {

  # Quick checks
  if (!identical(length(epsil), length(b))) {
    stop("epsil and b must be of the same length")
  }

  # Get the current sigma from the errors to get R and make a
  R0 <- cov(as.matrix(epsil))[[1]]
  Ri <- Matrix::.sparseDiagonal(x = R0, n = length(epsil))

  # Get the value of D
  # This is the covariance of the model betas
  # Since our formula has a 0 fixed-effect value, predict on LMER will give us this
  D0 <- cov(as.matrix(b))[[1]]
  Di <- Matrix::.sparseDiagonal(x = D0, n = length(epsil))

  # first part
  D_logdet <- Matrix::determinant(Di)$modulus[[1]]
  D_logdet <- ifelse(is.infinite(D_logdet), 0, D_logdet)

  R_logdet <- Matrix::determinant(Ri)$modulus[[1]]
  R_logdet <- ifelse(is.infinite(R_logdet), 0, R_logdet)


  GLL <- t(epsil) %*% Matrix::solve(Ri) %*% epsil +
    t(b) %*% Matrix::solve(Di) %*% b +
    D_logdet +
    R_logdet

  return(GLL[1])
}

# Low level function for parsing random effect terms
# split_nests tells the function to split cross/nested variables
parse_random_effect_terms <- function(rfterms, split_nests = FALSE) {

  terms <- trimws(unlist(strsplit(rfterms, split = "\\||\\+")))

  # Remove lone 0 and 1 terms (LHS of | in lmer formatting)
  terms <- terms[!grepl(terms, pattern = "^0$|^1$")]

  if (split_nests){
    terms <- trimws(unlist(strsplit(terms, split = "\\:|\\/")))
  }

  return(unique(terms))

}


#' Generate folds for cross-validation
#'
#' @param num_rows Number of rows of target set
#' @param nfolds Number of folds to generate (Default = 10)
#' @param seed Set to stabilize random sampling (Default is \code{as.integer(Sys.time())})
#'
define_cv_folds <-function (num_rows,
                            nfolds = 10,
                            seed = as.integer(Sys.time())) {

  # Set the seed, get random order
  set.seed(seed)
  indx <- sample(num_rows, num_rows)

  # Split into NFolds
  cut_indx <- cut(1:num_rows, nfolds, labels = FALSE)
  nfold_indx_list <- split(x = indx, f = cut_indx)

  return(nfold_indx_list)
}
