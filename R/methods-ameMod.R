
if (!isGeneric("predict")) {
  setGeneric("predict", function(object, ...)
    standardGeneric("predict"))
}

#' Predict method for ameMod object
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
setMethod(f = "predict", signature = "ameMod",
          definition = function(object,
                                newdata = NULL,
                                type = "raw",
                                allow_new_levels = FALSE,
                                separate_preds = FALSE,
                                na_action = stats::na.pass,
                                ...) {

            # Check type
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

            # Get prediction
            pred_outs <- predict.ameMod(object = object,
                                        newdata = newdata,
                                        type_fixed = type_fixed,
                                        type_random = type_random,
                                        allow_new_levels = allow_new_levels,
                                        separate_preds = separate_preds,
                                        na_action = na_action,
                                        ...)

            return(pred_outs)


          })


setGeneric("varImp", function(object) {standardGeneric('varImp')})
#' Variable importance for fixed effects measures
#'
#' @param object amem model object
#'
#' @export
setMethod('varImp',
          signature = 'ameMod',
          definition = function(object) {

            # Fixed effect importance
            imp <- xgboost::xgb.importance(feature_names = object@fixed_terms, model = object@fixef_model)
            imp <- as.data.frame(imp)[, 1:2]
            rownames(imp) <- as.character(imp[,1])
            imp <- imp[,2,drop = FALSE]
            colnames(imp) <- "Overall"

            missing <- object@fixed_terms[!(object@fixed_terms %in% rownames(imp))]
            missing_imp <- data.frame(Overall=rep(0, times=length(missing)))
            rownames(missing_imp) <- missing
            imp <- rbind(imp, missing_imp)

            imp
          })
