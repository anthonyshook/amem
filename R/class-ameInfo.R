#' Class of ameInfo
#'
#' @slot final_score The best score of the model
#' @slot converged Logical. If TRUE, then the model was stopped before the maximum iteration was reached.
#' @slot num_iters The number of interations run
#' @slot training_info A data.frame containing training info over time.
#'
ameInfo <- setClass("ameInfo",
                    slots = list(
                      final_score = "numeric",
                      converged = "logical",
                      num_iters = "integer",
                      training_info = "data.frame"
                    ),
                    prototype = list(
                      final_score = NA_real_,
                      best_iteration = 0L,
                      converged = FALSE,
                      num_iters = 0L,
                      training_info = data.frame(Iteration = integer(),
                                                 GLL = numeric(),
                                                 TrainingMAE = numeric(),
                                                 TrainingR = numeric(),
                                                 HoldoutMAE = numeric(),
                                                 HoldoutR = numeric(),
                                                 Time_Sec = numeric())
                    ))

setMethod("show", "ameInfo",
          function(object) {
            cat("Summary Training information\n\n")
            cat("  Final Score: ", object@final_score, "\n\n")
            cat("  Convergence: \n  ")
            cat(ifelse(object@converged,
                       paste("  Model reached convergence at iteration", object@num_iters),
                       paste("  Model did not converge, but reached max iteration of", object@num_iters)))
            cat("\n")
          })

#' Method to update ameInfo class with new data
#'
#' @param object an ameInfo object
#' @param GLL current Generalized Log Likelihood
#' @param TrainingMAE Current training MAE
#' @param TrainingR Correlation coefficient (R) of the training data predictions
#' @param HoldoutMAE Current holdout MAE (if applicable)
#' @param HoldoutR Correlation coefficient (R) of validation data
#' @param Time_Sec Time of current Iteration
#'
setGeneric("update_ameInfo_data", function(object, GLL, TrainingMAE, TrainingR, HoldoutMAE, HoldoutR, Time_Sec) {
  standardGeneric("update_ameInfo_data")})

setMethod(f = "update_ameInfo_data",
          signature = "ameInfo",
          definition = function(object,
                                GLL,
                                TrainingMAE,
                                TrainingR,
                                HoldoutMAE,
                                HoldoutR,
                                Time_Sec) {

            # Update the Iter row
            update_ind <- nrow(object@training_info) + 1
            object@training_info[update_ind, ] <- c(update_ind,
                                                    GLL,
                                                    TrainingMAE,
                                                    TrainingR,
                                                    HoldoutMAE,
                                                    HoldoutR,
                                                    Time_Sec)

            # num_iters update
            object@num_iters <- as.integer(update_ind)

            # GLL update
            object@final_GLL <- GLL

            return(object)

          } )

setGeneric("set_ameInfo_convergence", function(object, max_iter){standardGeneric("set_ameInfo_convergence")})

setMethod(f = "set_ameInfo_convergence",
          signature = "ameInfo",
          definition = function(object, max_iter) {
            if (nrow(object@training_info) == max_iter) {
              object@converged <- FALSE
            } else {
              object@converged <- TRUE
            }

            return(object)
          })
