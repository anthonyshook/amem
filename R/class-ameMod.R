setOldClass('xgb.Booster')
#' An S4 class of type ameMod
#'
#' @slot call The 'call' used to generate the model
#' @slot model_info Information about the model training process
#' @slot fixef_model The model representing the fixed effects
#' @slot ranef_model An lme4 class object
#' @slot fixed_terms A character vector of the column names used as fixed effects
#' @slot random_terms A character vector of random terms structure in form 'Z | B'
#' @slot resid Residuals of the model
#' @slot train_data Data used to train the model.
#' @slot predictions List containing training and validation set predictions
#'
#' @include class-ameInfo.R
#'
.ameMod <- setClass("ameMod",
                    slots = list(
                      call = "call",
                      model_info = "ameInfo",
                      fixef_model  = "xgb.Booster",
                      ranef_model  = "lmerMod",
                      fixed_terms  = "character",
                      random_terms = "character",
                      resid = "numeric",
                      train_data = "data.frame",
                      predictions = "list"
                    ))



setMethod("show", "ameMod",
          function(object) {
            cat(paste0("An Adaptive Mixed Effect Model ['", class(object),"']"))
          })

#' Initializer for the ameMod Class
#'
#' @slot call The 'call' used to generate the model
#' @slot model_info Information about the model training process
#' @slot fixef_model The model representing the fixed effects
#' @slot ranef_model An lme4 class object
#' @slot fixed_terms A character vector of the column names used as fixed effects
#' @slot random_terms A character vector of random terms structure in form 'Z | B'
#' @slot resid Residuals of the model
#' @slot train_data Data used to train the model.
#'
#' @export
ameMod <- function(call,
                   model_info,
                   fixef_model,
                   ranef_model,
                   fixed_terms,
                   random_terms,
                   resid,
                   train_data,
                   predictions){

  return(.ameMod(call = call,
                 model_info = model_info,
                 fixef_model = fixef_model,
                 ranef_model = ranef_model,
                 fixed_terms = fixed_terms,
                 random_terms = random_terms,
                 resid = resid,
                 train_data = train_data,
                 predictions = predictions
  ))
}

