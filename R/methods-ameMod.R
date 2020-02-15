
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
