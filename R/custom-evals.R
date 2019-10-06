
## RMSE Evaluation
amem_rmse <- function(preds, dtrain) {

  # Get the outcome
  outcome <- xgboost::getinfo(dtrain, "label")

  # If the lmerMod exists, and we're looking at validation sets,
  # get the random preds
  lmer.mod <- attr(dtrain, 'lmerMod')
  if (!is.null(lmer.mod) && parent.frame()$j == 2){
    random_betas <- predict(lmer.mod, newdata = attr(dtrain, 'random_data'))
  } else {
    random_betas <- rep(0, nrow(dtrain))
  }

  err <- sqrt(mean((outcome - preds - random_betas)^2))
  return(list(metric = "rmse", value = err))
}


## MAE Evaluation
amem_mae <- function(preds, dtrain) {

  # Get the outcome
  outcome <- xgboost::getinfo(dtrain, "label")

  # If the lmerMod exists, and we're looking at validation sets,
  # get the random preds
  lmer.mod <- attr(dtrain, 'lmerMod')
  if (!is.null(lmer.mod) && parent.frame()$j == 2){
    random_betas <- predict(lmer.mod, newdata = attr(dtrain, 'random_data'))
  } else {
    random_betas <- rep(0, nrow(dtrain))
  }

  err <- mean(abs(outcome - preds - random_betas))
  return(list(metric = "mae", value = err))
}
