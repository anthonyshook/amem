#' @include custom-evals.R

# Default Options for fixed and random models
default_xgb_options <-
  list(nrounds = 200,
       num_parallel_trees = 200,
       params = list(
         max_depth = 6,
         eta = .3,
         gamma = 0,
         colsample_bytree = 1,
         min_child_weight = 1,
         subsample = 1,
         nthread = 1,
         eval_metric = amem_rmse))

default_lmer_options <- lme4::lmerControl(calc.derivs = FALSE,
                                          optimizer = "nloptwrap",
                                          optCtrl = list(maxeval = 1))

default_tune_params <- list(nrounds = c(20L, 200L),
                            max_depth = c(2L, 6L),
                            min_child_weight = c(1L, 10L),
                            eta = c(.1, .5),
                            colsample_bytree = c(1,1),
                            gamma = c(1,1),
                            subsample = c(1,1))

# low level functions for generating modeling options
# Currently specific to XGB
parse_fixed_options <- function(params) {

  xgb_opts <- default_xgb_options

  if ("params" %in% names(params)) {

    # Full
    xgb_opts <- replace_in_list(params[-which(names(params) %in% 'params')],
                                xgb_opts)

    new_val_index.full <- which(names(params) %in% setdiff(names(params),
                                                           names(xgb_opts)))
    xgb_opts <- c(xgb_opts, params[new_val_index.full])


    # params
    xgb_opts$params <- replace_in_list(params$params,
                                       xgb_opts$params)
    #browser()
    new_val_index.param <- which(names(params$params) %in% setdiff(names(params$params),
                                                                   names(xgb_opts$params)))
    xgb_opts$params <- c(xgb_opts$params,
                         params$params[new_val_index.param])

  } else {
    xgb_opts <- replace_in_list(params,
                                xgb_opts)
    new_val_index.full <- which(names(params) %in% setdiff(names(params),
                                                           names(xgb_opts)))
    xgb_opts <- c(xgb_opts, params[new_val_index.full])

  }

  return(xgb_opts)
}


# Check Tuning Params
check_tuning_params <- function(tparams) {

  # Fill an object with the defaults
  out_params <- default_tune_params

  # USE THE REPLACE IN LIST function here.
  out_params <- replace_in_list(tparams, out_params)

  # Several things need to be integers, so make sure they are
  out_params$max_depth <- as.integer(out_params$max_depth)
  out_params$min_child_weight <- as.integer(out_params$min_child_weight)
  out_params$nrounds <- as.integer(out_params$nrounds)

  return(out_params)

}



# Any named value in 'from_list' will be used to replace the named value in 'to_list'
# Everything else is maintained
replace_in_list <- function(from_list, to_list) {

  # Compare and replace for the params
  replace_opts <- intersect(names(from_list),
                            names(to_list))

  for (ro in replace_opts) {
    to_list[[ro]] <- from_list[[ro]]
  }

  return(to_list)
}
