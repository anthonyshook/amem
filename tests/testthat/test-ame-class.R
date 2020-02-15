context("AME Class and Methods:")

lmeMod <- lme4::lmer("log_radon ~ 0 + (1|basement)", data = radon)
trainMod <- xgboost::xgboost(label = radon$log_radon,save_period = NULL,
                             data = as.matrix(radon[ c("Uppm", "activity")]),
                             params = list(), nrounds = 10, verbose = FALSE)

tstmod <- ameMod(call = call("amem", formula = 'log_radon ~ Uppm + activity + (1|basement)',
                             data = substitute(radon)),
                 model_info = ameInfo(),
                 fixef_model = trainMod,
                 ranef_model = lmeMod,
                 fixed_terms = c("Uppm", "activity"),
                 random_terms = c("1 | basement"),
                 resid = rep(0, times = nrow(radon)),
                 train_data = radon,
                 predictions = data.frame())

test_that("Create and show ameMod object", {

  expect_s4_class(tstmod, "ameMod")

})


test_that("Predict method accepts specific type values", {

  expect_error(predict(tstmod,
                       type = "none"),
               "predict currently only accepts type = 'raw'")

})


test_that("Predict Method generates predictions", {

  # Types
  expect_is(predict(tstmod), "numeric")
  expect_is(predict(tstmod, separate_preds = TRUE), "data.frame")

})
