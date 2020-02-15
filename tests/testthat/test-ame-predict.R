context("AME Fit and Predict testing")


# Data
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
                 predictions = list())

test_that("AMEM produces expected outputs", {
  expect_output({mod <- amem("log_radon ~ Uppm + pcterr + (1|basement)",
                             data = radon,
                             min_iter = 1,
                             max_iter = 1,
                             chatty = TRUE)})

  expect_silent({mod <- amem("log_radon ~ Uppm + pcterr + (1|basement)",
                             data = radon,
                             min_iter = 1,
                             max_iter = 1,
                             chatty = FALSE)})

  expect_is(mod@fixef_model, "xgb.Booster")
  expect_is(mod@ranef_model, "merMod")
  expect_equal(mod@fixed_terms,
               c("Uppm", "pcterr"))
  expect_equal(mod@random_terms,
               c("1 | basement"))

})

test_that("Formula parsing works correctly", {
  expect_error({mod <- amem("log_radon Uppm + (1|basement)",
                            data = radon,
                            min_iter = 1,
                            max_iter = 1,
                            chatty = TRUE)})


})


test_that("ameFit accepts validation data", {
  expect_output({mod <- amem("log_radon ~ Uppm + pcterr + (1|basement)",
                             data = radon,
                             min_iter = 1,
                             max_iter = 1,
                             val_data = radon,
                             chatty = TRUE)})
})

test_that("Prediction with new data works", {

  (P <- predict(tstmod,
                newdata = radon,
                type_fixed = "raw",
                type_random = "response",
                separate_preds = FALSE,
                allow_new_levels = TRUE))

  expect_true(all(dim(P) == c(nrow(radon), 3)))

})
