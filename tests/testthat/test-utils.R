
context("Testing Utils functions")

# Test Data
df <- data.frame(y = runif(10,0,1),
                 x1 = runif(10,0,1),
                 x2 = runif(10,0,1),
                 x3 = runif(10,0,1))


##### TESTS #####
test_that("Creation of fixed matrix", {
  #
  tst <- create_fixed_matrix(data = df,
                             fixed_terms = "x1",
                             outcome = "y")
  tst_sep <- create_fixed_matrix(data = df,
                                   fixed_terms = c("x1","x2","x3"),
                                   outcome = "y")
  tst_inter <- create_fixed_matrix(data = df,
                                   fixed_terms = "x1:x2",
                                   outcome = "y")

  expect_equal(tst$outcome, df$y)
  expect_equal(as.numeric(tst$fixed_data),
               df$x1)
  expect_equal(data.frame(tst_sep$fixed_data),
               df[, c("x1","x2","x3")], check.attributes = FALSE)
  expect_equal(data.frame(tst_inter$fixed_data),
               data.frame(x1.x2 = df$x1*df$x2), check.attributes = FALSE)


})

test_that("Generalized Log Likelihood function", {
  epsi  <- -5:5
  betas <- -5:5

  expect_equal(round(GLL(epsil = epsi, b = betas),1), 72.8)

  expect_error(GLL(epsil = 1:10, b = 1:5))
  expect_error(GLL(epsil = 1:10, b = 1:10, newdata = data.frame(x = 1:15)))

})

test_that("Can parse fixed options", {
  # Set-up

  param_0 <- parse_fixed_options(list())
  expect_equal(param_0$nrounds, 200)
  expect_equal(param_0$params$eta, .3)
  expect_equal(param_0$new_thing, NULL)
  expect_equal(param_0$params$new_param, NULL)

  param_1 <- parse_fixed_options(list(nrounds = 100, params = list(eta = .5)))
  expect_equal(param_1$nrounds, 100)
  expect_equal(param_1$params$eta, .5)
  expect_equal(param_1$new_thing, NULL)
  expect_equal(param_1$params$new_param, NULL)

  param_2 <- parse_fixed_options(list(nrounds = 100, params = list(eta = .5), new_thing = 10))
  expect_equal(param_2$nrounds, 100)
  expect_equal(param_2$params$eta, .5)
  expect_equal(param_2$new_thing, 10)
  expect_equal(param_2$params$new_param, NULL)

  param_3 <- parse_fixed_options(list(nrounds = 100, params = list(eta = .5, new_param = 5)))
  expect_equal(param_3$nrounds, 100)
  expect_equal(param_3$params$eta, .5)
  expect_equal(param_3$new_thing, NULL)
  expect_equal(param_3$params$new_param, 5)

  param_4 <- parse_fixed_options(list(nrounds = 100, params = list(eta = .5, new_param = 5), new_thing = 10))
  expect_equal(param_4$nrounds, 100)
  expect_equal(param_4$params$eta, .5)
  expect_equal(param_4$new_thing, 10)
  expect_equal(param_4$params$new_param, 5)

  param_5 <- parse_fixed_options(list(params = list(eta = .5, new_param = 5)))
  expect_equal(param_5$nrounds, 200)
  expect_equal(param_5$params$eta, .5)
  expect_equal(param_5$new_thing, NULL)
  expect_equal(param_5$params$new_param, 5)

  param_6 <- parse_fixed_options(list(params = list(new_param = 5)))
  expect_equal(param_6$nrounds, 200)
  expect_equal(param_6$params$eta, .3)
  expect_equal(param_6$new_thing, NULL)
  expect_equal(param_6$params$new_param, 5)

})
