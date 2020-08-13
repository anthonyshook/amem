# amem

The goal of Adaptable Mixed Effects Models (amem) is to provide a framework for using custom models (linear or non-linear) within a mixed-effects model structure. This allows us to build models that utilize an arbitrary method for generating predictions for fixed effects, which are then adjusted by calculating the linear form of random-effects.

## Support
Currently the model only supports XGBoost as a test case. Future development should get it synced up to caret or parsnip to allow for utilizing any parsnip-supported model as the fixed-effects regressor.


## Using the model

The syntax is meant, in the simplest form, to emulate the `lmer` function from the [https://github.com/lme4/lme4](lme4 package).

``` r
# Built in RADON dataset
train_index <- sample(1:nrow(radon), floor(nrow(radon) * .8))
radon.train <- radon[train_index, ]
radon.test  <- radon[-train_index, ]

# Building a model to predict  with fixed-effect of adjwt, and a random-effect of region
mod <- amem(log_radon ~ adjwt + (1|region), data = radon.train)

# Predict
pred <- predict(mod, newdata = radon.test, allow_new_levels = TRUE)

```
