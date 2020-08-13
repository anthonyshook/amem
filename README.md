# amem

The goal of Adaptable Mixed Effects Models (amem) is to provide a framework for using custom models (linear or non-linear) within a mixed-effects model structure. This allows us to build models that utilize an arbitrary method for generating predictions for fixed effects, which are then adjusted by calculating the linear form of random-effects.

## Support
Currently the model only supports XGBoost as a test case. Future development should get it synced up to caret or parsnip to allow for utilizing any parsnip-supported model as the fixed-effects regressor.
