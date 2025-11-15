# Estimate a SSF Model

Estimate a SSF Model

## Usage

``` r
estimate(
  model,
  data,
  marginal = FALSE,
  concentrated = TRUE,
  initialization = c("Augmented_Robust", "Diffuse", "SqrtDiffuse", "Augmented",
    "Augmented_NoCollapsing"),
  optimizer = c("LevenbergMarquardt", "MinPack", "BFGS", "LBFGS"),
  precision = 1e-15,
  initialParameters = NULL
)
```

## Arguments

- model:

  the model

- data:

  a matrix containing the data (one time series per column, time series
  dimension on the rows)

- marginal:

  logical value used to specify whether the marginal likelihood
  definition is used (TRUE) or not (FALSE) during the optimization. The
  marginal likelihood is recommended when there is at least one variable
  that loads on a non-stationary latent variable and the loading
  coefficient needs to be estimated.

- concentrated:

  logical value used to specify whether the likelihood is concentrated
  (TRUE) or not (FALSE) during the optimization

- initialization:

  initialization method.

- precision:

  indicating the largest likelihood deviations that make the algorithm
  stop.

- initialParameters:
