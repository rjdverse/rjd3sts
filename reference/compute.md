# Computes a model, for given parameters

Computes a model, for given parameters

## Usage

``` r
compute(model, data, parameters, marginal = FALSE, concentrated = TRUE)
```

## Arguments

- model:

  the model

- data:

  a matrix containing the data (one time series per column, time series
  dimension on the rows)

- parameters:

  Parameters of the model

- marginal:

  logical value used to specify whether the marginal likelihood
  definition is used (TRUE) or not (FALSE) during the optimization. The
  marginal likelihood is recommended when there is at least one variable
  that loads on a non-stationary latent variable and the loading
  coefficient needs to be estimated.

- concentrated:

  logical value used to specify whether the likelihood is concentrated
  (TRUE) or not (FALSE) during the optimization

## Value

An object of the class "JD3_SsfModelEstimation"
