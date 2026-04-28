# Computes smoothed states by means of the augmented Kalman filter in the case of diffuse initialization

Computes smoothed states by means of the augmented Kalman filter in the
case of diffuse initialization

## Usage

``` r
.ssf_smooth(
  ssf,
  data,
  all = TRUE,
  qtype = c("NORMAL", "PARTIAL_TRIANGULARIZATION", "FULL_TRIANGULARIZATION", "QR")
)
```

## Arguments

- ssf:

  A state space form.

- data:

  Arrays of data.

- all:

  True if the covariances of the smoothed states are compted.

- qtype:

  Type of the initialization in the augmented Kalman filter

## Value

A matrix with the smoothed states and - if requested - their standard
deviations.
