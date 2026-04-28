# Creates a seasonal component, corresponding to a multivariate random walk, with an aggregation constraint to 0 and various covariances for the innovations of the transition equation.

Creates a seasonal component, corresponding to a multivariate random
walk, with an aggregation constraint to 0 and various covariances for
the innovations of the transition equation.

## Usage

``` r
.seasonal(
  period,
  type = c("Trigonometric", "Crude", "HarrisonStevens", "Dummy"),
  var = 1
)
```

## Arguments

- period:

  Period of the seasonality

- type:

  Type of the innovations of the transition equation

- var:

  Variance of the innovations

## Value

A wrapper around the java object (class JD3_RawStateBlock)

## Examples

``` r
sb<-.seasonal(4, "HarrisonStevens", .01)
.ssf_V(sb, 0)
#>         [,1]    [,2]    [,3]
#> [1,]  0.0075 -0.0025 -0.0025
#> [2,] -0.0025  0.0075 -0.0025
#> [3,] -0.0025 -0.0025  0.0075
```
