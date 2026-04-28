# Computes the diffuse likelihood by means of the augmented Kalman filter.

Computes the diffuse likelihood by means of the augmented Kalman filter.

## Usage

``` r
.akf_likelihood(
  ssf,
  data,
  qtype = c("NORMAL", "PARTIAL_TRIANGULARIZATION", "FULL_TRIANGULARIZATION", "QR"),
  collapsing = TRUE,
  rescalingFactor = TRUE
)
```

## Arguments

- ssf:

  A state space form.

- data:

  Arrays of data.

- qtype:

  Type of the initialization in the augmented Kalman filter

- rescalingFactor:

  True if the innovation covariance matrix is defined up to a scaling
  factor.

## Value

The diffuse likelihood

## Examples

``` r
sarima<-.sarima(12, NULL, 1,  -.6, NULL, 1, -.5)
ssf<-.ssf(sarima, .loading(0))
.akf_likelihood(ssf, rjd3toolkit::ABS$X0.2.09.10.M)
#> $nobs
#> [1] 425
#> 
#> $ndiffuse
#> [1] 13
#> 
#> $ll
#> [1] -2251.818
#> 
#> $ssq
#> [1] 1335526
#> 
#> $ldet
#> [1] 7.054043
#> 
#> $dcorr
#> [1] -3.153393
#> 
#> attr(,"class")
#> [1] "JD3DIFFUSELIKELIHOOD"
```
