# Computes the diffuse likelihood by means of the diffuse Kalman filter (Durbin-Koopman).

Computes the diffuse likelihood by means of the diffuse Kalman filter
(Durbin-Koopman).

## Usage

``` r
.dk_likelihood(ssf, data, sqr = TRUE, rescalingFactor = TRUE)
```

## Arguments

- ssf:

  A state space form.

- data:

  Arrays of data.

- sqr:

  Square root diffuse initialization, if any

- rescalingFactor:

  True if the innovation covariance matrix is defined up to a scaling
  factor.

## Value

The diffuse likelihood

## Examples

``` r
sarima<-.sarima(12, NULL, 1,  -.6, NULL, 1, -.5)
ssf<-.ssf(sarima, .loading(0))
.dk_likelihood(ssf, rjd3toolkit::ABS$X0.2.09.10.M)
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
#> [1] 3.90065
#> 
#> $dcorr
#> [1] 0
#> 
#> attr(,"class")
#> [1] "JD3DIFFUSELIKELIHOOD"
```
