# Creates the state space form of an airline model;

Creates the state space form of an airline model;

## Usage

``` r
.airline(period = 12, theta = -0.6, btheta = -0.8)
```

## Arguments

- period:

  Period of the model.

- theta:

  Regular moving average parameter.

- btheta:

  Seasonal moving average parameter.

## Value

The raw state space form of the Airline model.

## Examples

``` r
ssf<-.airline(24)
.dk_likelihood(ssf, rjd3toolkit::ABS$X0.2.09.10.M)
#> $nobs
#> [1] 425
#> 
#> $ndiffuse
#> [1] 25
#> 
#> $ll
#> [1] -2378.489
#> 
#> $ssq
#> [1] 3215970
#> 
#> $ldet
#> [1] 24.95667
#> 
#> $dcorr
#> [1] 0
#> 
#> attr(,"class")
#> [1] "JD3DIFFUSELIKELIHOOD"
```
