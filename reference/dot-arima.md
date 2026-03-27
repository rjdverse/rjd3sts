# Title

Title

## Usage

``` r
.arima(ar, delta, ma, var = 1)
```

## Arguments

- var:

## Examples

``` r
sb<-.arima(c(1, -.5), NULL, c(1,-.8))
.ssf_P0(sb)
#>       [,1]  [,2]
#> [1,]  1.12 -0.24
#> [2,] -0.24  0.12
```
