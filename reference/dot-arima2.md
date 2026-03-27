# Title

Title

## Usage

``` r
.arima2(ar, delta, ma, var = 1)
```

## Arguments

- var:

## Examples

``` r
sb<-.arima2(c(1, -.5), NULL, c(1,-.8))
.ssf_P0(sb)
#>       [,1]  [,2]
#> [1,]  1.12 -0.80
#> [2,] -0.80  0.64
```
