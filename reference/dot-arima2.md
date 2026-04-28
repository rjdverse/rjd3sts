# Creates an ARIMA state block (representation II)

Creates an ARIMA state block (representation II)

## Usage

``` r
.arima2(ar, delta, ma, var = 1)
```

## Arguments

- ar:

  Stationary auto-regressive polynomial, including the constant (=1).
  True signs.

- delta:

  Non-stationary auto-regressive polynomial, including the constant
  (=1). True signs.

- ma:

  Moving average polynomial, including the constant (=1). True signs.

- var:

  Variance of the innovations

## Value

A raw java state block

## Examples

``` r
sb<-.arima2(c(1, -.5), NULL, c(1,-.8))
.ssf_P0(sb)
#>       [,1]  [,2]
#> [1,]  1.12 -0.80
#> [2,] -0.80  0.64
```
