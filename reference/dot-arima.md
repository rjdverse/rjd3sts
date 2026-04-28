# Creates an ARIMA state block (representation I)

Creates an ARIMA state block (representation I)

## Usage

``` r
.arima(ar, delta, ma, var = 1)
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

A raw java state block.

## Examples

``` r
sb<-.arima(c(1, -.5), NULL, c(1,-.8))
.ssf_P0(sb)
#>       [,1]  [,2]
#> [1,]  1.12 -0.24
#> [2,] -0.24  0.12
```
