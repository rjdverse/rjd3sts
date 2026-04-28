# Creates a white noise.

Creates a white noise.

## Usage

``` r
.noise(var = 1)
```

## Arguments

- var:

  Variance of the noise.

## Value

A wrapper around the java object (class JD3_RawStateBlock).

## Examples

``` r
sb<-.noise(.01)
.ssf_T(sb, 0)
#>      [,1]
#> [1,]    0
```
