# Creates a local linear trend state block.

Creates a local linear trend state block.

## Usage

``` r
.local_linear_trend(lvar, svar = 0)
```

## Arguments

- lvar:

  Innovation variance of the level equation.

- svar:

  Innovation variance of the slope equation.

## Value

A wrapper around the java object (class JD3_RawStateBlock).

## Examples

``` r
sb<-.local_linear_trend(1.5, 0.5)
.ssf_T(sb, 0)
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    0    1
```
