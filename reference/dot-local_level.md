# Creates a local level state block

Creates a local level state block

## Usage

``` r
.local_level(var = 1, start = NaN)
```

## Arguments

- var:

  Innovation variance of local level

- start:

  Initial value of the state block. Should be NaN for a diffuse
  initialization

## Value

A wrapper around the java object (class JD3_RawStateBlock).

## Examples

``` r
sb<-.local_level(1.5, 0)
.ssf_T(sb, 0)
#>      [,1]
#> [1,]    1
```
