# Creates a composite loading

Creates a composite loading

## Usage

``` r
.loading_composite(dims, cmps)
```

## Arguments

- dims:

  The dimensions of the corresponding state blocks

- cmps:

  The loadings

## Examples

``` r
l<-.loading_composite(c(1,2,3), list(.loading(0), .loading(0), .loading(0)))
.ssf_Z(l, 6, 0)
#> [1] 1 1 0 1 0 0
```
