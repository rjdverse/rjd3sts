# Retrieves the dimension of a state block

Retrieves the dimension of a state block

## Usage

``` r
.state_dim(x)
```

## Arguments

- x:

  A state block

## Value

The length of the state block

## Examples

``` r
s<-.seasonal(12)
.state_dim(s)
#> [1] 11
```
