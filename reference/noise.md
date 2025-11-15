# Noise state block

Noise state block

## Usage

``` r
noise(name, variance = 0.01, fixed = FALSE)
```

## Arguments

- name:

  Name of the block

- variance:

  Variance of the noise

- fixed:

  Indicates if the variance is fixed

## Examples

``` r
n<-noise("n", 1)
block_t(n)
#>      [,1]
#> [1,]    0
```
