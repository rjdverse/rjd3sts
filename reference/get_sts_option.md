# Set an option for sts

Set an option for sts

## Usage

``` r
get_sts_option(name)
```

## Arguments

- name:

  Name of the option

## Value

The requested option or NULL if it doesn't exist

## Examples

``` r
sts_option("test", "DUMMY")
get_sts_option("test")
#> [1] "DUMMY"
```
