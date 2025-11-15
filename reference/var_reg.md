# Time Varying Regressor

Time Varying Regressor

## Usage

``` r
var_reg(name, x, stderr, scale = 1, fixed = FALSE)
```

## Arguments

- x:

  Regression variable. Numerics

- stderr:

  Standard error of the innovations of the coefficient (1 in
  extrapolation)

- scale:

  Scaling factor

- fixed:

  Fixed scaling factor

## Examples

``` r
 x<-rjd3toolkit::Retail$BookStores
 std<-rep(1, length(x))
 std[c(20, 50, 150)]<-5
 v<-var_reg("vx", x, std, 0.1)
```
