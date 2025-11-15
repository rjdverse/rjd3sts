# Title

Title

## Usage

``` r
sts(
  y,
  X = NULL,
  X.td = NULL,
  level = 1,
  slope = 1,
  cycle = -1,
  noise = 1,
  seasonal = c("Trigonometric", "Dummy", "Crude", "HarrisonStevens", "Fixed", "Unused"),
  diffuse.regs = TRUE,
  tol = 1e-09
)
```

## Arguments

- y:

  input time series.

- X:

  Regression variables (same length as y) or NULL

- X.td:

  Groups of days for trading days regressors. The length of the array
  must be 7. It indicates to what group each week day belongs. The first
  item corresponds to Mondays and the last one to Sundays. The group
  used for contrasts (usually Sundays) is identified by 0. The other
  groups are identified by 1, 2,... n (\<= 6). For instance, usual
  trading days are defined by `c(1,2,3,4,5,6,0)`, week days by
  `c(1,1,1,1,1,0,0)`, etc...

- level:

  -1 = no level, 0 = fixed level, 1 = sotchastic level

- seasonal:

  Seasonal model

- tol:

## Examples

``` r
 x<-rjd3toolkit::Retail$BookStores
 sts(x)
#> Structural time series 
#> 
#> Variances:
#> level:  135.5975 
#> slope:  0.156252 
#> seasonal:  52.25239 
#> noise:  174.5098 
#> 
#> LogLikelihood:  -1233.544 
#> AIC:  2467.089 
#> 
#> No regression variable
```
