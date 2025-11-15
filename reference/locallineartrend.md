# Local linear trend state block

Local linear trend state block

## Usage

``` r
locallineartrend(
  name,
  levelVariance = 0.01,
  slopeVariance = 0.01,
  fixedLevelVariance = FALSE,
  fixedSlopeVariance = FALSE
)
```

## Arguments

- name:

  Name of the block

- levelVariance:

  variance of the level (\\\sigma^2_l\\)

- fixedLevelVariance, fixedSlopeVariance:

  boolean that triggers the estimation of the variances \\\sigma^2_l\\
  and \\\sigma^2_n\\ (`FALSE`) or fixes it (`TRUE`) to a pre-specified
  value set by the parameters `levelVariance` and `slopevariance`.

## Details

\$\$\begin{cases}l\_{t+1} = l_t + n_t + \xi_t \\ n\_{t+1} = n_t + \mu_t
\\ \xi_t \sim N(0, \sigma^2_l)\\ \mu_t \sim N(0, \sigma^2_n)
\end{cases}\$\$

## Examples

``` r
llt<-locallineartrend('llt', levelVariance=1, slopeVariance=.25)
print(block_t(llt))
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    0    1
```
