# Autoregressive model

Functions to create an autoregressive model (`ar`) or a modified
autoregressive model (`ar2`)

## Usage

``` r
ar(
  name,
  ar,
  fixedar = FALSE,
  variance = 0.01,
  fixedvariance = FALSE,
  nlags = 0,
  zeroinit = FALSE
)

ar2(
  name,
  ar,
  fixedar = FALSE,
  variance = 0.01,
  fixedvariance = FALSE,
  nlags = 0,
  nfcasts = 0
)
```

## Arguments

- ar:

  vector of the AR coefficients (See @details Additional details...).

- fixedar:

  boolean that triggers the estimation of the AR coefficients (`FALSE`)
  or fixed it (`TRUE`) to a pre-specified value set by the parameter
  `ar`.

- variance:

  the variance (\\\sigma^2\_{ar}\\).

- fixedvariance:

  boolean that triggers the estimation of the variance (`FALSE`) or
  fixed it (`TRUE`) to a pre-specified value set by the parameter
  `variance`.

- nlags:

  integer specifying how many lags of the state variable are needed

- zeroinit:

  boolean determining the initial condition for the state variable,
  which is equal to zero if `zeroinit = TRUE`. The default
  (`zeroinit = FAKSE`) triggers the an initialization based on the
  unconditional mean and variance of the AR(p) process.

- nfcasts:

  integer specifying how many forecasts of the state variable are needed

## Value

An element of type "JD3_SsfStateBlock" (wrapper around the corresponding
Java object))

## Details

The AR process is defined by \$\$y_t = \phi_1 y\_{t-1} + \phi_2
y\_{t-2} + \dots + \phi_p y\_{t-p} + \epsilon_t\$\$ The stability of the
auto-regressive polynomial is not checked.

## Examples

``` r
b_ar<-ar("my_ar", c(.8,-.3,.2), variance=1)
block_p0(b_ar)
#>           [,1]     [,2]      [,3]
#> [1,] 1.9441499 1.307883 0.7246377
#> [2,] 1.3078826 1.944150 1.3078826
#> [3,] 0.7246377 1.307883 1.9441499
```
