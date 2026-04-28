# Transforms a time invariant state space form based on functions into a state space models represented by matrices.

Transforms a time invariant state space form based on functions into a
state space models represented by matrices.

## Usage

``` r
.ssf_as_time_invariant(jssf)
```

## Arguments

- jssf:

  The object oriented (java) state space form, which should be time
  invariant

## Value

A new Java object based on matrices

## Examples

``` r
ll<-.local_linear_trend(0.1, 0.1)
s<-.seasonal(12, var=.5)
m<-.composite(list(ll, s))
ssf1<-.ssf(m, .loading(c(0,2)), 1)
ssf2<-.ssf_as_time_invariant(ssf1)
ll1<-.akf_likelihood(ssf1, rjd3toolkit::ABS$X0.2.09.10.M)
ll2<-.akf_likelihood(ssf2, rjd3toolkit::ABS$X0.2.09.10.M)
print(ll1$ll-ll2$ll)
#> [1] 4.547474e-13
```
