# Local Level

Local Level

## Usage

``` r
locallevel(name, variance = 0.01, fixed = FALSE, initial = NaN)
```

## Arguments

- name:

  Name of the block

- variance:

  the value of the variance (\\\sigma^2_l\\).

- fixed:

  boolean that triggers estimation of \\\sigma^2_l\\ (`FALSE`) or fixes
  it (`TRUE`) to a pre-specified value set by the parameter `variance`.

- initial:

  initial value of the level (\\l_0\\).

## Details

\$\$\begin{cases}l\_{t+1} = l_t + \mu_t \\ \mu_t \sim N(0, \sigma^2
\sigma^2_l) \end{cases} \$\$

## Examples

``` r
ll<-locallevel('ll', variance=1)
print(block_t(ll))
#>      [,1]
#> [1,]    1
```
