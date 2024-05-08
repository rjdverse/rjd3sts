
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjd3sts

## Installation

To get the current stable version (from the latest release):

``` r
# install.packages("remotes")
remotes::install_github("rjdemetra/rjd3toolkit@*release")
remotes::install_github("rjdemetra/rjd3sts@*release")
```

To get the current development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("rjdemetra/rjd3sts")
```

## Usage

``` r
library("rjd3sts")
#> 
#> Attaching package: 'rjd3sts'
#> The following objects are masked from 'package:stats':
#> 
#>     ar, arima, cycle

y <- log(rjd3toolkit::ABS$X0.2.09.10.M)
days<-c(1,1,1,1,2,3,0)

model<-rjd3sts::model()
sarima<-rjd3sts::sarima('arima', 12, orders=c(0,1,1), seasonal=c(0,1,1))
td<-rjd3sts::reg_td('td', 12, start(y), length(y), variance=1, fixed=FALSE)

rjd3sts::add(model, sarima)
rjd3sts::add(model, td)

rslt<-rjd3sts::estimate(model, y)
cmp<-rjd3sts::smoothed_components(rslt)
ss<-rjd3sts::smoothed_states(rslt)

plot(cmp[,2], type='l', ylim=c(-0.05, 0.04), col='blue', main="Time-varying td effect (+ Sundays coeff.)", xlab="", ylab="td effect")
lines(-rowSums(ss[,15:20]), col='green', lwd = 3)
```

<img src="man/figures/README-sts-var-td-1.png" width="100%" />

## Package Maintenance and contributing

Any contribution is welcome and should be done through pull requests
and/or issues. pull requests should include **updated tests** and
**updated documentation**. If functionality is changed, docstrings
should be added or updated.

## Licensing

The code of this project is licensed under the [European Union Public
Licence (EUPL)](https://joinup.ec.europa.eu/page/eupl-text-11-12).
