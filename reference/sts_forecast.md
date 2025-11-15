# Forecast with STS model

Forecast with STS model

## Usage

``` r
sts_forecast(y, model = c("none", "td2", "td3", "td7", "full"), nf = 12)
```

## Arguments

- y:

  Series

- model:

  Model for calendar effects

  - td2: leap year + week days (week-end derived)

  - td3: leap year + week days + saturdays (sundays derived)

  - td7: leap year + all days (sundays derived)

  - full: td3 + easter effect

  - none: no calendar effect

- nf:

  number of forecasts

## Examples

``` r
fcasts<-sts_forecast(rjd3toolkit::ABS$X0.2.09.10.M)
```
