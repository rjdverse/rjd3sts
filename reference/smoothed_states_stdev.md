# Standard deviations of the smoothed states

Standard deviations of the smoothed states

## Usage

``` r
smoothed_states_stdev(model)
```

## Arguments

- model:

  Estimated model

## Value

A matrix with the standard deviations of the states (a row corresponding
to one time point)

## Examples

``` r
model<-model()
ll<-locallevel("ll")
seas<-seasonal("seas", 12, "Crude")
n<-noise("n")
add(model,ll)
add(model,seas)
add(model,n)
emodel<-estimate(model, rjd3toolkit::Retail$BookStores)
ess<-smoothed_states_stdev(emodel)
cmps<-ess[,c(1,2,13)]
matplot(cmps, type='l')
```
