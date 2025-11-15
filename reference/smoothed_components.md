# Retrieves the components of the model (univariate case) or the components corresponding to a given equation (multivariate case)

Retrieves the components of the model (univariate case) or the
components corresponding to a given equation (multivariate case)

## Usage

``` r
smoothed_components(model, equation = 1, fast = TRUE)
```

## Arguments

- model:

  Estimated state space model

- equation:

  Equation containing the components

- fast:

  if true, only the components are computed. Otherwise, their stdev are
  also computed (not returned but available for future use).

## Value

A matrix with the components

## Examples

``` r
model<-model()
llt<-locallineartrend("llt")
seas<-seasonal("seas", 12, "Crude")
n<-noise("n")
add(model,llt)
add(model,seas)
add(model,n)
y<-rjd3toolkit::Retail$BookStores
emodel<-estimate(model, y)
scmp<-smoothed_components(emodel)
high<-cbind(scmp[,1], y-scmp[,2])
low<-scmp[,c(2,3)]
matplot(high, type='l')

matplot(low, type='l')
```
