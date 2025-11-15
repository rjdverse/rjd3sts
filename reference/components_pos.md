# Position of the components

Position of the components

## Usage

``` r
components_pos(model)
```

## Arguments

- model:

  Estimated model

## Value

The first position of the different blocks in the state array

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
print(components_pos(emodel))
#> [1]  1  2 13
```
