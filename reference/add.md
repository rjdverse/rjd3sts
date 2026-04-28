# Adds a state block or a measurement equation to a given state space model

Adds a state block or a measurement equation to a given state space
model

## Usage

``` r
add(model, item)
```

## Arguments

- model:

  A state space model

- item:

  A state block or a measurement equation

## Examples

``` r
model<-model()
llt<-locallineartrend("llt")
add(model,llt)
```
