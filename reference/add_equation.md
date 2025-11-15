# Add a building block to the considered equation

Add a building block to the considered equation

## Usage

``` r
add_equation(equation, item, coeff = 1, fixed = TRUE, loading = NULL)
```

## Arguments

- equation:

  the equation

- item:

  the block of the state array that will be linked to the observation
  corresponding to this equation through the specified loading and
  coefficient

- coeff:

  the value of the coefficient associated to the block of latent
  variables defined by `item`.

- fixed:

  logical that triggers estimation of coeff (FALSE) or fixes it (TRUE)
  to a pre-specified value

- loading:

  the loading that links the block to the observations

## Examples

``` r
eq<- equation('eq1')
ll<-locallevel('ll')
n<-noise("n", variance = 1, fixed = TRUE)
add_equation(eq, ll)
add_equation(eq, n, coeff=0.1, fixed=FALSE)
```
