# Computes the signal, which is defined by the scalar product between the rows of the smoothed states and of a given matrix.

Computes the signal, which is defined by the scalar product between the
rows of the smoothed states and of a given matrix.

## Usage

``` r
signal(object, pos = NULL, loading = NULL, stdev = FALSE)
```

## Arguments

- object:

  A model estimation

- pos:

  The selection of the elements of the states; NULL if we select all the
  items.

- loading:

  The matrix that will multiply (a selection of) the smoothed states. If
  NULL, we just sum the selected items.

- stdev:

  True if we compute the standard deviation of the signal, false
  otherwise

## Value

An array of data
