# Sort a GRiwrm network in upstream-downstream order ready for Calibration

It Uses
[getNodeRanking](https://inrae.github.io/airGRiwrm/dev/reference/getNodeRanking.md)
for determining the best order for calibration and leaves direct
injection nodes at the tail of the list.

## Usage

``` r
# S3 method for class 'GRiwrm'
sort(x, decreasing = FALSE, ...)
```

## Arguments

- x:

  A *GRiwrm* object (See
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md))

- decreasing:

  logical. Should the sort be increasing or decreasing? Not available
  for partial sorting.

- ...:

  arguments to be passed to or from methods or (for the default methods
  and objects without a class) to `sort.int`.

## Value

The sorted *GRiwrm* object in upstream-downstream order ready for
Calibration
