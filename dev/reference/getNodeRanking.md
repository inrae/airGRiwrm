# Sorting of the nodes from upstream to downstream for RunModel and Calibration

Sorting of the nodes from upstream to downstream for RunModel and
Calibration

## Usage

``` r
getNodeRanking(griwrm)
```

## Arguments

- griwrm:

  \[object of class `GRiwrm`\] see
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)
  for details

## Value

A [character](https://rdrr.io/r/base/character.html)
[vector](https://rdrr.io/r/base/vector.html) containing ordered node ids

## Details

The sort is done by searching upstream nodes in the networks
recursively. Ungauged node clusters are processed by cluster and the
algorithm tries to process ungauged nodes which receive their parameters
from upstream or sibling node after their donor node. Use
`options(debug = TRUE)` to get details on how the sort is performed.

## See also

[`CreateGRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)
