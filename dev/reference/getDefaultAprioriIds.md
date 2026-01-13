# Get default AprioriIds from direct upstream nodes of each node

Get default AprioriIds from direct upstream nodes of each node

## Usage

``` r
getDefaultAprioriIds(InputsModel)
```

## Arguments

- InputsModel:

  object of class *InputsModel* or *GRiwrmInputsModel*. See
  [CreateInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.md)

## Value

A [list](https://rdrr.io/r/base/list.html) named with node Ids and
containing the Ids of the upstream nodes that can be used for apriori
parameters.
