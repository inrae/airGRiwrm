# Check if a node is downstream or upstream another one

Check if a node is downstream or upstream another one

## Usage

``` r
isNodeDownstream(x, current_node, candidate_node)

# S3 method for class 'GRiwrmInputsModel'
isNodeDownstream(x, current_node, candidate_node)

# S3 method for class 'GRiwrm'
isNodeDownstream(x, current_node, candidate_node)

isNodeUpstream(x, current_node, candidate_node)

# S3 method for class 'GRiwrm'
isNodeUpstream(x, current_node, candidate_node)

# S3 method for class 'GRiwrmInputsModel'
isNodeUpstream(x, current_node, candidate_node)
```

## Arguments

- x:

  [GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md)
  object (see
  [CreateInputsModel.GRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md))
  or
  [GRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)
  (See
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md))

- current_node:

  [character](https://rdrr.io/r/base/character.html) with the id of the
  current node

- candidate_node:

  [character](https://rdrr.io/r/base/character.html) with the id of the
  node for which we want to know if it is downstream or upstream
  `current_node`

## Value

[logical](https://rdrr.io/r/base/logical.html) `TRUE` if the node with
the id `down_candidate` is downstream or upstream the node with the id
`current_node`
