# Merge Two outputs of airGR simulations

Merge Two outputs of airGR simulations

## Usage

``` r
# S3 method for class 'OutputsModel'
merge(x, y, ...)

# S3 method for class 'GRiwrmOutputsModel'
merge(x, y, ...)
```

## Arguments

- x, y:

  **OutputsModel** objects from
  [airGR::RunModel](https://rdrr.io/pkg/airGR/man/RunModel.html),
  [RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md),
  [RunModel.GRiwrmOutputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmOutputsModel.md),
  [RunModel.Supervisor](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md)

- ...:

  Not used

## Value

An object **OutputsModel** with merged times series of simulation
results.
