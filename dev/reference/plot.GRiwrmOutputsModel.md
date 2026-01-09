# Function which creates screen plots giving an overview of the model outputs in the GRiwrm network

Function which creates screen plots giving an overview of the model
outputs in the GRiwrm network

## Usage

``` r
# S3 method for class 'GRiwrmOutputsModel'
plot(x, Qobs = NULL, Vobs = NULL, unit = "m3/s", ...)
```

## Arguments

- x:

  \[object of class *GRiwrmOutputsModel*\] see
  [RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md)
  for details

- Qobs:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) time series of
  observed flows (for the same time steps than simulated) (mm/time step)
  with one column by hydrological model output named with the node ID
  (See
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)
  for details)

- Vobs:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) time series of
  observed or targeted storage (for the same time steps than simulated)
  \[m3\] with one column by hydrological model output named with the
  node ID (See
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)
  for details)

- unit:

  (optional) [character](https://rdrr.io/r/base/character.html) flows
  unit ("m3/s" or "mm")

- ...:

  Further arguments for
  [airGR::plot.OutputsModel](https://rdrr.io/pkg/airGR/man/plot.OutputsModel.html)
  and [plot](https://rdrr.io/pkg/airGR/man/plot.OutputsModel.html)

## Value

[list](https://rdrr.io/r/base/list.html) of plots.

## Details

For examples of use see topics
[RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md),
[RunModel_Reservoir](https://inrae.github.io/airGRiwrm/dev/reference/RunModel_Reservoir.md),
and
[RunModel.Supervisor](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md).
