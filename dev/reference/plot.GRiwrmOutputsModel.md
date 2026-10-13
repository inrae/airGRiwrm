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

The simulated discharge of a node integrates the flows of the whole
catchment of the node, while the meteorological time series stored in
the *OutputsModel* of the node (precipitation and potential
evapotranspiration) come from the meteorological data of its local
sub-basin only. Therefore, when these meteorological time series are
available in the *GRiwrmOutputsModel* object (i.e. requested in
`Outputs_Sim`, see
[CreateRunOptions](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md)),
they are replaced on the fly, without modifying the object, by their
area-weighted mean over the whole catchment of the node (the node and
all its upstream nodes), so that the displayed meteorology reflects the
whole catchment (See issue inrae/airGRiwrm#178).

When the ID of a node is shared with a Diversion node (two rows of the
network with the same `id`, see
[CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)),
the local sub-basin area of the shared ID cannot be computed and the
node is simply excluded from the weighted mean (graceful behaviour).
