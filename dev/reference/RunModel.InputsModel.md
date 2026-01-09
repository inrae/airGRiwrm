# Wrapper for [airGR::RunModel](https://rdrr.io/pkg/airGR/man/RunModel.html) for one sub-basin

Wrapper for
[airGR::RunModel](https://rdrr.io/pkg/airGR/man/RunModel.html) for one
sub-basin

## Usage

``` r
# S3 method for class 'InputsModel'
RunModel(x = NULL, RunOptions, Param, FUN_MOD = NULL, InputsModel = NULL, ...)
```

## Arguments

- x:

  \[object of class *InputsModel*\] see
  [airGR::CreateInputsModel](https://rdrr.io/pkg/airGR/man/CreateInputsModel.html)
  for details

- RunOptions:

  \[object of class *RunOptions*\] see
  [`CreateRunOptions`](https://rdrr.io/pkg/airGR/man/CreateRunOptions.html)
  for details

- Param:

  \[numeric\] vector of model parameters (See details for SD lag model)

- FUN_MOD:

  \[function\] hydrological model function (e.g.
  [`RunModel_GR4J`](https://rdrr.io/pkg/airGR/man/RunModel_GR4J.html),
  [`RunModel_CemaNeigeGR4J`](https://rdrr.io/pkg/airGR/man/RunModel_CemaNeigeGR4J.html))

- InputsModel:

  \[object of class *InputsModel*\] see
  [`CreateInputsModel`](https://rdrr.io/pkg/airGR/man/CreateInputsModel.html)
  for details

- ...:

  Further arguments for compatibility with S3 methods

## Value

\[object of class *OutputsModel*\] returned by
[airGR::RunModel](https://rdrr.io/pkg/airGR/man/RunModel.html) (See
Value section of
[airGR::RunModel_GR4J](https://rdrr.io/pkg/airGR/man/RunModel_GR4J.html))
completed by new items:

- `Qsim_m3`: simulated flow in cubic meters per time step

- `Qover_m3` volumes of over abstractions which occurs when
  `RunModel_Lag` warns for negative simulated flows

- `Qnat`: only present in case of Diversion in the node, simulated flow
  in mm per time step before application of the Diversion

- `Qdiv_m3`: only present in case of Diversion in the node, simulated
  diverted flow in cubic meters per time step. The latter differs from
  the flows time series provided in argument `Qinf` of
  [CreateInputsModel.GRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md)
  by the limitation of diversion applied by the minimum flow threshold
  `Qmin` to keep flowing in the river
