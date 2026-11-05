# Run of a rainfall-runoff model on a sub-basin

Run ONLY the rainfall-runoff model, upstream flow routing and Diversions
are not processed.

## Usage

``` r
# S3 method for class 'GR'
RunModel(x, RunOptions, Param, ...)
```

## Arguments

- x:

  \[object of class `InputsModel`\] `InputsModel` for
  [airGR::RunModel](https://rdrr.io/pkg/airGR/man/RunModel.html)

- RunOptions:

  \[object of class *RunOptions*\] see
  [airGR::CreateRunOptions](https://rdrr.io/pkg/airGR/man/CreateRunOptions.html)
  for details

- Param:

  [numeric](https://rdrr.io/r/base/numeric.html) vector of model
  parameters (See details for SD lag model)

- ...:

  further arguments passed to or from other methods

## Value

\[list\] see
[`RunModel_GR4J`](https://rdrr.io/pkg/airGR/man/RunModel_GR4J.html) or
[`RunModel_CemaNeigeGR4J`](https://rdrr.io/pkg/airGR/man/RunModel_CemaNeigeGR4J.html)
for details.

If `InputsModel` parameter has been created for using a semi-distributed
(SD) lag model (See
[`CreateInputsModel`](https://rdrr.io/pkg/airGR/man/CreateInputsModel.html)),
the list value contains an extra item named `QsimDown` which is a
numeric series of simulated discharge \[mm/time step\] related to the
run-off contribution of the downstream sub-catchment.

## Details

This function runs
[airGR::RunModel](https://rdrr.io/pkg/airGR/man/RunModel.html) (without
lag) and add an item `Qsim_m3` to the returned *OutputsModel* object.
