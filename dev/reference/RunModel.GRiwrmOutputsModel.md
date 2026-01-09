# RunModel for hot restart after a previous simulation period

This function allows to restart a simulation at the end of a previous
simulation period. Parameters `Qinf`, `Qrelease`, and `Qmin` can be
redefined for this new simulation period.

## Usage

``` r
# S3 method for class 'GRiwrmOutputsModel'
RunModel(
  x,
  InputsModel,
  RunOptions,
  IndPeriod_Run = which(InputsModel[[1]]$DatesR %in% DatesR),
  DatesR = getNextTimeSteps(x),
  Qinf = NULL,
  Qrelease = NULL,
  Qmin = NULL,
  merge_outputs = TRUE,
  ...
)
```

## Arguments

- x:

  Object returned by
  [RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md),
  [RunModel.Supervisor](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md),
  or RunModel.GRiwrmOutputsModel

- InputsModel:

  [GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md)
  (see
  [CreateInputsModel.GRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md))
  or
  [Supervisor](https://inrae.github.io/airGRiwrm/dev/reference/CreateSupervisor.md)
  (See
  [CreateSupervisor](https://inrae.github.io/airGRiwrm/dev/reference/CreateSupervisor.md))

- RunOptions:

  \[object of class *GRiwrmRunOptions*\] see
  [CreateRunOptions.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md)
  for details

- IndPeriod_Run:

  \[numeric\] index of period to be used for the model run \[-\]. See
  details

- DatesR:

  (optional) [POSIXt](https://rdrr.io/r/base/DateTimeClasses.html)
  vector of dates of period to be used for the model run. See details

- Qinf:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) of
  [numeric](https://rdrr.io/r/base/numeric.html) containing observed
  flows. It must be provided only for nodes of type "Direct injection"
  and "Diversion" \[m3 per time step\]. Column names correspond to node
  IDs. Negative flows are abstracted from the model and positive flows
  are injected to the model. See details

- Qrelease:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) of
  [numeric](https://rdrr.io/r/base/numeric.html) containing release
  flows by nodes using the model `RunModel_Reservoir` \[m3 per time
  step\]. See details

- Qmin:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) of
  [numeric](https://rdrr.io/r/base/numeric.html) containing minimum
  flows to let downstream of a node with a Diversion \[m3 per time
  step\]. Default is zero. Column names correspond to node IDs. See
  details

- merge_outputs:

  [logical](https://rdrr.io/r/base/logical.html) Merge simulation
  outputs with the one provided in argument `x`

- ...:

  Further arguments for compatibility with S3 methods

## Value

An object of class *GRiwrmOutputsModel*. This object is a
[list](https://rdrr.io/r/base/list.html) of *OutputsModel* objects
produced by
[RunModel.InputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.InputsModel.md)
for each node of the semi-distributed model.

It also contains the following attributes (see
[attr](https://rdrr.io/r/base/attr.html)):

- "Qm3s": a [data.frame](https://rdrr.io/r/base/data.frame.html)
  containing the dates of simulation and one column by node with the
  simulated flows in cubic meters per seconds (See
  [plot.Qm3s](https://inrae.github.io/airGRiwrm/dev/reference/plot.Qm3s.md))

- "GRiwrm": a copy of the *GRiwrm* object produced by
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)
  and used for the simulation

- "TimeStep": time step of the simulation in seconds

## Details

`IndPeriod_Run` or `DatesR` must must be continuous periods starting the
time step after the last simulation time step of the
`GRiwrmOutputsModel` object provided through the argument `x`.

`Qinf`, `Qmin`, and `Qrelease` are used for overwriting the
corresponding arguments provided to
[CreateInputsModel.GRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md)
for the period to be simulated. Therefore, the number of rows of these
arguments must correspond to `IndPeriod_Run` or `DatesR` lengths.
