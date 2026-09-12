# RunModel for hot restart after a previous simulation period

This function restarts a simulation using the state at the end of a
previous simulation (`GRiwrmOutputsModel` object `x`). It allows
redefining the boundary conditions `Qinf`, `Qrelease`, and `Qmin` for
the new period.

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

  `GRiwrmOutputsModel` object resulting from a previous run.

- InputsModel:

  `GRiwrmInputsModel` object (see
  [CreateInputsModel.GRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md))
  or `Supervisor` object (see
  [CreateSupervisor](https://inrae.github.io/airGRiwrm/dev/reference/CreateSupervisor.md)).

- RunOptions:

  List of run options created with
  [CreateRunOptions](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md).

- IndPeriod_Run:

  Integer vector indicating the indices of the time steps to run. Must
  start at the index immediately following the previous run.

- DatesR:

  (optional) `POSIXt` vector of dates for the simulation period. See
  details.

- Qinf:

  (optional) `matrix` or `data.frame` of `numeric` observed flows for
  nodes of type "Direct injection" and "Diversion" (m³ per time step).
  Column names correspond to node IDs. Negative flows are abstracted
  from the model and positive flows are injected to the model. See
  details.

- Qrelease:

  (optional) `matrix` or `data.frame` of `numeric` release flows by
  nodes using the model `RunModel_Reservoir` (m³ per time step). See
  details.

- Qmin:

  (optional) `matrix` or `data.frame` of `numeric` minimum flows for
  downstream of a Diversion node (m³ per time step). Default is zero.
  Column names correspond to node IDs. See details.

- merge_outputs:

  `logical` Merge simulation outputs with the one provided in argument
  `x`.

- ...:

  Further arguments for compatibility with S3 methods.

## Value

An object of class `GRiwrmOutputsModel` (see
[RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md)
for details).

## Details

`IndPeriod_Run` or `DatesR` must be continuous periods starting the time
step after the last simulation time step of the `GRiwrmOutputsModel`
object provided through the argument `x`.

`Qinf`, `Qmin`, and `Qrelease` are used to overwrite the corresponding
arguments provided to
[CreateInputsModel.GRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md)
for the period to be simulated. Therefore, the number of rows of these
arguments must correspond to `IndPeriod_Run` or `DatesR` lengths.

## See also

[`CreateGRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md),
[`CreateInputsModel.GRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md),
[`CreateRunOptions()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md)

Vignette "V07_Combine_tactical_operational_management" in package
airGRiwrm
