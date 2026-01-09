# Creation of an InputsModel object for an **airGRiwrm** network

Creation of an InputsModel object for an **airGRiwrm** network

## Usage

``` r
# S3 method for class 'GRiwrm'
CreateInputsModel(
  x,
  DatesR,
  Precip = NULL,
  PotEvap = NULL,
  Qinf = NULL,
  Qobs = NULL,
  Qmin = NULL,
  Qrelease = NULL,
  PrecipScale = TRUE,
  TempMean = NULL,
  TempMin = NULL,
  TempMax = NULL,
  ZInputs = NULL,
  HypsoData = NULL,
  NLayers = 5,
  IsHyst = FALSE,
  FUN_REGUL = NULL,
  ...
)
```

## Arguments

- x:

  [GRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)
  object containing diagram of the semi-distributed model (See
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md))

- DatesR:

  [POSIXt](https://rdrr.io/r/base/DateTimeClasses.html) vector of dates

- Precip:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) of
  [numeric](https://rdrr.io/r/base/numeric.html) containing
  precipitation in \[mm per time step\]. Column names correspond to node
  IDs

- PotEvap:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) of
  [numeric](https://rdrr.io/r/base/numeric.html) containing potential
  evaporation \[mm per time step\]. Column names correspond to node IDs

- Qinf:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) of
  [numeric](https://rdrr.io/r/base/numeric.html) containing observed
  flows. It must be provided only for nodes of type "Direct injection"
  and "Diversion". See
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)
  for details about these node types. Unit is \[mm per time step\] for
  nodes with an area, and \[m³ per time step\] for nodes with `area=NA`.
  Column names correspond to node IDs. Negative flows are abstracted
  from the model and positive flows are injected to the model

- Qobs:

  (deprecated) use `Qinf` instead

- Qmin:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) of
  [numeric](https://rdrr.io/r/base/numeric.html) containing minimum
  flows that must be provided downstream of a node with a Diversion \[m³
  per time step\]. Default is zero. Column names correspond to node IDs

- Qrelease:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) of
  [numeric](https://rdrr.io/r/base/numeric.html) containing targeted
  release flows by nodes using the
  [RunModel_Reservoir](https://inrae.github.io/airGRiwrm/dev/reference/RunModel_Reservoir.md)
  model \[m³ per time step\]. Column names correspond to node IDs

- PrecipScale:

  (optional) [logical](https://rdrr.io/r/base/logical.html)
  [vector](https://rdrr.io/r/base/vector.html) indicating if the mean of
  the precipitation interpolated on the elevation layers must be kept or
  not, required to create CemaNeige module inputs, default `TRUE` (the
  mean of the precipitation is kept to the original value). Column names
  correspond to node IDs

- TempMean:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) of time series of
  mean air temperature \[°C\], required to create the CemaNeige module
  inputs. Column names correspond to node IDs

- TempMin:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) of time series of
  minimum air temperature \[°C\], possibly used to create the CemaNeige
  module inputs. Column names correspond to node IDs

- TempMax:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) of time series of
  maximum air temperature \[°C\], possibly used to create the CemaNeige
  module inputs. Column names correspond to node IDs

- ZInputs:

  (optional) [numeric](https://rdrr.io/r/base/numeric.html)
  [vector](https://rdrr.io/r/base/vector.html) giving the mean elevation
  of the Precip and Temp series (before extrapolation) \[m\], possibly
  used to create the CemaNeige module input. Column names correspond to
  node IDs

- HypsoData:

  (optional) [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) containing 101
  [numeric](https://rdrr.io/r/base/numeric.html) rows: min, q01 to q99
  and max of catchment elevation distribution \[m\], if not defined a
  single elevation is used for CemaNeige. Column names correspond to
  node IDs

- NLayers:

  (optional) [numeric](https://rdrr.io/r/base/numeric.html) vector
  (integer) giving the number of elevation layers requested \[-\],
  required to create CemaNeige module inputs, default=5. Column names
  correspond to node IDs

- IsHyst:

  [logical](https://rdrr.io/r/base/logical.html) indicating if the
  hysteresis version of CemaNeige is used. See details of
  [`airGR::CreateRunOptions()`](https://rdrr.io/pkg/airGR/man/CreateRunOptions.html).

- FUN_REGUL:

  [list](https://rdrr.io/r/base/list.html) of functions for local
  regulation (See details)

- ...:

  used for compatibility with S3 methods

## Value

A *GRiwrmInputsModel* object which is a
[list](https://rdrr.io/r/base/list.html) of *InputsModel* objects
created by
[`airGR::CreateInputsModel()`](https://rdrr.io/pkg/airGR/man/CreateInputsModel.html)
with one item per modeled sub-catchment.

## Details

Meteorological data are needed for the nodes of the network that
represent a catchment simulated by a rainfall-runoff model. Instead of
[`airGR::CreateInputsModel()`](https://rdrr.io/pkg/airGR/man/CreateInputsModel.html)
that has [numeric](https://rdrr.io/r/base/numeric.html)
[vector](https://rdrr.io/r/base/vector.html) as time series inputs, this
function uses [matrix](https://rdrr.io/r/base/matrix.html) or
[data.frame](https://rdrr.io/r/base/data.frame.html) with the id of the
sub-catchments as column names. For single values (`ZInputs` or
`NLayers`), the function requires named
[vector](https://rdrr.io/r/base/vector.html) with the id of the
sub-catchment as name item. If an argument is optional, only the column
or the named item has to be provided.

See
[`airGR::CreateInputsModel()`](https://rdrr.io/pkg/airGR/man/CreateInputsModel.html)
documentation for details concerning each argument

The number of rows of `Precip`, `PotEvap`, `Qinf`, `Qmin`, `Qrelease`,
`TempMean`, `TempMin`, `TempMax` must be the same as the length of
`DatesR` (each row corresponds to a time step defined in `DatesR`).

For various examples of use see topics
[`RunModel.GRiwrmInputsModel()`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md),
[`RunModel_Reservoir()`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel_Reservoir.md),
and
[`RunModel.Supervisor()`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md).

For example of use of Direct Injection nodes, see vignettes
"V03_Open-loop_influenced_flow" and
"V04_Closed-loop_regulated_withdrawal".

For example of use of Diversion nodes, see example in
[`RunModel.GRiwrmInputsModel()`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md)
topic and vignette "V06_Modelling_regulated_diversion".

### The `FUN_REGUL` parameter

`FUN_REGUL` argument is a named [list](https://rdrr.io/r/base/list.html)
of functions that modify the node `InputsModel` before sending it to the
node's model. This feature is useful for modifying data such as
`InputsModel$Qdiv` or `InputsModel$Qrelease` giving simulated flows
already available from upstream nodes. Each item of the list has a name
corresponding to the node on which the function is applied. Each
function must follow this interface:
`function(InputsModel, RunOptions, OutputsModel, env)` where the
arguments are:

- `InputsModel`, the *InputsModel* object of the current node

- `RunOptions`, the *RunOptions* object of the current node

- `OutputsModel`, the *GRiwrmOutputsModel* object of the upstream and
  sibling nodes that have been already computed when the computation of
  the current node occurs

- `env`, the [environment](https://rdrr.io/r/base/environment.html) of
  the
  [`RunModel.GRiwrmInputsModel()`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md)
  function

The functions embedded in `FUN_REGUL` should all return the argument
`InputsModel` after calculation.

## See also

[`CreateGRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md),
[`CreateRunOptions()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md),
[`RunModel.GRiwrmInputsModel()`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md)

## Examples

``` r
# Loading catchment data
data(Severn)

# Creating catchment network
nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
nodes$model <- "RunModel_GR4J"
rename_columns <- list(id = "gauge_id",
                       down = "downstream_id",
                       length = "distance_downstream")
griwrm <- CreateGRiwrm(nodes, rename_columns)
griwrm
#>      id  down length    area         model donor
#> 4 54095 54001     42 3722.68 RunModel_GR4J 54095
#> 5 54002 54057     43 2207.95 RunModel_GR4J 54002
#> 6 54029 54032     32 1483.65 RunModel_GR4J 54029
#> 3 54001 54032     45 4329.90 RunModel_GR4J 54001
#> 2 54032 54057     15 6864.88 RunModel_GR4J 54032
#> 1 54057  <NA>     NA 9885.46 RunModel_GR4J 54057

# Preparation of InputsModel object
BasinsObs <- Severn$BasinsObs
DatesR <- BasinsObs[[1]]$DatesR
PrecipTot <- cbind(sapply(BasinsObs, function(x) {x$precipitation}))
PotEvapTot <- cbind(sapply(BasinsObs, function(x) {x$peti}))
Qobs <- cbind(sapply(BasinsObs, function(x) {x$discharge_spec}))
Precip <- ConvertMeteoSD(griwrm, PrecipTot)
PotEvap <- ConvertMeteoSD(griwrm, PotEvapTot)
InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap)
#> CreateInputsModel.GRiwrm: Processing sub-basin 54095...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54002...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54029...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54001...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54032...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54057...
```
