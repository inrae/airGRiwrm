# Compute the meteorological data of a node at the whole-catchment scale

Replace, on a copy, the meteorological time series of a node by their
area-weighted mean over the whole catchment of the node (the node and
all its upstream nodes). The original object is not modified: the time
series of the object passed in `OutputsModel` are read only.

## Usage

``` r
adjustMeteoToCatchment(
  OM_id,
  id,
  OutputsModel,
  griwrm = attr(OutputsModel, "GRiwrm")
)
```

## Arguments

- OM_id:

  \[object of class *OutputsModel*\] the *OutputsModel* object of the
  node `id` (see
  [RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md))

- id:

  \[character\] the ID of the node (See
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md))

- OutputsModel:

  \[object of class *GRiwrmOutputsModel*\] the object produced by
  [RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md),
  or a [list](https://rdrr.io/r/base/list.html) of *OutputsModel*
  objects with one item by node (names are the node IDs). When a simple
  [list](https://rdrr.io/r/base/list.html) is passed, it must carry the
  "GRiwrm" attribute or `griwrm` must be provided explicitly (see below)

- griwrm:

  \[object of class *GRiwrm*\] the network of the simulation (See
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)).
  Defaults to the "GRiwrm" attribute of `OutputsModel` (set by
  [RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md)).
  When `OutputsModel` is a [list](https://rdrr.io/r/base/list.html)
  which does not carry this attribute, the default resolves to `NULL`
  and `griwrm` must be passed explicitly, otherwise the catchment of the
  node cannot be identified.

## Value

A copy of `OM_id` with the meteorological data at the whole-catchment
scale.

## Details

The meteorological data stored in the *OutputsModel* object of a node
come from the meteorological data of its local sub-basin only, whereas
the simulated discharge of the node integrates the flows of its whole
catchment. This function computes, for each time step, the area-weighted
mean of these data over the whole catchment, so that they can be used to
compute statistics at the catchment scale (or displayed, see
[plot.GRiwrmOutputsModel](https://inrae.github.io/airGRiwrm/dev/reference/plot.GRiwrmOutputsModel.md)).

The following time series are replaced when they are available in the
*OutputsModel* object of the node:

- `Precip` and `PotEvap`;

- for a CemaNeige node, the per-layer series `Temp` (mean air
  temperature), `Psol` (solid precipitation) and `SnowPack` (snow pack
  state) stored in `OM_id$CemaNeigeLayers`, one item by elevation layer
  (See
  [CreateRunOptions](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md)).

Each node contributes to the mean with its own time series weighted by
the area of its local sub-basin: the part of the catchment for which the
meteorological data of the node are the "real" data (See
`getNodeLocalArea`). A node cannot contribute when its local sub-basin
area is unknown or zero, or when the concerned time series is not
available; a missing value in a time series excludes the node from the
mean at the concerned time steps. When no node can contribute, the time
series of the node `id` itself are returned unchanged (graceful
behaviour). When the ID of the node is shared with a Diversion node (two
rows of the network with the same `id`, see
[CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)),
the local sub-basin area of the shared ID cannot be computed and the
node is simply excluded from the weighted mean (graceful behaviour).

The elevation layers of the CemaNeige nodes are matched by index (layer
`i` of a node with layer `i` of the other nodes) and a node with fewer
layers does not contribute to the upper layer indices. The elevation of
a given layer index slightly differs from one node to the other: this
approximation is assumed. `SnowPack` is a simulated state and `Psol` an
output of the snow module: both are adjusted as well, for the
consistency of the whole-catchment display of the snow and temperature
series.

The network is read from the `griwrm` argument, which defaults to the
"GRiwrm" attribute set on the *GRiwrmOutputsModel* object by
[RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md).
When `OutputsModel` is a plain [list](https://rdrr.io/r/base/list.html)
without this attribute, the default resolves to `NULL` and `griwrm` must
be passed explicitly.

## See also

[`plot.GRiwrmOutputsModel()`](https://inrae.github.io/airGRiwrm/dev/reference/plot.GRiwrmOutputsModel.md),
[`CreateGRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)

## Examples

``` r
##############################################################
# Meteorological data at the catchment scale of a CemaNeige   #
# network: the network used here is a chain of 2 nodes built  #
# on the airGR example data set (L0123001)                    #
##############################################################
data(L0123001, package = "airGR")

# Network: Upstream (360 km2) -> Downstream (720 km2, i.e. a local
# sub-basin of 360 km2)
griwrm <- CreateGRiwrm(data.frame(
  id = c("Upstream", "Downstream"),
  down = c("Downstream", NA),
  length = c(0, NA),
  area = c(360, 720),
  model = rep("RunModel_CemaNeigeGR4J", 2)
))

# The 2 nodes share the meteorological data of the example data set
InputsPrecip <- matrix(BasinObs$P, ncol = 2, nrow = length(BasinObs$P))
InputsPotEvap <- matrix(BasinObs$E, ncol = 2, nrow = length(BasinObs$E))
InputsTemp <- matrix(BasinObs$T, ncol = 2, nrow = length(BasinObs$T))
InputsHypso <- matrix(rep(BasinInfo$HypsoData, 2), ncol = 2)
InputsZ <- rep(BasinInfo$HypsoData[51], 2)
colnames(InputsPrecip) <- colnames(InputsPotEvap) <- colnames(InputsTemp) <-
  colnames(InputsHypso) <- names(InputsZ) <- griwrm$id

InputsModel <- CreateInputsModel(
  griwrm,
  DatesR = BasinObs$DatesR,
  Precip = InputsPrecip,
  PotEvap = InputsPotEvap,
  TempMean = InputsTemp,
  ZInputs = InputsZ,
  HypsoData = InputsHypso
)
#> CreateInputsModel.GRiwrm: Processing sub-basin Upstream...
#> input series were successfully created on 5 elevation layers for use by CemaNeige
#> CreateInputsModel.GRiwrm: Processing sub-basin Downstream...
#> input series were successfully created on 5 elevation layers for use by CemaNeige

# Short run period (1 month) for a fast example. `Precip` and `PotEvap` are
# requested in `Outputs_Sim` because they are not part of the default
# outputs of a CemaNeige node
RunOptions <- CreateRunOptions(
  InputsModel,
  IndPeriod_Run = seq(1, 30),
  Outputs_Sim = c("DatesR", "Qsim", "CemaNeigeLayers", "Precip", "PotEvap")
)
#> model warm up period not defined: default configuration used
#>  no data were found for model warm up! No warm up period is used
#> Warning: model warm up period not defined: default configuration used
#>  no data were found for model warm up!
#> Warning: 'MeanAnSolidPrecip' not defined: it was automatically set to c(84,84,84,84,84)  from the 'InputsModel' given to the function. Be careful in case your application is (short-term) forecasting.
#> model warm up period not defined: default configuration used
#>  no data were found for model warm up! No warm up period is used
#> Warning: model warm up period not defined: default configuration used
#>  no data were found for model warm up!
#> Warning: 'MeanAnSolidPrecip' not defined: it was automatically set to c(84,84,84,84,84)  from the 'InputsModel' given to the function. Be careful in case your application is (short-term) forecasting.

# `Downstream` receives the simulated flow of `Upstream`: its parameter
# vector starts with the celerity of the routing model (airGR::RunModel_Lag)
ParamCN <- c(
  X1 = 408.774, X2 = 2.646, X3 = 131.264, X4 = 1.174,
  CNX1 = 0.962, CNX2 = 2.249
)
Param <- list(Upstream = ParamCN, Downstream = c(1, ParamCN))
OutputsModels <- RunModel(InputsModel, RunOptions = RunOptions, Param = Param)
#> RunModel.GRiwrmInputsModel: Processing sub-basin Upstream...
#> RunModel.GRiwrmInputsModel: Processing sub-basin Downstream...

# Area-weighted mean of the meteorological data over the whole catchment
# of the node `Downstream` (the local sub-basin areas are 360 km2 each, so
# the mean is the arithmetic mean of the 2 nodes here)
OutputsDownstream <- adjustMeteoToCatchment(
  OutputsModels[["Downstream"]],
  "Downstream",
  OutputsModels
)

# Precipitation at the whole-catchment scale and per-layer temperature
head(OutputsDownstream$Precip)
#> [1]  4.1 15.9  0.8  0.0  0.0  0.0
head(OutputsDownstream$CemaNeigeLayers[[1]]$Temp)
#> [1]  1.44178  1.14178  1.84395  1.44612 -0.65171  1.85263
```
