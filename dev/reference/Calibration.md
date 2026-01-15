# Calibration of the parameters of one catchment or a network of sub-catchments

Calibration algorithm that optimizes the error criterion selected as
objective function using the provided functions.

## Usage

``` r
# S3 method for class 'GRiwrmInputsModel'
Calibration(
  InputsModel,
  RunOptions,
  InputsCrit,
  CalibOptions,
  useUpstreamQsim = TRUE,
  forceReservoirObs = TRUE,
  ...
)

# S3 method for class 'InputsModel'
Calibration(InputsModel, RunOptions, CalibOptions, ...)

Calibration(InputsModel, ...)
```

## Arguments

- InputsModel:

  \[object of class *InputsModel* or *GRiwrmInputsModel*\] see
  [CreateInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.md)

- RunOptions:

  \[object of class *RunOptions* or *GRiwrmRunOptions*\] see
  [CreateRunOptions](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md)

- InputsCrit:

  \[object of class *InputsCrit* or *GRiwrmInputsCrit*\] see
  [CreateInputsCrit](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsCrit.md)

- CalibOptions:

  \[object of class *CalibOptions* or *GRiwrmCalibOptions*\] see
  [CreateCalibOptions](https://inrae.github.io/airGRiwrm/dev/reference/CreateCalibOptions.md)
  for details

- useUpstreamQsim:

  boolean describing if simulated (`TRUE`) or observed (`FALSE`) flows
  are used for calibration. Default is `TRUE`

- forceReservoirObs:

  boolean indicating if reservoir observed flows must be forced during
  the model run. Default is `TRUE` (See details of
  [RunModel_Reservoir](https://inrae.github.io/airGRiwrm/dev/reference/RunModel_Reservoir.md))

- ...:

  further arguments passed to
  [airGR::Calibration](https://rdrr.io/pkg/airGR/man/Calibration.html),
  see details

## Value

Depending on the class of `InputsModel` argument (respectively
`InputsModel` or `GRiwrmInputsModel` object), the returned value is
respectively:

- an `OutputsCalib` object (See
  [airGR::Calibration](https://rdrr.io/pkg/airGR/man/Calibration.html)
  for more details on this object)

- a `GRiwrmOutputsCalib` object which is a
  [list](https://rdrr.io/r/base/list.html) of `OutputsCalib` objects
  with one item per modeled sub-catchment

## Details

This function can be used either for a catchment (with an *InputsModel*
object), for a network (with a *GRiwrmInputsModel* object), or for an
ungauged node cluster (with a *Ungauged* object).

Argument classes should be consistent to the usage:

- a `InputsModel` argument of class *InputsModel* must be followed by a
  `RunOptions` argument of class *RunOptions*, an `InputsCrit` argument
  of class *InputsCrit* and a `CalibOptions` of class *CalibOptions*

- an `InputsModel` argument of class *GRiwrmInputsModel* must be
  followed by a `RunOptions` argument of class *GRiwrmRunOptions*, an
  `InputsCrit` argument of class *GRiwrmInputsCrit* and a `CalibOptions`
  of class *GRiwrmCalibOptions*

## See also

[`CreateGRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md),
[`CreateInputsModel.GRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md),
[`CreateInputsCrit()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsCrit.md),
[`CreateCalibOptions()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateCalibOptions.md)

## Examples

``` r
# Loading catchment data
data(Severn)

#############################################################################
# EXAMPLE 1 - Basic calibration workflow                                   #
#############################################################################

# Creating catchment network
nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id",
                               "distance_downstream", "area")]
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

# Calibration period selection
# Set aside warm-up period and use the rest for calibration
IndPeriod_Run <- seq(
  which(InputsModel[[1]]$DatesR ==
        (InputsModel[[1]]$DatesR[1] + 365 * 24 * 60 * 60)),
  length(InputsModel[[1]]$DatesR)
)
IndPeriod_WarmUp <- seq(1, IndPeriod_Run[1] - 1)

# Preparation of RunOptions object
RunOptions <- CreateRunOptions(
  InputsModel,
  IndPeriod_WarmUp = IndPeriod_WarmUp,
  IndPeriod_Run = IndPeriod_Run
)

# Calibration criterion: preparation of the InputsCrit object
InputsCrit <- CreateInputsCrit(
  InputsModel = InputsModel,
  FUN_CRIT = ErrorCrit_KGE2,
  RunOptions = RunOptions,
  Obs = Qobs[IndPeriod_Run, ],
  transfo = "sqrt"
)

# Preparation of CalibOptions object
CalibOptions <- CreateCalibOptions(InputsModel)

if (interactive()) { # Too long for CRAN check...
  # Calibration
  OutputsCalib <- suppressWarnings(
    Calibration(InputsModel, RunOptions, InputsCrit, CalibOptions)
  )

  # Simulation
  OutputsModels <- RunModel(
    InputsModel,
    RunOptions = RunOptions,
    Param = extractParam(OutputsCalib)
  )
}
```
