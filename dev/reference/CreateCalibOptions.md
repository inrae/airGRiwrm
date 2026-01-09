# Creation of the CalibOptions object

This function can be used either for a single catchment (with an
*InputsModel* object) or for a network of catchments (with a
*GRiwrmInputsModel* object)

## Usage

``` r
# S3 method for class 'GRiwrmInputsModel'
CreateCalibOptions(x, FixedParam = NULL, ...)

CreateCalibOptions(x, FixedParam = NULL, ...)

# S3 method for class 'InputsModel'
CreateCalibOptions(x, FixedParam = NULL, ...)

# S3 method for class 'character'
CreateCalibOptions(x, FixedParam = NULL, ...)

# S3 method for class 'function'
CreateCalibOptions(x, FixedParam = NULL, ...)

# S3 method for class 'RunModel_Reservoir'
CreateCalibOptions(x, FixedParam = NULL, ...)
```

## Arguments

- x:

  For a single catchment, it can be an object of class *InputsModel* or
  a [function](https://rdrr.io/r/base/function.html) or a
  [character](https://rdrr.io/r/base/character.html) corresponding to
  `FUN_MOD` (compliant with **airGR** call). For a network, it should be
  an object of class *GRiwrmInputsModel*. See
  [CreateInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.md)
  for details

- FixedParam:

  (optional) [numeric](https://rdrr.io/r/base/numeric.html) vector or
  list of vectors giving the values set for the non-optimised parameter
  values, as for
  [airGR::CreateCalibOptions](https://rdrr.io/pkg/airGR/man/CreateCalibOptions.html)
  (see details)

- ...:

  arguments passed to
  [airGR::CreateCalibOptions](https://rdrr.io/pkg/airGR/man/CreateCalibOptions.html),
  see details

## Value

Depending on the class of the `InputsModel` argument (respectively
`InputsModel` or `GRiwrmInputsModel` object), the returned value is
respectively:

- a `CalibOptions` object (See
  [airGR::CreateCalibOptions](https://rdrr.io/pkg/airGR/man/CreateCalibOptions.html))

- a `GRiwrmCalibOptions` object which is a
  [list](https://rdrr.io/r/base/list.html) of `CalibOptions` objects
  with one item per modeled sub-catchment

## Details

See
[airGR::CreateCalibOptions](https://rdrr.io/pkg/airGR/man/CreateCalibOptions.html)
documentation for a complete list of arguments.

With a *GRiwrmInputsModel* object, all arguments are applied on each
sub-catchment of the network with some adaptation depending on the model
used on each node.

If the argument `FixedParam` is a
[numeric](https://rdrr.io/r/base/numeric.html)
[vector](https://rdrr.io/r/base/vector.html), it is applied to each node
of the network. Beware that parameters must be adapted depending on the
use of the routing model and of the CemaNeige model on each node. If
`FixedParam` is a [list](https://rdrr.io/r/base/list.html) of
[numeric](https://rdrr.io/r/base/numeric.html) vectors, each item of the
list will be applied on corresponding nodes. Use the id "\*" for
applying a setting on the remaining nodes. Example for applying one
setting for all the nodes except the id "54057":

    FixedParam <- list(`*` = c(NA, NA, NA, NA, NA, 0.25, NA, 10, NA),
                       `54057` = c(0.5, NA, NA, NA, NA, 0.25, NA, 10, NA))

The argument `IsHyst` is ignored since it should be defined previously
with
[CreateInputsModel.GRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md).

Beware, if you transfer a parameter set from an upstream catchment to a
downstream catchment, you must prescribe the celerity parameter (see
example).

See
[transferGRparams](https://inrae.github.io/airGRiwrm/dev/reference/transferGRparams.md)
for more details about how model parameters are transferred to ungauged
nodes.

## See also

[`CreateGRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md),
[`CreateInputsModel.GRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md),
[`CreateRunOptions()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md),
[`CreateInputsCrit()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsCrit.md),
[`Calibration()`](https://inrae.github.io/airGRiwrm/dev/reference/Calibration.md)

## Examples

``` r
# Loading catchment data
data(Severn)

#############################################################################
# EXAMPLE 1 - The one where all nodes are calibrated                        #
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

# Calibration
if (interactive()) { # Too long for CRAN check...
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

#############################################################################
# EXAMPLE 2 - The one where the node "54032" is ungauged                    #
#############################################################################

# Creating catchment network with ungauged node
nodes$model[nodes$gauge_id == "54032"] <- "Ungauged"
griwrm <- CreateGRiwrm(nodes, rename_columns)
#> Ungauged node '54032' automatically gets the node '54057' as parameter donor

# Preparation of InputsModel object
InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap)
#> CreateInputsModel.GRiwrm: Processing sub-basin 54095...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54002...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54029...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54001...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54032...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54057...

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

# Calibration
if (interactive()) { # Too long for CRAN check...
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

#############################################################################
# EXAMPLE 3 - Parameter transfer from donor catchment "54001"                #
# In this case, we want the "54032" node to be ungauged, and the "54001"     #
# node to give its parameters                                             #
#############################################################################

# Creating catchment network with parameter transfer
nodes$model[nodes$gauge_id == "54032"] <- "Ungauged"
nodes$donor <- as.character(NA)
nodes$donor[nodes$gauge_id == "54032"] <- "54001"
griwrm <- CreateGRiwrm(nodes, rename_columns)

# Preparation of InputsModel object
InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap)
#> CreateInputsModel.GRiwrm: Processing sub-basin 54095...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54002...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54029...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54001...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54032...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54057...

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

#############################################################################
# EXAMPLE 4 - Parameter transfer with prescribed celerity parameter         #
# In this case, we want the "54032" node to be ungauged, and the "54029"     #
# node to give its parameters. Beware, the "54029" node has no celerity     #
# parameters, as it is an upstream catchment, but "54032" needs one         #
#############################################################################

# Creating catchment network with parameter transfer and celerity
nodes$model[nodes$gauge_id == "54032"] <- "Ungauged"
nodes$donor <- as.character(NA)
nodes$donor[nodes$gauge_id == "54032"] <- "54029"
griwrm <- CreateGRiwrm(nodes, rename_columns)

# Preparation of InputsModel object
InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap)
#> CreateInputsModel.GRiwrm: Processing sub-basin 54095...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54002...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54029...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54001...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54032...
#> CreateInputsModel.GRiwrm: Processing sub-basin 54057...

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

# Preparation of CalibOptions object with prescribed celerity
# Here you need to prescribe the celerity parameter for node "54032"
CalibOptions <- CreateCalibOptions(
  InputsModel,
  FixedParam = list("54032" = c(1, NA, NA, NA, NA))
)

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
