# Creation of the InputsCrit object required to the `ErrorCrit` functions

This function can be used either for a catchment (with an *InputsModel*
object) or for a network (with a *GRiwrmInputsModel* object)

## Usage

``` r
# S3 method for class 'GRiwrmInputsModel'
CreateInputsCrit(
  InputsModel,
  FUN_CRIT = ErrorCrit_KGE2,
  RunOptions,
  Obs,
  AprioriIds = getDefaultAprioriIds(InputsModel),
  k = 0.15,
  AprCelerity = 1,
  ...
)

# S3 method for class 'InputsModel'
CreateInputsCrit(InputsModel, FUN_CRIT, ...)

CreateInputsCrit(InputsModel, ...)
```

## Arguments

- InputsModel:

  object of class *InputsModel* or *GRiwrmInputsModel*. See
  [CreateInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.md)

- FUN_CRIT:

  \[function (atomic or list)\] error criterion function (e.g.
  [airGR::ErrorCrit_RMSE](https://rdrr.io/pkg/airGR/man/ErrorCrit_RMSE.html),
  [airGR::ErrorCrit_NSE](https://rdrr.io/pkg/airGR/man/ErrorCrit_NSE.html))

- RunOptions:

  object of class *RunOptions* or *GRiwrmRunOptions*, see
  [CreateRunOptions](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md)

- Obs:

  [numeric](https://rdrr.io/r/base/numeric.html),
  [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) series of
  observed flows, see details

- AprioriIds:

  (optional) named [list](https://rdrr.io/r/base/list.html) or named
  [vector](https://rdrr.io/r/base/vector.html) whose entries are
  [character](https://rdrr.io/r/base/character.html) vectors (ids of a
  priori sub-catchments) or
  [numeric](https://rdrr.io/r/base/numeric.html) vectors (user-defined a
  priori parameters), used for the parameter regularization (see
  details)

- k:

  (optional) [numeric](https://rdrr.io/r/base/numeric.html) weight
  coefficient used in the parameter regularization (See
  [airGR::CreateInputsCrit_Lavenne](https://rdrr.io/pkg/airGR/man/CreateInputsCrit_Lavenne.html))

- AprCelerity:

  (optional) [numeric](https://rdrr.io/r/base/numeric.html) Default
  celerity used as a priori parameter for upstream catchments

- ...:

  arguments passed to
  [airGR::CreateInputsCrit](https://rdrr.io/pkg/airGR/man/CreateInputsCrit.html),
  see details

## Value

Depending on the class of `InputsModel` argument (respectively
`InputsModel` and `GRiwrmInputsModel` object), the returned value is
respectively:

- a `InputsCrit` object (See
  [airGR::CreateInputsCrit](https://rdrr.io/pkg/airGR/man/CreateInputsCrit.html))

- a `GRiwrmInputsCrit` object which is a
  [list](https://rdrr.io/r/base/list.html) of `InputsCrit` objects with
  one item per modeled sub-catchment

## Details

See
[airGR::CreateInputsCrit](https://rdrr.io/pkg/airGR/man/CreateInputsCrit.html)
documentation for a complete list of arguments.

`Obs` argument is equivalent to the same argument in
[airGR::CreateInputsCrit](https://rdrr.io/pkg/airGR/man/CreateInputsCrit.html)
except that it must be a [matrix](https://rdrr.io/r/base/matrix.html) or
a [data.frame](https://rdrr.io/r/base/data.frame.html) if `InputsModel`
is a *GRiwrmInputsModel* object. Then, each column of the
[matrix](https://rdrr.io/r/base/matrix.html) or
[data.frame](https://rdrr.io/r/base/data.frame.html) represents the
observations of one of the simulated node with the name of the columns
representing the id of each node.

With a *GRiwrmInputsModel* object, all arguments are applied on each
sub-catchments of the network.

Parameter regularization consists of defining a priori parameters which
are used in a composed criterion based on the formula proposed by
Lavenne et al. (2019) (See
[airGR::CreateInputsCrit_Lavenne](https://rdrr.io/pkg/airGR/man/CreateInputsCrit_Lavenne.html)).
The parameter `AprioriIds` allows to define which neighbor
sub-catchments are used for providing a priori parameters. Its format is
as follows:
`AprioriIds <- list("Downstream sub-catchment 1" = c("A priori upstream sub-catchment 1", ...))`
where the quoted strings are the ids of the sub-catchments. The nodes
providing a priori parameters must be calibrated before the current one.
The sequence order of calibration can be checked with
[getNodeRanking](https://inrae.github.io/airGRiwrm/dev/reference/getNodeRanking.md).
If the latter is not adequate, this order can be forced by setting the
node providing a priori parameters as donor of the current node in
[CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md).
See vignettes for more details. The parameter `AprCelerity` is a default
value used as a priori for the parameter 'Celerity' in case of an
upstream catchment (without celerity parameter) is used as a priori
catchment. In the calibration process, all a priori parameter sets are
tested and the one getting the best `ErrorCrit` score is used for the
parameter regularization. By default, the immediate upstream catchments
are used as a priori catchments, as determined by
[getDefaultAprioriIds](https://inrae.github.io/airGRiwrm/dev/reference/getDefaultAprioriIds.md).

The entries of `AprioriIds` can also be
[numeric](https://rdrr.io/r/base/numeric.html) vectors of user-defined a
priori parameters. In that case, the given parameters are used as the a
priori parameters of the De Lavenne criterion of the named sub-catchment
instead of the parameters of an upstream sub-catchment:
`AprioriIds <- list("Sub-catchment 1" = c("Upstream sub-catchment 1"), "Sub-catchment 2" = c(1, 50, -1, 3, 2))`.
A user-defined a priori parameter must have one value per model
parameter of the sub-catchment (in the same order as the `Param`
argument of
[airGR::RunModel](https://rdrr.io/pkg/airGR/man/RunModel.html): e.g.
`c(celerity, X1, X2, X3, X4)` for a GR4J sub-catchment with celerity).
User-defined a priori parameters are used as-is: no default celerity is
added and no X4 rescaling is applied.

## References

De Lavenne, A., Andréassian, V., Thirel, G., Ramos, M.-H., Perrin, C.,
2019. A Regularization Approach to Improve the Sequential Calibration of
a Semidistributed Hydrological Model. Water Resources Research 55,
8821–8839.
[doi:10.1029/2018WR024266](https://doi.org/10.1029/2018WR024266)

## See also

[`CreateGRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md),
[`CreateInputsModel.GRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md),
[`CreateRunOptions()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateRunOptions.md),
[`CreateCalibOptions()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateCalibOptions.md),
[`Calibration()`](https://inrae.github.io/airGRiwrm/dev/reference/Calibration.md)

## Examples

``` r
# Loading catchment data
data(Severn)

# Creating catchment network
nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id",
                               "distance_downstream", "area")]
nodes$model <- "RunModel_GR4J"
rename_columns <- list(id = "gauge_id",
                       down = "downstream_id",
                       length = "distance_downstream")
griwrm <- CreateGRiwrm(nodes, rename_columns)

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

# De Lavenne criterion with the default node-based a priori parameters
InputsCrit <- CreateInputsCrit(
  InputsModel = InputsModel,
  FUN_CRIT = ErrorCrit_KGE2,
  RunOptions = RunOptions,
  Obs = Qobs[IndPeriod_Run, ],
  transfo = "sqrt"
)

# De Lavenne criterion with a user-defined a priori parameter for node
# "54057" instead of the parameters of an upstream sub-catchment:
# one value per model parameter of the node (e.g. for a GR4J sub-catchment
# with celerity: celerity, X1, X2, X3, X4)
InputsCrit <- CreateInputsCrit(
  InputsModel = InputsModel,
  FUN_CRIT = ErrorCrit_KGE2,
  RunOptions = RunOptions,
  Obs = Qobs[IndPeriod_Run, ],
  AprioriIds = list(
    "54057" = c(1, 50, -1, 3, 2),
    "54032" = "54001",
    "54001" = "54095"
  ),
  transfo = "sqrt"
)
```
