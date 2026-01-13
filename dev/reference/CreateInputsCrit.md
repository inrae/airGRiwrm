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
  [vector](https://rdrr.io/r/base/vector.html) of
  [character](https://rdrr.io/r/base/character.html) used for the
  parameter regularization (see details)

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
