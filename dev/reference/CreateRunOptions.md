# Creation of the RunOptions object

This function can be used either for a catchment (with an *InputsModel*
object) or for a network (with a *GRiwrmInputsModel* object)

## Usage

``` r
# S3 method for class 'GRiwrmInputsModel'
CreateRunOptions(x, IniStates = NULL, ...)

CreateRunOptions(x, ...)

# S3 method for class 'InputsModel'
CreateRunOptions(x, ...)

# S3 method for class 'character'
CreateRunOptions(x, InputsModel, ...)

# S3 method for class 'function'
CreateRunOptions(x, InputsModel, ...)
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

- IniStates:

  (optional) [numeric](https://rdrr.io/r/base/numeric.html) object or
  [list](https://rdrr.io/r/base/list.html) of
  [numeric](https://rdrr.io/r/base/numeric.html) object of class
  *IniStates*, see
  [airGR::CreateIniStates](https://rdrr.io/pkg/airGR/man/CreateIniStates.html)
  for details

- ...:

  arguments passed to
  [airGR::CreateRunOptions](https://rdrr.io/pkg/airGR/man/CreateRunOptions.html),
  see details

- InputsModel:

  object of class *InputsModel* (only used to be consistent with the
  original
  [airGR::CreateRunOptions](https://rdrr.io/pkg/airGR/man/CreateRunOptions.html)
  which has `FUN_MOD` as first parameter) see
  [airGR::CreateInputsModel](https://rdrr.io/pkg/airGR/man/CreateInputsModel.html)
  for details

## Value

Depending on the class of `InputsModel` argument (respectively
*InputsModel* and *GRiwrmInputsModel* object), the returned value is
respectively:

- a `RunOptions` object (See
  [airGR::CreateRunOptions](https://rdrr.io/pkg/airGR/man/CreateRunOptions.html))

- a `GRiwrmRunOptions` object which is a
  [list](https://rdrr.io/r/base/list.html) of `RunOptions` objects with
  one item per modeled sub-catchment

## Details

See
[airGR::CreateRunOptions](https://rdrr.io/pkg/airGR/man/CreateRunOptions.html)
documentation for a complete list of arguments.

If `x` argument is a *GRiwrmInputsModel* object, `IniStates` must be a
list of [numeric](https://rdrr.io/r/base/numeric.html) object of class
*IniStates* with one item per modeled sub-catchment.

With a *GRiwrmInputsModel* object, all arguments are applied on each
sub-catchments of the network.

For examples of use see topics
[RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md),
[RunModel_Reservoir](https://inrae.github.io/airGRiwrm/dev/reference/RunModel_Reservoir.md),
and
[RunModel.Supervisor](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md).

## See also

[`CreateGRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md),
[`CreateInputsModel.GRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md),
[`RunModel.GRiwrmInputsModel()`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md)
