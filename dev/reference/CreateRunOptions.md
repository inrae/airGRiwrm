# Creation of the RunOptions object

This function can be used either for a catchment (with an *InputsModel*
object) or for a network (with a *GRiwrmInputsModel* object)

## Usage

``` r
# S3 method for class 'GRiwrmInputsModel'
CreateRunOptions(x, IniStates = NULL, Outputs_Sim = MinimalOutputsSim, ...)

CreateRunOptions(x, ...)

# S3 method for class 'InputsModel'
CreateRunOptions(x, ...)

# S3 method for class 'character'
CreateRunOptions(x, InputsModel, ...)

# S3 method for class 'function'
CreateRunOptions(x, InputsModel, ...)

# S3 method for class 'RunModel_Reservoir'
CreateRunOptions(x, ...)
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

  (optional) object containing initial model states. Can be:

  - A single
    [`IniStates`](https://rdrr.io/pkg/airGR/man/CreateIniStates.html)
    object when calling CreateRunOptions.InputsModel

  - A named [list](https://rdrr.io/r/base/list.html) of
    [`IniStates`](https://rdrr.io/pkg/airGR/man/CreateIniStates.html)
    objects when calling CreateRunOptions.GRiwrmInputsModel, with names
    matching node IDs

  See
  [airGR::CreateIniStates](https://rdrr.io/pkg/airGR/man/CreateIniStates.html)
  for details

- Outputs_Sim:

  (optional) outputs to be returned by the model run. Can be:

  - `"all"` to return all available simulated outputs for each node
    (previous default behaviour, but not recommended for large networks)

  - A [character](https://rdrr.io/r/base/character.html) vector, applied
    to every node, to restrict the returned outputs (default:
    `c("DatesR", "WarmUpQsim", "Qsim", "Qsim_m3", "QsimDown", "Param", "StateEnd")`).

  - A named [list](https://rdrr.io/r/base/list.html) (names matching
    node IDs) to define outputs per node, each item being a
    [character](https://rdrr.io/r/base/character.html) vector.

  On a semi-distributed network, an explicit selection is completed with
  the minimal default output set so that the outputs required to
  propagate the simulated flows between nodes are available (#98).
  `Outputs_Sim = "all"` is not affected.

  See details.

  See
  [airGR::CreateRunOptions](https://rdrr.io/pkg/airGR/man/CreateRunOptions.html)
  for the list of output names supported by each rainfall-runoff model.

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

The default output selection has been reduced compared to previous
versions of the package (where it was `"all"`) in order to keep the size
of the resulting *GRiwrmOutputsModel* object reasonable for a network
with a large number of nodes (See issue inrae/airGRiwrm#141). The
default keeps only the outputs required to propagate simulated flows to
downstream nodes (`Qsim`, `Qsim_m3`), to warm-up semi-distributed nodes
correctly (`WarmUpQsim`), to carry the model parameters (`Param`) and
the model states at the end of the run (`StateEnd`) which are needed for
De Lavenne parameter regularization and hot restart. For CemaNeige
models, the `CemaNeigeLayers` output is added automatically: it carries
the CemaNeige layer output series required by SCA-based error criteria
(a reduced CemaNeige output set would change the model run in airGR).
`Precip` is also added automatically on CemaNeige models: the default
plot of a CemaNeige node requires it in the *OutputsModel* to compute
the precipitation panel axis as soon as the per-layer solid
precipitation (`Psol`) is present (upstream airGR `ylimP` limitation).
It is a recorded meteo input, not a Fortran CemaNeige output: the
CemaNeige `NOutputs` and the model run are unchanged. Use
`Outputs_Sim = "all"` to restore the previous behaviour.

On a semi-distributed network (a network with at least one
semi-distributed node, i.e. a node whose sub-catchment receives the
routed flows of upstream nodes), an explicit `Outputs_Sim` selection is
completed with the minimal default output set (the default selection,
See above, \#141). This is required because airGR computes the discharge
of each semi-distributed node by routing the upstream flows and
aggregating them with the local runoff: the node's own outputs are
passed to airGR's `RunModel_Lag` as `QcontribDown`, which requires in
particular `Qsim` and, when the warm-up period is non-zero,
`WarmUpQsim`. Without this completion, an explicit selection like
`Outputs_Sim = c("Qsim")` fails with the error "Time series WarmUpQsim
in 'QcontribDown' should have the same length as
'RunOptions\$IndPeriod_WarmUp'" (#98).

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
