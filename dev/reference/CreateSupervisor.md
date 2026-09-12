# Creation of a Supervisor for handling regulation in a model

Creation of a Supervisor for handling regulation in a model

## Usage

``` r
CreateSupervisor(InputsModel, TimeStep = 1L)
```

## Arguments

- InputsModel:

  \[object of type `GRiwrmInputsModel`\] inputs of the model

- TimeStep:

  [integer](https://rdrr.io/r/base/integer.html) number of time steps
  between each supervision

## Value

A `Supervisor` object which is an
[environment](https://rdrr.io/r/base/environment.html) containing all
the necessary variables to run a supervised simulation, such as:

- `DatesR` [base::POSIXct](https://rdrr.io/r/base/DateTimeClasses.html):
  vector of date from `InputsModel`

- `InputsModel`: a copy of `InputsModel` provided by
  [CreateInputsModel.GRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md)

- `griwrm`: a copy of `griwrm` provided by
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)

- `Controllers` [list](https://rdrr.io/r/base/list.html): list of the
  controllers used in the supervised simulation (See
  [CreateController](https://inrae.github.io/airGRiwrm/dev/reference/CreateController.md))

- `idx.output` [integer](https://rdrr.io/r/base/integer.html): index of
  the current time step output in the modeled series (updated during
  simulation)

- `idx.output_previous` [integer](https://rdrr.io/r/base/integer.html):
  index of the previous output time step

- `ts.date`
  [base::POSIXct](https://rdrr.io/r/base/DateTimeClasses.html):
  date/time of the current time step for controller calculations

- `ts.index0` [integer](https://rdrr.io/r/base/integer.html): index of
  the time step preceding the start of the simulation period

- `controller.id` [character](https://rdrr.io/r/base/character.html):
  identifier of the current controller being applied

## Details

See
[RunModel.Supervisor](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md)
and vignettes for examples of use.

## See also

[`RunModel.Supervisor()`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md),
[`CreateController()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateController.md)
