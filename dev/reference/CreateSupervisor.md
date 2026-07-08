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

- some internal state variables updated during simulation (`ts.index`,
  `ts.previous`, `ts.date`, `ts.index0`, `controller.id`)

## Details

See
[RunModel.Supervisor](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md)
and vignettes for examples of use.

## See also

[`RunModel.Supervisor()`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md),
[`CreateController()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateController.md)
