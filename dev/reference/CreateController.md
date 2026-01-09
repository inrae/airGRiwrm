# Creation and adding of a controller in a supervisor

Creation and adding of a controller in a supervisor

## Usage

``` r
CreateController(supervisor, ctrl.id, Y, U, FUN)
```

## Arguments

- supervisor:

  `Supervisor` object, see
  [CreateSupervisor](https://inrae.github.io/airGRiwrm/dev/reference/CreateSupervisor.md)

- ctrl.id:

  [character](https://rdrr.io/r/base/character.html) id of the
  controller (see Details)

- Y:

  [character](https://rdrr.io/r/base/character.html) location of the
  controlled and/or measured variables in the model.

- U:

  [character](https://rdrr.io/r/base/character.html) location of the
  command variables in the model.

- FUN:

  [function](https://rdrr.io/r/base/function.html) controller logic
  which calculates `U` from `Y` (see Details)

## Value

a `Controller` object which is a list with the following items:

- `id` [character](https://rdrr.io/r/base/character.html): the
  controller identifier

- `U` [matrix](https://rdrr.io/r/base/matrix.html): the list of controls
  for command variables with each column being the location of the
  variables and the rows being the values of the variable for the
  current time steps (empty by default)

- `Unames` [character](https://rdrr.io/r/base/character.html): location
  of the command variables

- `Y` [matrix](https://rdrr.io/r/base/matrix.html): the lists of
  controls for controlled variables with each column being the location
  of the variables and the rows being the values of the variable for the
  current time steps (empty by default)

- `Ynames` [character](https://rdrr.io/r/base/character.html): location
  of the controlled variables

- `FUN` [function](https://rdrr.io/r/base/function.html): controller
  logic which calculates `U` from `Y`

## Details

The `ctrl.id` is a unique id for finding the controller in the
supervisor. If a controller with the same id already exists, it is
overwritten by this new one.

`FUN` should be a function with one
[numeric](https://rdrr.io/r/base/numeric.html) parameter. This parameter
will receive the measured values of at `Y` locations as input for the
previous time step and returns calculated `U`. These `U` will then be
applied at their location for the current time step of calculation of
the model.

See
[RunModel.Supervisor](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md)
and vignettes for examples of use.

## See also

[`RunModel.Supervisor()`](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md),
[`CreateSupervisor()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateSupervisor.md)
