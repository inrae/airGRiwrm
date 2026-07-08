# Get the next time steps date/time of a simulation

Get the next time steps date/time of a simulation

## Usage

``` r
getNextTimeSteps(x, TimeStep = 1L)
```

## Arguments

- x:

  Object returned by
  [RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md),
  [RunModel.Supervisor](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md),
  or
  [RunModel.GRiwrmOutputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmOutputsModel.md)

- TimeStep:

  [integer](https://rdrr.io/r/base/integer.html) number of time steps to
  get after the end of the simulation

## Value

A [`base::POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) vector
containing the date/time of the time steps following the end of the
simulation.
