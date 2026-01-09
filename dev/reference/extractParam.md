# Extract calibrated parameters

Extract [list](https://rdrr.io/r/base/list.html) of parameters from the
output of
[Calibration.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/Calibration.md)
which can be directly used as argument `Param` of
[RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md),
[RunModel.Supervisor](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md),
and
[RunModel.GRiwrmOutputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmOutputsModel.md).

## Usage

``` r
extractParam(x)

# S3 method for class 'GRiwrmOutputsCalib'
extractParam(x)

# S3 method for class 'GRiwrmOutputsModel'
extractParam(x)
```

## Arguments

- x:

  Object of class
  [GRiwrmOutputsModel](https://inrae.github.io/airGRiwrm/dev/reference/Calibration.md)
  returned by
  [Calibration.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/Calibration.md)

## Value

A [list](https://rdrr.io/r/base/list.html) containing representing the
calibrated parameters of each modeled node as a
[numeric](https://rdrr.io/r/base/numeric.html)
[vector](https://rdrr.io/r/base/vector.html).

## Details

See vignettes and example of
[RunModel_Reservoir](https://inrae.github.io/airGRiwrm/dev/reference/RunModel_Reservoir.md)
for examples of use.

## See also

[Calibration](https://inrae.github.io/airGRiwrm/dev/reference/Calibration.md),
[RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md),
[RunModel.Supervisor](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md)
