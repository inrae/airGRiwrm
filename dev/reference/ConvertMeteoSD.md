# Conversion of meteorological data from basin scale to sub-basin scale

Conversion of meteorological data from basin scale to sub-basin scale

## Usage

``` r
ConvertMeteoSD(x, ...)

# S3 method for class 'GRiwrm'
ConvertMeteoSD(x, meteo, ...)

# S3 method for class 'character'
ConvertMeteoSD(x, griwrm, meteo, ...)

# S3 method for class 'matrix'
ConvertMeteoSD(x, areas, temperature = FALSE, ...)
```

## Arguments

- x:

  either a `GRiwrm` network description (See
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)),
  a [character](https://rdrr.io/r/base/character.html) id of a node, or
  a [matrix](https://rdrr.io/r/base/matrix.html) containing
  meteorological data

- ...:

  Parameters passed to the methods

- meteo:

  [matrix](https://rdrr.io/r/base/matrix.html) or
  [data.frame](https://rdrr.io/r/base/data.frame.html) containing
  meteorological data. Its
  [colnames](https://rdrr.io/r/base/colnames.html) should be equal to
  the ID of the basins

- griwrm:

  `GRiwrm` object describing the semi-distributed network (See
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md))

- areas:

  [numeric](https://rdrr.io/r/base/numeric.html) vector with the total
  area of the basin followed by the areas of the upstream basins in km2

- temperature:

  [logical](https://rdrr.io/r/base/logical.html) `TRUE` if the
  meteorological data contain air temperature. If `FALSE` minimum output
  values are bounded to zero

## Value

[matrix](https://rdrr.io/r/base/matrix.html) a matrix containing the
converted meteorological data

## See also

[`CreateGRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md),
[`CreateInputsModel.GRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md)
