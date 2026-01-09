# Plot of a `Qm3s` object (time series of simulated flows)

This function plot time series of flow rate in m3/s. It's a method for
object of class "Qm3s" which can be directly called by `plot`. It can
also be called as a function `plot.Qm3s` if the first parameter has the
good format.

## Usage

``` r
# S3 method for class 'Qm3s'
plot(
  x,
  type = "l",
  xlab = "Date",
  ylab = expression("Flow rate (m"^"3" * "/s)"),
  main = "Simulated flows",
  col = grDevices::hcl.colors(ncol(x) - 1, "Zissou 1"),
  legend = colnames(x)[-1],
  legend.cex = 0.7,
  legend.x = "topright",
  legend.y = NULL,
  lty = 1,
  mgp = c(2.5, 1, 0),
  ...
)
```

## Arguments

- x:

  [data.frame](https://rdrr.io/r/base/data.frame.html) with a first
  column with [POSIXt](https://rdrr.io/r/base/DateTimeClasses.html)
  dates and followings columns with flows at each node of the network

- type:

  [character](https://rdrr.io/r/base/character.html) plot type (See
  [plot.default](https://rdrr.io/r/graphics/plot.default.html)), default
  "l"

- xlab:

  [character](https://rdrr.io/r/base/character.html) label for the x
  axis, default to "Date"

- ylab:

  [character](https://rdrr.io/r/base/character.html) label for the y
  axis, default to "Flow (m3/s)"

- main:

  [character](https://rdrr.io/r/base/character.html) main title for the
  plot, default to "Simulated flows"

- col:

  [character](https://rdrr.io/r/base/character.html) plotting colors
  (See [par](https://rdrr.io/r/graphics/par.html))

- legend:

  [character](https://rdrr.io/r/base/character.html) see parameter
  `legend` of [legend](https://rdrr.io/r/graphics/legend.html). Set it
  to [NULL](https://rdrr.io/r/base/NULL.html) to hide the legend

- legend.cex:

  [character](https://rdrr.io/r/base/character.html) `cex` parameter for
  the text of the legend (See
  [par](https://rdrr.io/r/graphics/par.html))

- legend.x, legend.y:

  Legend position, see `x` and `y` parameters in
  [graphics::legend](https://rdrr.io/r/graphics/legend.html)

- lty:

  [character](https://rdrr.io/r/base/character.html) or
  [numeric](https://rdrr.io/r/base/numeric.html) The line type (See
  [par](https://rdrr.io/r/graphics/par.html))

- mgp:

  The margin line for the axis title, axis labels and axis line (See
  [par](https://rdrr.io/r/graphics/par.html))

- ...:

  Further arguments to pass to the
  [matplot](https://rdrr.io/r/graphics/matplot.html) functions

## Value

Screen plot window.

## Details

For examples of use see topics
[RunModel.GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.GRiwrmInputsModel.md),
[RunModel_Reservoir](https://inrae.github.io/airGRiwrm/dev/reference/RunModel_Reservoir.md),
and
[RunModel.Supervisor](https://inrae.github.io/airGRiwrm/dev/reference/RunModel.Supervisor.md).
