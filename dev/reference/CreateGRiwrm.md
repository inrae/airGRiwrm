# Generation of a network description containing all hydraulic nodes and the description of their connections

Generation of a network description containing all hydraulic nodes and
the description of their connections

## Usage

``` r
CreateGRiwrm(
  db,
  cols = list(id = "id", down = "down", length = "length", area = "area", model =
    "model", donor = "donor"),
  keep_all = FALSE
)
```

## Arguments

- db:

  [data.frame](https://rdrr.io/r/base/data.frame.html) description of
  the network (See details)

- cols:

  [list](https://rdrr.io/r/base/list.html) or
  [vector](https://rdrr.io/r/base/vector.html) columns of `db`. By
  default, mandatory column names are: `id`, `down`, `length`, `area`
  and `model`. Other names can be handled with a named list or vector
  containing items defined as `"required name" = "column name in db"`
  (See details)

- keep_all:

  [logical](https://rdrr.io/r/base/logical.html) indicating if all
  columns of `db` should be kept or if only columns defined in `cols`
  should be kept

## Value

[data.frame](https://rdrr.io/r/base/data.frame.html) of class `GRiwrm`
describing the airGR semi-distributed model network, with each line
corresponding to a location on the river network and with the following
columns:

- `id` ([character](https://rdrr.io/r/base/character.html)): node
  identifier

- `down` ([character](https://rdrr.io/r/base/character.html)):
  identifier of the node downstream of the current node
  ([NA](https://rdrr.io/r/base/NA.html) for the most downstream node)

- `length` ([numeric](https://rdrr.io/r/base/numeric.html)): hydraulic
  distance to the downstream node in km
  ([NA](https://rdrr.io/r/base/NA.html) for the most downstream node)

- `area` ([numeric](https://rdrr.io/r/base/numeric.html)): total area of
  the basin starting from the current node location in km2

- `model` ([character](https://rdrr.io/r/base/character.html)):
  hydrological model to use ([NA](https://rdrr.io/r/base/NA.html) for
  using observed flow instead of a runoff model output)

- `donor` ([character](https://rdrr.io/r/base/character.html)): node
  used as model and calibration parameter "donor" for ungauged nodes.
  For other types of nodes, if the donor is different than the id, it
  indicates that the node is embedded in an ungauged node cluster.

## Details

`db` is a [data.frame](https://rdrr.io/r/base/data.frame.html) which at
least contains in its columns:

- a node identifier (column `id`),

- the identifier and the hydraulic distance to the downstream node
  ([character](https://rdrr.io/r/base/character.html) columns `down` and
  [numeric](https://rdrr.io/r/base/numeric.html) columns `length` in
  km). The last downstream node should have fields `down` and `length`
  set to `NA`,

- the total area of the basin at the node location
  ([numeric](https://rdrr.io/r/base/numeric.html) column `area` in km2).
  Direct injection node can have a null area defined by `NA`

- the model to use ([character](https://rdrr.io/r/base/character.html)
  column `model`), see section below for details

An optional column `donor` can be used to manually define which
sub-basin will give its parameters to an ungauged node (See `Ungauged`
model below).

### Available models in airGRiwrm

The "model" column should be filled by one of the following:

- One of the hydrological models available in the **airGR** package
  defined by its `RunModel` function (i.e.: `RunModel_GR4J`,
  `RunModel_GR5HCemaneige`...)

- `RunModel_Reservoir` for simulating a reservoir (See:
  [RunModel_Reservoir](https://inrae.github.io/airGRiwrm/dev/reference/RunModel_Reservoir.md))

- `Ungauged` for an ungauged node. The sub-basin inherits hydrological
  model and parameters from a "donor" sub-basin. If not defined by the
  user in the column `donor`, the donor is automatically set to the
  first gauged node at downstream. This set of sub-basins with the same
  donor downstream then forms an ungauged node cluster that will be
  calibrated at once.

- `NA` for injecting (or abstracting) a flow time series at the location
  of the node (direct flow injection)

- `Diversion` for abstracting a flow time series from an existing node
  transfer it to another node. As a `Diversion` is attached to an
  existing node, this node is then described with 2 lines: one for the
  hydrological model and another one for the diversion

## Examples

``` r
library(airGRiwrm)

#########################################
# Network of 2 nodes distant of 150 km: #
#########################################
# - an upstream reservoir modeled as a direct flow injection (no model)
# - a gauging station downstream a catchment of 360 km² modeled with GR4J
db <- data.frame(
  id = c("Reservoir", "GaugingDown"),
  length = c(150, NA),
  down = c("GaugingDown", NA),
  area = c(NA, 360),
  model = c(NA, "RunModel_GR4J"),
  stringsAsFactors = FALSE
)
griwrm_basic <- CreateGRiwrm(db)
griwrm_basic
#>            id        down length area         model       donor
#> 2 GaugingDown        <NA>     NA  360 RunModel_GR4J GaugingDown
#> 1   Reservoir GaugingDown    150   NA          <NA>        <NA>
# Network diagram with direct flow node in red, intermediate sub-basin in green
if (interactive()) {
  plot(griwrm_basic)
}

###################################################
# GR4J semi-distributed model of the Severn River #
###################################################
data(Severn)
nodes <- Severn$BasinsInfo
nodes$model <- "RunModel_GR4J"
str(nodes)
#> 'data.frame':    6 obs. of  13 variables:
#>  $ gauge_id           : chr  "54057" "54032" "54001" "54095" ...
#>  $ gauge_name         : chr  "Severn at Haw Bridge" "Severn at Saxons Lode" "Severn at Bewdley" "Severn at Buildwas" ...
#>  $ gauge_lat          : num  52 52 52.4 52.6 52.1 ...
#>  $ gauge_lon          : num  -2.23 -2.2 -2.32 -2.53 -1.94 -2.39
#>  $ area               : num  9885 6865 4330 3723 2208 ...
#>  $ elev_mean          : int  145 170 175 186 99 212
#>  $ station_type       : chr  "VA" "US" "US" "US" ...
#>  $ flow_period_start  : chr  "1971-07-01" "1970-10-01" "1970-10-01" "1984-03-01" ...
#>  $ flow_period_end    : chr  "2015-09-30" "2015-09-30" "2015-09-30" "2015-09-30" ...
#>  $ bankfull_flow      : num  460 340 420 285 125 190
#>  $ downstream_id      : chr  NA "54057" "54032" "54001" ...
#>  $ distance_downstream: num  NA 15 45 42 43 32
#>  $ model              : chr  "RunModel_GR4J" "RunModel_GR4J" "RunModel_GR4J" "RunModel_GR4J" ...
# Mismatch column names are renamed to stick with GRiwrm requirements
rename_columns <- list(
  id = "gauge_id",
  down = "downstream_id",
  length = "distance_downstream"
)
griwrm_severn <- CreateGRiwrm(nodes, rename_columns)
griwrm_severn
#>      id  down length    area         model donor
#> 4 54095 54001     42 3722.68 RunModel_GR4J 54095
#> 5 54002 54057     43 2207.95 RunModel_GR4J 54002
#> 6 54029 54032     32 1483.65 RunModel_GR4J 54029
#> 3 54001 54032     45 4329.90 RunModel_GR4J 54001
#> 2 54032 54057     15 6864.88 RunModel_GR4J 54032
#> 1 54057  <NA>     NA 9885.46 RunModel_GR4J 54057
# Network diagram with upstream basin nodes in blue, intermediate sub-basin in green
if (interactive()) {
  plot(griwrm_severn)
}

####################################################################
# Severn network with an ungauged station at nodes 54029 and 54001 #
####################################################################
nodes_ungauged <- nodes
nodes_ungauged$model[
  nodes_ungauged$gauge_id %in% c("54029", "54001")
] <- "Ungauged"
# By default the first gauged node at downstream is used for parameter calibration (54032)
# Add a `donor`column for defining manually an upstream or sibling donor
nodes_ungauged$donor <- as.character(NA)
nodes_ungauged$donor[nodes_ungauged$id == "54001"] <- "54095"
griwrm_ungauged <- CreateGRiwrm(nodes_ungauged, rename_columns)
#> Ungauged node '54001' automatically gets the node '54032' as parameter donor
#> Ungauged node '54029' automatically gets the node '54032' as parameter donor
griwrm_ungauged
#>      id  down length    area         model donor
#> 4 54095 54001     42 3722.68 RunModel_GR4J 54095
#> 5 54002 54057     43 2207.95 RunModel_GR4J 54002
#> 3 54001 54032     45 4329.90      Ungauged 54032
#> 6 54029 54032     32 1483.65      Ungauged 54032
#> 2 54032 54057     15 6864.88 RunModel_GR4J 54032
#> 1 54057  <NA>     NA 9885.46 RunModel_GR4J 54057
# Network diagram with gauged nodes of vivid color, and ungauged nodes of dull color
if (interactive()) {
  plot(griwrm_ungauged)
}

###########################################################
# Severn network with a Diversion on the node "54029"     #
# to a reservoir which transfer flows to the node "54001" #
# and a withdrawal on the reservoir                       #
###########################################################
nodes_div <- nodes[, c(
  "gauge_id",
  "downstream_id",
  "distance_downstream",
  "model",
  "area"
)]
nodes_div <- rbind(
  nodes_div,
  data.frame(
    gauge_id = c("54029", "Reservoir", "Irrigation_Pump"),
    downstream_id = c("Reservoir", "54001", "Reservoir"),
    distance_downstream = c(10, 5, 0),
    model = c("Diversion", "RunModel_Reservoir", NA),
    area = c(NA, NA, NA)
  )
)
griwrm_div <- CreateGRiwrm(nodes_div, rename_columns)
# Network diagram figures Diversion node by a red frame and a red arrow
if (interactive()) {
  plot(griwrm_div, orientation = "TB")
}

# It's also possible to custom the diagram's look with mermaid directives
# (See details in plot.GRiwrm help topic)
if (interactive()) {
  plot(
    griwrm_div,
    header = "%%{init: {'flowchart': {'nodeSpacing': 30, 'rankSpacing': 30, 'curve': 'linear'}}}%%"
  )
}
```
