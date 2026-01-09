# Properties of GRiwrm nodes

`getNodeProperties` returns properties of a single node, and

## Usage

``` r
getNodeProperties(id, griwrm)

getAllNodesProperties(griwrm)
```

## Arguments

- id:

  [character](https://rdrr.io/r/base/character.html) Id of the node in
  the GRiwrm object

- griwrm:

  \[GRiwrm object\] describing the network of the semi-distributed model
  (See
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md))

## Value

`getNodeProperties` returns a [list](https://rdrr.io/r/base/list.html)
with the following items:

- "position" ([character](https://rdrr.io/r/base/character.html)):
  Position of the node in the network ("Upstream" or "Intermediate")

- "DirectInjection" ([logical](https://rdrr.io/r/base/logical.html)): is
  the node a Direct Injection node?

- "Diversion" ([logical](https://rdrr.io/r/base/logical.html)): is the
  node a Diversion node?

- "Reservoir" ([logical](https://rdrr.io/r/base/logical.html)): is the
  node a Reservoir?

- "airGR" ([logical](https://rdrr.io/r/base/logical.html)): is the node
  contains an airGR model?

- "calibration" ([character](https://rdrr.io/r/base/character.html)):
  describe if the node is a "Gauged", or an "Ungauged" station, (see
  details), or "NA" otherwise

- "Upstream" ([logical](https://rdrr.io/r/base/logical.html)): is the
  node an upstream node?

- "RunOff" ([logical](https://rdrr.io/r/base/logical.html)): is the node
  contains an hydrological model?

`getAllNodesProperties` returns a
[data.frame](https://rdrr.io/r/base/data.frame.html) constituted from
the list returned by `getNodeProperties` for all nodes.

## Details

A "Gauged" node is either a node containing a model that is already
calibrated (parameters are already fixed) or a node containing a model
where observations are available for calibration.

A "Ungauged" node is a node containing a model which derives its
parameters from another "donor" node.

## See also

[`CreateGRiwrm()`](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md)

## Examples

``` r
###############################################################################
# Severn network with :                                                       #
# - a Diversion on the node "54001" which transfer flows to the node "540029" #
# - node 54002 as a Direct Injection node                                     #
###############################################################################
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
nodes <- nodes[, c(
  "gauge_id",
  "downstream_id",
  "distance_downstream",
  "model",
  "area"
)]
# Add a Diversion node from node "54001" to "54029"
nodes <- rbind(
  nodes,
  data.frame(
    gauge_id = "54001",
    downstream_id = "54029",
    distance_downstream = 20,
    model = "Diversion",
    area = NA
  )
)
# Set node '54002' as a Direct Injection node
nodes$model[nodes$id == "54002"] <- NA
# Mismatch column names are renamed to stick with GRiwrm requirements
rename_columns <- list(
  id = "gauge_id",
  down = "downstream_id",
  length = "distance_downstream"
)
# Create GRiwrm object and display properties
griwrm <- CreateGRiwrm(nodes, rename_columns)

str(getNodeProperties("54001", griwrm))
#> List of 9
#>  $ position       : chr "Intermediate"
#>  $ DirectInjection: logi FALSE
#>  $ Diversion      : logi TRUE
#>  $ Reservoir      : logi FALSE
#>  $ airGR          : logi TRUE
#>  $ gauged         : logi TRUE
#>  $ calibration    : chr "Gauged"
#>  $ Upstream       : logi FALSE
#>  $ RunOff         : logi TRUE

getAllNodesProperties(griwrm)
#>          id     position DirectInjection Diversion Reservoir airGR gauged
#> 54095 54095     Upstream           FALSE     FALSE     FALSE  TRUE   TRUE
#> 54002 54002     Upstream           FALSE     FALSE     FALSE  TRUE   TRUE
#> 54001 54001 Intermediate           FALSE      TRUE     FALSE  TRUE   TRUE
#> 54029 54029 Intermediate           FALSE     FALSE     FALSE  TRUE   TRUE
#> 54032 54032 Intermediate           FALSE     FALSE     FALSE  TRUE   TRUE
#> 54057 54057 Intermediate           FALSE     FALSE     FALSE  TRUE   TRUE
#>       calibration Upstream RunOff
#> 54095      Gauged     TRUE   TRUE
#> 54002      Gauged     TRUE   TRUE
#> 54001      Gauged    FALSE   TRUE
#> 54029      Gauged    FALSE   TRUE
#> 54032      Gauged    FALSE   TRUE
#> 54057      Gauged    FALSE   TRUE
```
