# Reduce the size of a GRiwrm by selecting the subset of nodes corresponding to a downstream node

Reduce the size of a GRiwrm by selecting the subset of nodes
corresponding to a downstream node

## Usage

``` r
reduceGRiwrm(griwrm, down_node, check = FALSE)
```

## Arguments

- griwrm:

  A *GRiwrm* object (See
  [CreateGRiwrm](https://inrae.github.io/airGRiwrm/dev/reference/CreateGRiwrm.md))

- down_node:

  The ID of the downstream node of the reduced *GRiwrm*

- check:

  [logical](https://rdrr.io/r/base/logical.html) Check the consistency
  of the reduced *GRiwrm*

## Value

A *GRiwrm* object only containing nodes located upstream the given
downstream node

## Examples

``` r
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
rename_columns <- list(id = "gauge_id",
                       down = "downstream_id",
                       length = "distance_downstream")
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
plot(griwrm_severn)

plot(reduceGRiwrm(griwrm_severn, "54032"))

```
