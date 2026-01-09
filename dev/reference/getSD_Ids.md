# Function to get the IDs of sub-basins using SD model or not

Function to get the IDs of sub-basins using SD model or not

## Usage

``` r
getSD_Ids(InputsModel, add_diversions = FALSE)

getNoSD_Ids(InputsModel, include_diversion = TRUE)
```

## Arguments

- InputsModel:

  [GRiwrmInputsModel](https://inrae.github.io/airGRiwrm/dev/reference/CreateInputsModel.GRiwrm.md)
  object

- add_diversions:

  [logical](https://rdrr.io/r/base/logical.html) for adding upstream
  nodes due to diversion

- include_diversion:

  [logical](https://rdrr.io/r/base/logical.html) for including diversion
  nodes

## Value

[character](https://rdrr.io/r/base/character.html) IDs of the sub-basins
using SD model or not
