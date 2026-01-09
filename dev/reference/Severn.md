# Catchment attributes and hydro-meteorological timeseries for some gauging stations on the Severn River

Catchment attributes and hydro-meteorological timeseries for some
gauging stations on the Severn River

## Usage

``` r
Severn
```

## Format

a [list](https://rdrr.io/r/base/list.html) with 2 items:

- "BasinsInfo" which contains a
  [data.frame](https://rdrr.io/r/base/data.frame.html) with Gauging
  station identifier, name, coordinates (GPS), area (km2), mean
  elevation (m), station type, flow period start and end, the bank full
  flow (m3/s), the identifier of the following downstream station and
  the distance to the following downstream station

- "BasinObs" which contains a [list](https://rdrr.io/r/base/list.html)
  with an item by gauging station which contains a
  [data.frame](https://rdrr.io/r/base/data.frame.html) with
  lubridate::POSIXct dates, precipitations (mm/time step), potential
  evapotranspiration (mm/time step) and measured flows (mm/time step)

## Source

These data are extracted from the CAMEL-GB dataset.

Coxon, G.; Addor, N.; Bloomfield, J.P.; Freer, J.; Fry, M.; Hannaford,
J.; Howden, N.J.K.; Lane, R.; Lewis, M.; Robinson, E.L.; Wagener, T.;
Woods, R. (2020). Catchment attributes and hydro-meteorological
timeseries for 671 catchments across Great Britain (CAMELS-GB). NERC
Environmental Information Data Centre. (Dataset).
[doi:10.5285/8344E4F3-D2EA-44F5-8AFA-86D2987543A9](https://doi.org/10.5285/8344E4F3-D2EA-44F5-8AFA-86D2987543A9)
