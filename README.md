# airGRiwrm: airGR based Integrated Water Resource Management R package

**airGRiwrm** is an extension of the **airGR** R package for managing semi-distributive hydrological model on an anthropized catchment.

This package is developed as part of the IN-WOP project (http://www.waterjpi.eu/joint-calls/joint-call-2018-waterworks-2017/booklet/in-wop) by the mixed research unit G-EAU (https://g-eau.fr) and the HYDRO team of the INRAE HYCAR research unit (https://www6.jouy.inrae.fr/hycar/Equipes-de-recherche/HYDRO).

## Installation

### Requirements

We need the package `remotes` to install the package from the Irstea Gitlab repository:

```r
install.packages("remotes")
```

This version is based on a development version of **airGR** which should be installed as below:

```r
remotes::install_gitlab("HYCAR-Hydro/airgr@5d52933b", host = "gitlab.irstea.fr")
```

### Local installation

The package **airGRiwrm** is under development and is only available on Gitlab:

```r
remotes::install_gitlab("in-wop/airGRiwrm", host = "gitlab.irstea.fr", dependencies = TRUE)
```

`dependencies = TRUE` triggers the installation of suggested packages used in the vignettes. Remove this argument if you don't need it.

## Get started

See the package vignettes for more information.
