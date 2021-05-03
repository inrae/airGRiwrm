# airGRiwrm: airGR based Integrated Water Resource Management R package

**airGRiwrm** is an extension of the **airGR** R package for managing semi-distributive hydrological model on an anthropized catchment.

This package is developed as part of the IN-WOP project (http://www.waterjpi.eu/joint-calls/joint-call-2018-waterworks-2017/booklet/in-wop) by the mixed research unit G-EAU (https://g-eau.fr) and the HYDRO team of the INRAE HYCAR research unit (https://webgr.inrae.fr/en/home/).

## Installation

We need the package `remotes` to install the package from the Irstea Gitlab repository:

```r
install.packages("remotes")
```
The package **airGRiwrm** is under development and is only available on Gitlab:

```r
remotes::install_gitlab("in-wop/airGRiwrm", host = "gitlab.irstea.fr", dependencies = TRUE, build_vignettes = TRUE)
```

`dependencies = TRUE` and `build_vignettes = TRUE` are optional and respectively trigger the installation of suggested packages used in the vignettes and the compilation and the installation of the vignettes.

## Get started

Visit the website dedicated to the package at https://airgriwrm.g-eau.fr for tutorials, usage examples and documentation.
