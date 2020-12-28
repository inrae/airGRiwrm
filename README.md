# airGRiwrm: airGR based Integrated Water Resource Management R package

**airGRiwrm** is an extension of the **airGR** R package for managing semi-distributive hydrological model on an anthropized catchment.

This package is developed as part of the IN-WOP project (http://www.waterjpi.eu/joint-calls/joint-call-2018-waterworks-2017/booklet/in-wop) by the mixed research unit G-EAU (https://g-eau.fr) and the HYDRO team of the INRAE HYCAR research unit (https://www6.jouy.inrae.fr/hycar/Equipes-de-recherche/HYDRO).

## Installation

### Requirements

This package depends on R packages installed on CRAN. To install these package use the following instruction in a R console:

```r
install.packages(c("remotes", "dplyr", "roxygen2"))
```

This package also depends on **airGR** version 1.6 and above which is currently under development. To install it, use the following instruction in R console:

```r
remotes::install_gitlab("HYCAR-Hydro/airgr@dev", host = "gitlab.irstea.fr")
```

To run the tutorial vignettes, some other dependencies should also be installed:

```r
install.packages(c("knitr", "rmarkdown", "DiagrammeR", "testthat", "htmltools"))
```

Note: the 'DiagrammeR' package is necessary for running `DiagramGRiwrm` function. This last can also be  useful outside the vignettes.

### Local installation

Installation of the package from source should be done in four steps:

- download sources
- run `roxygen` for generating `NAMESPACE` file and documentation from sources
- install the package

Open a terminal and type:

```shell
git clone https://gitlab.irstea.fr/in-wop/airGRiwrm.git
cd airGRiwrm
Rscript -e "roxygen2::roxygenise()"
cd ..
R CMD INSTALL airGRiwrm
```

## Get started

See the package vignettes for more information.
