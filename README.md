# airGRiwrm: airGR based Integrated Water Resource Management R package <img src="man/figures/logo.png" align="right" height="80px"/>

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

## Acknowledgement

The authors would like to thank the European Commission and the French National Research Agency (ANR) for funding in the frame of the collaborative  international consortium [IN-WOP](http://www.waterjpi.eu/joint-calls/joint-call-2018-waterworks-2017/booklet/in-wop) financed under the 2018 Joint call of the WaterWorks2017 ERA-NET Cofund. This ERA-NET is an integral part of the activities developed by the Water JPI.

<div style="display: flex; justify-content: space-between;">![Water JPI](man/figures/logo_water_jpi.png) ![Water Works 2017](man/figures/logo_water_works_2017.png) ![European Commission](man/figures/logo_european_commission.jpg) ![2018 Joint call](man/figures/logo_2018_joint_call.png)</div>