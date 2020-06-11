# GR-IWRM: airGR based Integrated Water Resource Management R package

GR-IWRM is an extension of airGR for managing semi-distributive hydrological model on an anthropized catchment.

This package is developped as part of the IN-WOP project (http://www.waterjpi.eu/joint-calls/joint-call-2018-waterworks-2017/booklet/in-wop) by the mixed research unit G-EAU (https://g-eau.fr).

## Installation

Open a terminal and type: 

```shell
git clone git@gitlab-ssh.irstea.fr:in-wop/griwrm.git
cd griwrm
Rscript -e "install.packages("roxygen2");roxygen2::roxygenise()"
cd ..
R CMD INSTALL griwrm
```

## Get started

See the package vignettes.