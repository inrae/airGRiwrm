
<!-- README.md is generated from README.Rmd. Please edit that file -->

# airGRiwrm <img src="man/figures/logo.png" align="right" width="20%"/>

> **airGR-based Integrated Water Resource Management Modeling**

<!-- badges: start -->

[![CRAN
version](https://www.r-pkg.org/badges/version-ago/airGRiwrm)](https://cran.r-project.org/package=airGRiwrm)
[![Total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/airGRiwrm)](https://cran.r-project.org/package=airGRiwrm)
[![License:
AGPL-3](https://img.shields.io/badge/license-AGPL--3-orange.svg)](https://cran.r-project.org/web/licenses/AGPL-3)
[![DOI:10.32614/CRAN.package.airGRiwrm](https://img.shields.io/badge/doi-10.32614/CRAN.package.airGRiwrm-purple)](https://doi.org/10.32614/CRAN.package.airGRiwrm)
[![Dev
pipeline](https://gitlab.irstea.fr/in-wop/airGRiwrm/badges/dev/pipeline.svg)](https://gitlab.irstea.fr/in-wop/airGRiwrm/-/pipelines)
<!-- badges: end -->

**airGRiwrm** is an R package extending
[**airGR**](https://hydrogr.github.io/airGR/) to support
**semi-distributed hydrological modeling** for **anthropized
catchments**.

This package is developed by:

- [UMR G-EAU, Montpellier, France](https://g-eau.fr/index.php/en/)
- [INRAE HYCAR – HYDRO team, Antony, France](https://webgr.inrae.fr/eng)

## 🔧 Installation

``` r
# Stable version from CRAN
install.packages("airGRiwrm")

# Development version from R-universe
install.packages(
   'airGRiwrm',
   repos = c('https://inrae.r-universe.dev', 'https://cloud.r-project.org')
)

# Development version from github
# install.packages("remotes")
remotes::install_github("inrae/airGRiwrm", ref = "dev")
```

## 🚀 Getting Started

Visit the package website: 👉 <https://inrae.github.io/airGRiwrm/>

There you’ll find: - Tutorials - Usage examples - Full documentation

📬 [Subscribe to the airGRiwrm mailing
list](https://groupes.renater.fr/sympa/subscribe/airgriwrm-users) To
stay informed, ask questions, and connect with other users.

## 💡 Use Cases

**airGRiwrm** can be used in various contexts:

1.  **Water Resource Management** Model and manage water systems in
    human-impacted catchments.

2.  **Research & Academia** Study anthropogenic impacts on hydrological
    behavior.

3.  **Environmental Impact Assessment** Evaluate effects of
    infrastructure or policy on water flows.

4.  **Policy Support** Inform evidence-based decision-making with robust
    simulations.

5.  **Education** Teach distributed hydrology and human-nature system
    modeling.

## 🤝 Acknowledgements

This package was developed within:

- [**IN-WOP project**
  (2019–2023)](http://www.waterjpi.eu/joint-calls/joint-call-2018-waterworks-2017/booklet/in-wop)

- [**Talanoa-Water project**
  (2021–2025)](https://talanoa-water-france.hub.inrae.fr/) Part of the
  **PRIMA Programme**, supported under **Horizon 2020** (Grant No. 2023)

**Funding agencies:** - European Commission - French National Research
Agency (ANR) - Water JPI via WaterWorks2017

<p align="center">

<img src="man/figures/logo_water_jpi.png" height="80"/>
<img src="man/figures/logo_water_works_2017.png" height="80"/>
<img src="man/figures/logo_european_commission.jpg" height="80"/>
<img src="man/figures/logo_2018_joint_call.png" height="80"/>
</p>

<p align="center">

<img src="https://upload.wikimedia.org/wikipedia/commons/2/29/Horizon_2020_Logo.png" width="25%"/>
<img src="man/figures/logo_prima.png" width="25%"/>
</p>
