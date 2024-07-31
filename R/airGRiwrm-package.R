#' airGR based Integrated Water Resource Management Modelling
#'
#' The R-package **airGRiwrm** is an extension of the **airGR** R-package
#' for semi-distributed hydrological models.
#' It keeps the same operating mode and functions as **airGR**.
#'
#' @section Main features:
#'
#' The R-package **airGRiwrm** proposes a simplified network description for building semi-distributed
#' models containing several sub-basins with diverse connections including inter-basin
#' transfers.
#' It allows to automatically transfer hydrological model parameters for ungauged
#' locations in the catchment.
#' Local human influences such as water transfers, releases and withdrawalad can
#' be injected in the network.
#' The model also handles reservoirs by simulating volume time series from releases
#' time series provided by the user.
#' The package also integrates a supervisor that apply user-defined decision algorithms
#' given model outputs during simulation (centralized feedback control).
#'
#' @references
#'
#' - Thirel, G., Dorchies, D., Delaigue, O., Nunez Torres, L., Elmalki, D., 2022.
#' Évaluation de l’impact du changement climatique et de l’adaptation avec des
#' outils de modélisation hydrologiques libres.
#' Presented at the 35e colloque annuel de l’Association Internationale de Climatologie.
#' - Dorchies, D., Delaigue, O., Thirel, G., 2021. airGRiwrm: an extension of the
#' airGR R-package for handling Integrated Water Resources Management modeling.
#' Presented at the EGU General Assembly 2021 - vEGU21: Gather Online, p. EGU21.
#' https://doi.org/10.5194/egusphere-egu21-2190
#'
#' - de Lavenne, A., Andréassian, V., Thirel, G., Ramos, M.-H., Perrin, C., 2019.
#' A regularization approach to improve the sequential calibration of a semidistributed
#' hydrological model. Water Resources Research 55, 8821–8839. https://doi.org/10.1029/2018WR024266
#' - Lobligeois, F., Andréassian, V., Perrin, C., Tabary, P., Loumagne, C., 2014.
#' When does higher spatial resolution rainfall information improve streamflow
#' simulation? An evaluation using 3620 flood events. Hydrology and Earth System
#' Sciences 18, 575–594. https://doi.org/10.5194/hess-18-575-2014
#'
#' @section References of works using airGRiwrm:
#'
#' - Lemaitre-Basset, T., Thirel, G., Oudin, L., Dorchies, D., 2024.
#' Water use scenarios versus climate change: Investigating future water
#' management of the French part of the Moselle.
#' Journal of Hydrology: Regional Studies 54, 101855. https://doi.org/10.1016/j.ejrh.2024.101855
#' - Dorchies, D., Delaigue, O., Kahiyeh-Moumin, I., Ricquier, F., Thirel, G., 2023.
#' Risk-based flood and drought management for multiple reservoirs in a
#' non-stationary climate: application to the Seine River (No. EGU23-8697).
#' Presented at the EGU23, Copernicus Meetings. https://doi.org/10.5194/egusphere-egu23-8697
#' - Soutif-Bellenger, M., Thirel, G., Fernandez, S., Dorchies, D., 2023.
#' Build and evaluate climate change adaptation with a parsimonious integrated
#' agro-hydrological model over a catchment in northeastern France (No. EGU23-12160).
#' Presented at the EGU23, Copernicus Meetings. https://doi.org/10.5194/egusphere-egu23-12160
#' - Soutif-Bellenger, M., Thirel, G., Therond, O., Villerd, J., 2022.
#' How much can we simplify irrigation in an integrated modelling purpose?
#' A case study in southern France (No. IAHS2022-32). Presented at the IAHS2022,
#' Copernicus Meetings. https://meetingorganizer.copernicus.org/IAHS2022/IAHS2022-32.html.
#' - Santos, L., Thirel, G., Perrin, C., 2022. Impact of water withdrawals and
#' releases on the parameters of a bucket-type rainfall-runoff model (No. IAHS2022-421).
#' Presented at the IAHS2022, Copernicus Meetings.
#' - Nunez Torres, L., 2021. Simulation d’un bassin versant anthropisé à l’aide
#' d’un modèle hydrologique semi-distribué: Le bassin de la Seine et ses réservoirs.
#' Rapport de stage ST5 -Polytech Sorbonne -1er septembre 2021 (other). PolyTech Sorbonne.
#' https://hal.inrae.fr/hal-03359617
#' Dau, Q., Dorchies, D., Bader, J.-C., 2021. Many-objective risk assessment
#' framework for guiding operational decisions on multiple reservoirs.
#' Presented at the EGU General Assembly 2021 - vEGU21: Gather Online.
#' https://doi.org/10.5194/egusphere-egu21-10676
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
