#' Creation of the InputsCrit object required to the `ErrorCrit` functions
#'
#' This function can be used either for a catchment (with an \emph{InputsModel} object) or for a network (with a \emph{GRiwrmInputsModel} object)
#'
#' @param InputsModel object of class \emph{InputsModel} or \emph{GRiwrmInputsModel}. See [CreateInputsModel]
#' @param FUN_CRIT \[function (atomic or list)\] error criterion function (e.g. [airGR::ErrorCrit_RMSE], [airGR::ErrorCrit_NSE])
#' @param RunOptions object of class \emph{RunOptions} or \emph{GRiwrmRunOptions}, see [CreateRunOptions]
#' @param Obs [numeric], [matrix] or [data.frame] series of observed flows, see details
#' @param AprioriIds (optional) named [list] or named [vector] of [character] used for the parameter regularization (see details)
#' @param k (optional) [numeric] weight coefficient used in the parameter regularization (See [airGR::CreateInputsCrit_Lavenne])
#' @param AprCelerity (optional) [numeric] Default celerity used as a priori parameter for upstream catchments
#' @param ... arguments passed to [airGR::CreateInputsCrit], see details
#'
#' @details See [airGR::CreateInputsCrit] documentation for a complete list of arguments.
#'
#' `Obs` argument is equivalent to the same argument in [airGR::CreateInputsCrit] except that it must be a [matrix] or a [data.frame] if `InputsModel` is a \emph{GRiwrmInputsModel} object.
#' Then, each column of the [matrix] or [data.frame] represents the observations of one of the simulated node with the name of the columns representing the id of each node.
#'
#' With a \emph{GRiwrmInputsModel} object, all arguments are applied on each sub-catchments of the network.
#'
#' Parameter regularization consists of defining a priori parameters which are used in a composed criterion based on the formula proposed by Lavenne et al. (2019) (See [airGR::CreateInputsCrit_Lavenne]).
#' The parameter `AprioriIds` allows to define which upstream sub-catchment is used for providing a priori parameters. Its format is as follows: `AprioriIds <- c("Downstream sub-catchment 1" = "A priori upstream sub-catchment 1", ...)` where the quoted strings are the ids of the sub-catchments. See vignettes for more details. The parameter `AprCelerity` is a default value used as a priori for the parameter 'Celerity' in case of an upstream catchment (without celerity parameter) is used as a priori catchment.
#'
#' @return Depending on the class of `InputsModel` argument (respectively `InputsModel` and `GRiwrmInputsModel` object), the returned value is respectively:
#' - a `InputsCrit` object (See [airGR::CreateInputsCrit])
#' - a `GRiwrmInputsCrit` object which is a [list] of `InputsCrit` objects with one item per modeled sub-catchment
#'
#' @references De Lavenne, A., Andréassian, V., Thirel, G., Ramos, M.-H., Perrin, C., 2019. A Regularization Approach to Improve the Sequential Calibration of a Semidistributed Hydrological Model. Water Resources Research 55, 8821–8839. \doi{10.1029/2018WR024266}
#'
#' @rdname CreateInputsCrit
#' @export
CreateInputsCrit <- function(InputsModel, ...) {
  UseMethod("CreateInputsCrit", InputsModel)
}
