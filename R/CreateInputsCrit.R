#' Creation of the InputsCrit object required to the `ErrorCrit` functions
#'
#' This function can be used either for a catchment (with an \emph{InputsModel} object) or for a network (with a \emph{GRiwrmInputsModel} object)
#'
#' @param InputsModel object of class \emph{InputsModel} or \emph{GRiwrmInputsModel}. See [CreateInputsModel]
#' @param FUN_CRIT \[function (atomic or list)\] error criterion function (e.g. [airGR::ErrorCrit_RMSE], [airGR::ErrorCrit_NSE])
#' @param RunOptions object of class \emph{RunOptions} or \emph{GRiwrmRunOptions}, see [CreateRunOptions]
#' @param Obs [numeric], [matrix] or [data.frame] series of observed flows, see details
#' @param AprioriIds (optional) named [list] or named [vector] whose entries are [character] vectors (ids of a priori sub-catchments) or [numeric] vectors (user-defined a priori parameters), used for the parameter regularization (see details)
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
#' Parameter regularization consists of defining a priori parameters which are
#' used in a composed criterion based on the formula proposed by
#' Lavenne et al. (2019) (See [airGR::CreateInputsCrit_Lavenne]).
#' The parameter `AprioriIds` allows to define which neighbor sub-catchments are
#' used for providing a priori parameters.
#' Its format is as follows:
#' `AprioriIds <- list("Downstream sub-catchment 1" = c("A priori upstream sub-catchment 1", ...))`
#' where the quoted strings are the ids of the sub-catchments.
#' The nodes providing a priori parameters must be calibrated before the
#' current one.
#' The sequence order of calibration can be checked with [getNodeRanking].
#' If the latter is not adequate, this order can be forced by setting the node providing
#' a priori parameters as donor of the current node in [CreateGRiwrm].
#' See vignettes for more details.
#' The parameter `AprCelerity` is a default value used as a priori for the
#' parameter 'Celerity' in case of an upstream catchment (without celerity parameter)
#' is used as a priori catchment.
#' In the calibration process, all a priori parameter sets are tested and the one
#' getting the best `ErrorCrit` score is used for the parameter regularization.
#' By default, the immediate upstream catchments are used as a priori catchments,
#' as determined by [getDefaultAprioriIds].
#'
#' The entries of `AprioriIds` can also be [numeric] vectors of user-defined
#' a priori parameters. In that case, the given parameters are used as the a
#' priori parameters of the De Lavenne criterion of the named sub-catchment
#' instead of the parameters of an upstream sub-catchment:
#' `AprioriIds <- list("Sub-catchment 1" = c("Upstream sub-catchment 1"), "Sub-catchment 2" = c(1, 50, -1, 3, 2))`.
#' A user-defined a priori parameter must have one value per model parameter of
#' the sub-catchment (in the same order as the `Param` argument of
#' [airGR::RunModel]: e.g. `c(celerity, X1, X2, X3, X4)` for a GR4J sub-catchment
#' with celerity).
#' User-defined a priori parameters are used as-is: no default celerity is added
#' and no X4 rescaling is applied.
#'
#' @return Depending on the class of `InputsModel` argument (respectively `InputsModel` and `GRiwrmInputsModel` object), the returned value is respectively:
#' - a `InputsCrit` object (See [airGR::CreateInputsCrit])
#' - a `GRiwrmInputsCrit` object which is a [list] of `InputsCrit` objects with one item per modeled sub-catchment
#'
#' @references De Lavenne, A., Andréassian, V., Thirel, G., Ramos, M.-H., Perrin, C., 2019. A Regularization Approach to Improve the Sequential Calibration of a Semidistributed Hydrological Model. Water Resources Research 55, 8821–8839. \doi{10.1029/2018WR024266}
#'
#' @seealso [CreateGRiwrm()], [CreateInputsModel.GRiwrm()], [CreateRunOptions()], [CreateCalibOptions()], [Calibration()]
#' @rdname CreateInputsCrit
#' @export
#'
#' @examples
#' # Loading catchment data
#' data(Severn)
#'
#' # Creating catchment network
#' nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id",
#'                                "distance_downstream", "area")]
#' nodes$model <- "RunModel_GR4J"
#' rename_columns <- list(id = "gauge_id",
#'                        down = "downstream_id",
#'                        length = "distance_downstream")
#' griwrm <- CreateGRiwrm(nodes, rename_columns)
#'
#' # Preparation of InputsModel object
#' BasinsObs <- Severn$BasinsObs
#' DatesR <- BasinsObs[[1]]$DatesR
#' PrecipTot <- cbind(sapply(BasinsObs, function(x) {x$precipitation}))
#' PotEvapTot <- cbind(sapply(BasinsObs, function(x) {x$peti}))
#' Qobs <- cbind(sapply(BasinsObs, function(x) {x$discharge_spec}))
#' Precip <- ConvertMeteoSD(griwrm, PrecipTot)
#' PotEvap <- ConvertMeteoSD(griwrm, PotEvapTot)
#' InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap)
#'
#' # Calibration period selection
#' # Set aside warm-up period and use the rest for calibration
#' IndPeriod_Run <- seq(
#'   which(InputsModel[[1]]$DatesR ==
#'         (InputsModel[[1]]$DatesR[1] + 365 * 24 * 60 * 60)),
#'   length(InputsModel[[1]]$DatesR)
#' )
#' IndPeriod_WarmUp <- seq(1, IndPeriod_Run[1] - 1)
#'
#' # Preparation of RunOptions object
#' RunOptions <- CreateRunOptions(
#'   InputsModel,
#'   IndPeriod_WarmUp = IndPeriod_WarmUp,
#'   IndPeriod_Run = IndPeriod_Run
#' )
#'
#' # De Lavenne criterion with the default node-based a priori parameters
#' InputsCrit <- CreateInputsCrit(
#'   InputsModel = InputsModel,
#'   FUN_CRIT = ErrorCrit_KGE2,
#'   RunOptions = RunOptions,
#'   Obs = Qobs[IndPeriod_Run, ],
#'   transfo = "sqrt"
#' )
#'
#' # De Lavenne criterion with a user-defined a priori parameter for node
#' # "54057" instead of the parameters of an upstream sub-catchment:
#' # one value per model parameter of the node (e.g. for a GR4J sub-catchment
#' # with celerity: celerity, X1, X2, X3, X4)
#' InputsCrit <- CreateInputsCrit(
#'   InputsModel = InputsModel,
#'   FUN_CRIT = ErrorCrit_KGE2,
#'   RunOptions = RunOptions,
#'   Obs = Qobs[IndPeriod_Run, ],
#'   AprioriIds = list(
#'     "54057" = c(1, 50, -1, 3, 2),
#'     "54032" = "54001",
#'     "54001" = "54095"
#'   ),
#'   transfo = "sqrt"
#' )
#'
CreateInputsCrit <- function(InputsModel, ...) {
  UseMethod("CreateInputsCrit", InputsModel)
}
