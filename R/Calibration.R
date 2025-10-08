#' Calibration of the parameters of one catchment or a network of sub-catchments
#'
#' Calibration algorithm that optimizes the error criterion selected as objective
#' function using the provided functions.
#'
#' This function can be used either for a catchment (with an \emph{InputsModel}
#' object), for a network (with a \emph{GRiwrmInputsModel} object), or for an
#' ungauged node cluster (with a \emph{Ungauged} object).
#'
#' @param InputsModel \[object of class \emph{InputsModel} or \emph{GRiwrmInputsModel}\] see [CreateInputsModel]
#' @param RunOptions \[object of class \emph{RunOptions} or \emph{GRiwrmRunOptions}\] see [CreateRunOptions]
#' @param InputsCrit \[object of class \emph{InputsCrit} or \emph{GRiwrmInputsCrit}\] see [CreateInputsCrit]
#' @param CalibOptions \[object of class \emph{CalibOptions} or \emph{GRiwrmCalibOptions}\] see [CreateCalibOptions] for details
#' @param ... further arguments passed to [airGR::Calibration], see details
#'
#' @details Argument classes should be consistent to the usage:
#' - a `InputsModel` argument of class \emph{InputsModel} must be followed by a
#' `RunOptions` argument of class \emph{RunOptions}, an `InputsCrit` argument of
#' class \emph{InputsCrit} and a `CalibOptions` of class \emph{CalibOptions}
#' - an `InputsModel` argument of class \emph{GRiwrmInputsModel} must be followed
#' by a `RunOptions` argument of class \emph{GRiwrmRunOptions}, an `InputsCrit`
#' argument of class \emph{GRiwrmInputsCrit} and a `CalibOptions` of class
#' \emph{GRiwrmCalibOptions}
#'
#' @return Depending on the class of `InputsModel` argument (respectively
#' `InputsModel` or `GRiwrmInputsModel` object), the returned value is respectively:
#' - an `OutputsCalib` object (See [airGR::Calibration] for more details on this object)
#' - a `GRiwrmOutputsCalib` object which is a [list] of `OutputsCalib` objects with
#' one item per modeled sub-catchment
#'
#' @rdname Calibration
#' @seealso [CreateGRiwrm()], [CreateInputsModel.GRiwrm()], [CreateInputsCrit()], [CreateCalibOptions()]
#' @export
#'
#' @examples
#' # Loading catchment data
#' data(Severn)
#'
#' #############################################################################
#' # EXAMPLE 1 - Basic calibration workflow                                   #
#' #############################################################################
#'
#' # Creating catchment network
#' nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id",
#'                                "distance_downstream", "area")]
#' nodes$model <- "RunModel_GR4J"
#' rename_columns <- list(id = "gauge_id",
#'                        down = "downstream_id",
#'                        length = "distance_downstream")
#' griwrm <- CreateGRiwrm(nodes, rename_columns)
#' griwrm
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
#' # Calibration criterion: preparation of the InputsCrit object
#' InputsCrit <- CreateInputsCrit(
#'   InputsModel = InputsModel,
#'   FUN_CRIT = ErrorCrit_KGE2,
#'   RunOptions = RunOptions,
#'   Obs = Qobs[IndPeriod_Run, ],
#'   transfo = "sqrt"
#' )
#'
#' # Preparation of CalibOptions object
#' CalibOptions <- CreateCalibOptions(InputsModel)
#'
#' if (interactive()) { # Too long for CRAN check...
#'   # Calibration
#'   OutputsCalib <- suppressWarnings(
#'     Calibration(InputsModel, RunOptions, InputsCrit, CalibOptions)
#'   )
#'
#'   # Simulation
#'   OutputsModels <- RunModel(
#'     InputsModel,
#'     RunOptions = RunOptions,
#'     Param = extractParam(OutputsCalib)
#'   )
#' }
#'
Calibration <- function(InputsModel, ...) {
  UseMethod("Calibration", InputsModel)
}

#' @rdname Calibration
#' @noRd
Calibration.Ungauged <- function(InputsModel, ...) {
  InputsModel$FUN_MOD <- "RunModel_Ungauged"
  NextMethod()
}
