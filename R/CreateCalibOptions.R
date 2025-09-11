#' Creation of the CalibOptions object
#'
#' This function can be used either for a single catchment (with an
#' \emph{InputsModel} object) or for a network of catchments (with a
#' \emph{GRiwrmInputsModel} object)
#'
#' @template param_x
#' @param FixedParam (optional) [numeric] vector or list of vectors giving the
#' values set for the non-optimised parameter values, as for
#' [airGR::CreateCalibOptions] (see details)
#' @param ... arguments passed to [airGR::CreateCalibOptions], see details
#'
#' @details See [airGR::CreateCalibOptions] documentation for a complete list of arguments.
#'
#' With a \emph{GRiwrmInputsModel} object, all arguments are applied on each
#' sub-catchment of the network with some adaptation depending on the model used
#' on each node.
#'
#' If the argument `FixedParam` is a [numeric] [vector], it is applied to each
#' node of the network.
#' Beware that parameters must be adapted depending on the use of the routing model
#' and of the CemaNeige model on each node.
#' If `FixedParam` is a [list] of [numeric] vectors, each item of the list will
#' be applied on corresponding nodes. Use the id "*" for applying a setting on
#' the remaining nodes. Example for applying one setting for all the nodes
#' except the id "54057":
#'
#' ```
#' FixedParam <- list(`*` = c(NA, NA, NA, NA, NA, 0.25, NA, 10, NA),
#'                    `54057` = c(0.5, NA, NA, NA, NA, 0.25, NA, 10, NA))
#' ```
#'
#' The argument `IsHyst` is ignored since it should be defined previously with
#' [CreateInputsModel.GRiwrm].
#'
#' Beware, if you transfer a parameter set from an upstream catchment to a
#' downstream catchment, you must prescribe the celerity parameter (see example).
#'
#' See [transferGRparams] for more details about how model parameters are
#' transferred to ungauged nodes.
#'
#' @return Depending on the class of the `InputsModel` argument (respectively
#' `InputsModel` or `GRiwrmInputsModel` object), the returned value is
#' respectively:
#' - a `CalibOptions` object (See [airGR::CreateCalibOptions])
#' - a `GRiwrmCalibOptions` object which is a [list] of `CalibOptions` objects
#' with one item per modeled sub-catchment
#'
#' @seealso [CreateGRiwrm()], [CreateInputsModel.GRiwrm()], [CreateRunOptions()], [CreateInputsCrit()], [Calibration()]
#' @rdname CreateCalibOptions
#' @export
#'
#' @examples
#' # Loading catchment data
#' data(Severn)
#'
#' #############################################################################
#' # EXAMPLE 1 - The one where all nodes are calibrated                        #
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
#' # Calibration
#' OutputsCalib <- suppressWarnings(
#'   Calibration(InputsModel, RunOptions, InputsCrit, CalibOptions)
#' )
#'
#' # Simulation
#' OutputsModels <- RunModel(
#'   InputsModel,
#'   RunOptions = RunOptions,
#'   Param = extractParam(OutputsCalib)
#' )
#'
#' #############################################################################
#' # EXAMPLE 2 - The one where the node "54032" is ungauged                    #
#' #############################################################################
#'
#' # Creating catchment network with ungauged node
#' nodes$model[nodes$gauge_id == "54032"] <- "Ungauged"
#' griwrm <- CreateGRiwrm(nodes, rename_columns)
#'
#' # Preparation of InputsModel object
#' InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap)
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
#' # Calibration
#' OutputsCalib <- suppressWarnings(
#'   Calibration(InputsModel, RunOptions, InputsCrit, CalibOptions)
#' )
#'
#' # Simulation
#' OutputsModels <- RunModel(
#'   InputsModel,
#'   RunOptions = RunOptions,
#'   Param = extractParam(OutputsCalib)
#' )
#'
#' #############################################################################
#' # EXAMPLE 3 - Parameter transfer from donor catchment "54001"                #
#' # In this case, we want the "54032" node to be ungauged, and the "54001"     #
#' # node to give its parameters                                             #
#' #############################################################################
#'
#' # Creating catchment network with parameter transfer
#' nodes$model[nodes$gauge_id == "54032"] <- "Ungauged"
#' nodes$donor <- as.character(NA)
#' nodes$donor[nodes$gauge_id == "54032"] <- "54001"
#' griwrm <- CreateGRiwrm(nodes, rename_columns)
#'
#' # Preparation of InputsModel object
#' InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap)
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
#' # Calibration
#' OutputsCalib <- suppressWarnings(
#'   Calibration(InputsModel, RunOptions, InputsCrit, CalibOptions)
#' )
#'
#' # Simulation
#' OutputsModels <- RunModel(
#'   InputsModel,
#'   RunOptions = RunOptions,
#'   Param = extractParam(OutputsCalib)
#' )
#'
#' #############################################################################
#' # EXAMPLE 4 - Parameter transfer with prescribed celerity parameter         #
#' # In this case, we want the "54032" node to be ungauged, and the "54029"     #
#' # node to give its parameters. Beware, the "54029" node has no celerity     #
#' # parameters, as it is an upstream catchment, but "54032" needs one         #
#' #############################################################################
#'
#' # Creating catchment network with parameter transfer and celerity
#' nodes$model[nodes$gauge_id == "54032"] <- "Ungauged"
#' nodes$donor <- as.character(NA)
#' nodes$donor[nodes$gauge_id == "54032"] <- "54029"
#' griwrm <- CreateGRiwrm(nodes, rename_columns)
#'
#' # Preparation of InputsModel object
#' InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap)
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
#' # Preparation of CalibOptions object with prescribed celerity
#' # Here you need to prescribe the celerity parameter for node "54032"
#' CalibOptions <- CreateCalibOptions(
#'   InputsModel,
#'   FixedParam = list("54032" = c(1, NA, NA, NA, NA))
#' )
#'
#' # Calibration
#' OutputsCalib <- suppressWarnings(
#'   Calibration(InputsModel, RunOptions, InputsCrit, CalibOptions)
#' )
#'
#' # Simulation
#' OutputsModels <- RunModel(
#'   InputsModel,
#'   RunOptions = RunOptions,
#'   Param = extractParam(OutputsCalib)
#' )
#'
CreateCalibOptions <- function(x, FixedParam = NULL, ...) {
  UseMethod("CreateCalibOptions", x)
}

#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.InputsModel <- function(x, FixedParam = NULL, ...) {
  dots <- list(...)
  # Add FUN_MOD in parameters if carried by InputsModel
  if (!"FUN_MOD" %in% names(dots)) {
    if (!is.null(x$FUN_MOD)) {
      dots$FUN_MOD <- x$FUN_MOD
    } else {
      stop(" The parameter `FUN_MOD` must be defined")
    }
  }
  # Add FixedParam
  dots$FixedParam <- FixedParam
  # Automatically define IsSD for intermediate basin GR models
  dots$IsSD = !is.null(x$Qupstream) & dots$FUN_MOD != "RunModel_Lag"
  # Add IsHyst in parameters if carried by InputsModel
  if (!is.null(x$model$IsHyst)) {
    dots$IsHyst <- x$model$IsHyst
  }
  # Call airGR function
  do.call(airGR::CreateCalibOptions, dots)
}

#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.character <- function(x, FixedParam = NULL, ...) {
  airGR::CreateCalibOptions(
    FUN_MOD = x,
    ...
  )
}

#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.function <- function(x, FixedParam = NULL, ...) {
  airGR::CreateCalibOptions(
    FUN_MOD = x,
    ...
  )
}

#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.RunModel_Reservoir <- function(x, FixedParam = NULL, ...) {
  stopifnot(inherits(x, "InputsModel"))
  CalibOptions <- CreateCalibOptions.InputsModel(x, FixedParam = NULL, ...)
  if (!is.null(FixedParam)) {
    CalibOptions$FixedParam <- FixedParam
  } else {
    warning(
      "The node '",
      x$id,
      "' which uses `RunModel_Reservoir` must have its parameters fixed: ",
      "\n",
      "You can either fix these parameters afterward by using the command:\n",
      "`CalibOptions[['",
      x$id,
      "']]$FixedParam <- c(Vmax, celerity)`\n",
      "Or by calling `CreateCalibOptions(InputsModel, FixedParam = list('",
      x$id,
      "' = c(Vmax, celerity)))`"
    )
  }
  return(CalibOptions)
}
