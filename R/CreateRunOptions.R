#' Creation of the RunOptions object
#'
#' This function can be used either for a catchment (with an \emph{InputsModel} object) or for a network (with a \emph{GRiwrmInputsModel} object)
#'
#' @template param_x
#' @param InputsModel object of class \emph{InputsModel} (only used to be consistent
#'        with the original [airGR::CreateRunOptions] which has `FUN_MOD` as first
#'        parameter)
#'        see [airGR::CreateInputsModel] for details
#' @param ... arguments passed to [airGR::CreateRunOptions], see details
#'
#' @details See [airGR::CreateRunOptions] documentation for a complete list of arguments.
#'
#' If `x` argument is a \emph{GRiwrmInputsModel} object, `IniStates` must be a
#' list of [numeric] object of class \emph{IniStates} with one item per modeled sub-catchment.
#'
#' With a \emph{GRiwrmInputsModel} object, all arguments are applied on each
#' sub-catchments of the network.
#'
#' For examples of use see topics [RunModel.GRiwrmInputsModel], [RunModel_Reservoir],
#' and [RunModel.Supervisor].
#'
#' @return Depending on the class of `InputsModel` argument (respectively
#' \emph{InputsModel} and \emph{GRiwrmInputsModel} object), the returned value is respectively:
#' - a `RunOptions` object (See [airGR::CreateRunOptions])
#' - a `GRiwrmRunOptions` object which is a [list] of `RunOptions` objects with one item per modeled sub-catchment
#'
#' @rdname CreateRunOptions
#' @export
#' @seealso [CreateGRiwrm()], [CreateInputsModel.GRiwrm()], [RunModel.GRiwrmInputsModel()]
CreateRunOptions <- function(x, ...) {
  UseMethod("CreateRunOptions", x)
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.InputsModel <- function(x, ...) {
  dots <- CreateRunOptions_prepare_inputs(x, ...)
  # Add IsHyst in parameters if carried by InputsModel
  if (!is.null(x$model$IsHyst)) {
    dots$IsHyst <- x$model$IsHyst
  }
  warning_pattern <- "does not require .* Values? set to NA"
  # ClodoFix: airGR does not accept Qsim_m3 on upstream nodes :(
  if (!inherits(x, "SD") && "Qsim_m3" %in% dots$Outputs_Sim) {
    Outputs_Sim <- dots$Outputs_Sim
    dots$Outputs_Sim <- setdiff(dots$Outputs_Sim, "Qsim_m3")
  }
  RunOptions <- CreateRunOptions_runAirGR(dots, warning_pattern)
  # ClodoFix: airGR does not accept Qsim_m3 on upstream nodes :(
  if (exists("Outputs_Sim")) {
    RunOptions$Outputs_Sim <- Outputs_Sim
  }
  return(RunOptions)
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.character <- function(x, InputsModel, ...) {
  CreateRunOptions(x = InputsModel, FUN_MOD = x, ...)
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.function <- function(x, InputsModel, ...) {
  CreateRunOptions(x = InputsModel, FUN_MOD = x, ...)
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.RunModel_Reservoir <- function(x, ...) {
  dots <- CreateRunOptions_prepare_inputs(x, ...)
  # Bypass Outputs_Sim and Outputs_Cal
  Outputs_Sim <- dots$Outputs_Sim
  dots$Outputs_Sim <- "all"
  Outputs_Cal <- dots$Outputs_Cal
  dots$Outputs_Cal <- NULL
  # Bypass airGR::CreateRunOptions for reservoir models with Inistates argument
  # because it is not designed for this type of model and crashes
  if (!is.null(dots$IniStates)) {
    if (!is.numeric(dots$IniStates)) {
      stop("For reservoir models, `IniStates` must be a numeric vector")
      warning_pattern <- ""
    } else {
      IniStates <- dots$IniStates
      dots$IniStates <- NULL
      warning_pattern <- sprintf(
        "(%s)|(%s)",
        warning_pattern,
        "model states initialisation not defined"
      )
    }
  } else {
    IniStates <- NULL
  }
  RunOptions <- CreateRunOptions_runAirGR(dots, warning_pattern)
  if (!is.null(IniStates)) {
    RunOptions$IniStates <- IniStates
  }
  RunOptions$Outputs_Sim <- Outputs_Sim
  RunOptions$Outputs_Cal <- Outputs_Cal
  return(RunOptions)
}

CreateRunOptions_prepare_inputs <- function(x, ...) {
  dots <- list(...)
  dots$InputsModel <- x

  # Add FUN_MOD in parameters if carried by InputsModel
  FUN_MOD <- attr(x, "FeatFUN_MOD")$NameFunMod
  if ("FUN_MOD" %in% names(dots)) {
    if (!identical(match.fun(dots$FUN_MOD), match.fun(FUN_MOD))) {
      stop(
        "The parameter `FUN_MOD` differe from the one defined in `InputsModel`"
      )
    }
  }
  dots$FUN_MOD <- FUN_MOD
  return(dots)
}

CreateRunOptions_runAirGR <- function(dots, warning_pattern) {
  RunOptions <- suppressWarningsRegex(
    do.call(airGR::CreateRunOptions, dots),
    pattern = warning_pattern
  )
  return(RunOptions)
}
