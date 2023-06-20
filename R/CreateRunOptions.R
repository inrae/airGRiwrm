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
#' @return Depending on the class of `InputsModel` argument (respectively
#' \emph{InputsModel} and \emph{GRiwrmInputsModel} object), the returned value is respectively:
#' - a `RunOptions` object (See [airGR::CreateRunOptions])
#' - a `GRiwrmRunOptions` object which is a [list] of `RunOptions` objects with one item per modeled sub-catchment
#'
#' @rdname CreateRunOptions
#' @export
#' @example man-examples/RunModel.GRiwrmInputsModel.R
CreateRunOptions <- function(x, ...) {
  UseMethod("CreateRunOptions", x)
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.InputsModel <- function(x, ...) {
  dots <- list(...)
  dots$InputsModel <- x

  # Add FUN_MOD in parameters if carried by InputsModel
  if (!"FUN_MOD" %in% names(dots)) {
    if(!is.null(x$FUN_MOD)) {
      dots$FUN_MOD <- x$FUN_MOD
    } else {
      stop(" The parameter `FUN_MOD` must be defined")
    }
  }
  # Add IsHyst in parameters if carried by InputsModel
  if (!is.null(x$model$IsHyst)) dots$IsHyst <- x$model$IsHyst

  # Temporary fix waiting for resolution of HYCAR-Hydro/airgr#167
  if (identical(match.fun(dots$FUN_MOD), RunModel_Lag)) {
    dots$IniStates <- CreateIniStates(RunModel_Lag, x)
  }
  # End of temporary fix HYCAR-Hydro/airgr#167
  do.call(airGR::CreateRunOptions, dots)
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.character <- function(x, InputsModel, ...) {
  CreateRunOptions(x = InputsModel,
                   FUN_MOD = x,
                   ...)
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.function <- function(x, InputsModel, ...) {
  CreateRunOptions(x = InputsModel,
                   FUN_MOD = x,
                   ...)
}
