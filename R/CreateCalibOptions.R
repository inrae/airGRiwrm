#' Creation of the CalibOptions object
#'
#' This function can be used either for a catchment (with an \emph{InputsModel} object) or for a network (with a \emph{GRiwrmInputsModel} object)
#'
#' @template param_x
#' @param ... arguments passed to [airGR::CreateCalibOptions], see details
#'
#' @details See [airGR::CreateCalibOptions] documentation for a complete list of arguments.
#'
#' With a \emph{GRiwrmInputsModel} object, all arguments are applied on each sub-catchments of the network.
#'
#' @return Depending on the class of `InputsModel` argument (respectively `InputsModel` and `GRiwrmInputsModel` object), the returned value is respectively:
#' - a `CalibOptions` object (See [airGR::CreateCalibOptions])
#' - a `GRiwrmCalibOptions` object which is a [list] of `CalibOptions` object with one item per modeled sub-catchment
#'
#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions <- function(x, ...) {
  UseMethod("CreateCalibOptions", x)
}

#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.InputsModel <- function(x,
                                           ...) {
  dots <- list(...)
  # Add FUN_MOD in parameters if carried by InputsModel
  if (!"FUN_MOD" %in% names(dots)) {
    if(!is.null(x$FUN_MOD)) {
      dots$FUN_MOD <- x$FUN_MOD
    } else {
      stop(" The parameter `FUN_MOD` must be defined")
    }
  }
  # Automatically define IsSD for intermediate basin GR models
  dots$IsSD = !is.null(x$Qupstream) & dots$FUN_MOD != "RunModel_Lag"
  # Add IsHyst in parameters if carried by InputsModel
  if (!is.null(x$model$IsHyst)) dots$IsHyst <- x$model$IsHyst
  # Call airGR function
  do.call(airGR::CreateCalibOptions, dots)
}

#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.character <- function(x,
                                           ...) {
  airGR::CreateCalibOptions(
    FUN_MOD = x,
    ...
  )
}

#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.function <- function(x,
                                         ...) {
  airGR::CreateCalibOptions(
    FUN_MOD = x,
    ...
  )
}
