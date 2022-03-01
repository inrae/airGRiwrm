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
  if (!exists("FUN_MOD") && !is.null(x$FUN_MOD)) {
    airGR::CreateCalibOptions(
      FUN_MOD = x$FUN_MOD,
      IsSD = !is.null(x$Qupstream),
      ...
    )
  } else {
    airGR::CreateCalibOptions(
      ...
    )
  }
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
