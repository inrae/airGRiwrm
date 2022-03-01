#' Creation of the RunOptions object
#'
#' This function can be used either for a catchment (with an \emph{InputsModel} object) or for a network (with a \emph{GRiwrmInputsModel} object)
#'
#' @template param_x
#' @param ... arguments passed to [airGR::CreateRunOptions], see details
#'
#' @details See [airGR::CreateRunOptions] documentation for a complete list of arguments.
#'
#' If `InputsModel` argument is a \emph{GRiwrmInputsModel} object, `IniStates` must be a list of [numeric] object of class \emph{IniStates} with one item per modeled sub-catchment.
#'
#' With a \emph{GRiwrmInputsModel} object, all arguments are applied on each sub-catchments of the network.
#'
#' @return Depending on the class of `InputsModel` argument (respectively \emph{InputsModel} and \emph{GRiwrmInputsModel} object), the returned value is respectively:
#' - a `RunOptions` object (See [airGR::CreateRunOptions])
#' - a `GRiwrmRunOptions` object which is a [list] of `RunOptions` objects with one item per modeled sub-catchment
#'
#' @rdname CreateRunOptions
#' @export
#' @inherit RunModel.GRiwrmInputsModel return examples
CreateRunOptions <- function(x, ...) {
  UseMethod("CreateRunOptions", x)
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.InputsModel <- function(x, ...) {
  if (!exists("FUN_MOD") && !is.null(x$FUN_MOD)) {
    airGR::CreateRunOptions(FUN_MOD = x$FUN_MOD,
                            InputsModel = x,
                            ...)
  } else {
    airGR::CreateRunOptions(InputsModel = x,
                            ...)
  }
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.character <- function(x, ...) {

  airGR::CreateRunOptions(FUN_MOD = x,
                          ...)
}

#' @rdname CreateRunOptions
#' @export
CreateRunOptions.function <- function(x, ...) {

  airGR::CreateRunOptions(FUN_MOD = x,
                          ...)
}
