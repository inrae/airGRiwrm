#' Creation of the CalibOptions object
#'
#' This function can be used either for a catchment (with an \emph{InputsModel} object) or for a network (with a \emph{GRiwrmInputsModel} object)
#'
#' @template param_x
#' @param FixedParam a [numeric] [vector] as for [airGR::CreateCalibOptions],
#' or a [list] giving the values of non-optimised parameters (see details)
#' @param ... arguments passed to [airGR::CreateCalibOptions], see details
#'
#' @details See [airGR::CreateCalibOptions] documentation for a complete list of arguments.
#'
#' With a \emph{GRiwrmInputsModel} object, all arguments are applied on each sub-catchments of the network
#' with some adaptation depending on the model used on each node.
#'
#' If the argument `FixedParam` is a [numeric] [vector], it is applied to each node of
#' the network. Parameters are adapted depending on the use of the routing model
#' and the CemaNeige model on each node. If `FixedParam` is a [list] of [numeric],
#' each item of the list will be applied on corresponding nodes. Use the id "*" for applying
#' a setting on the remaining nodes. Example for applying one setting for all the
#' nodes except the id "54057":
#'
#' ```
#' FixedParam <- list(`*` = c(NA, NA, NA, NA, NA, 0.25, NA, 10, NA),
#'                    `54057` = c(0.5, NA, NA, NA, NA, 0.25, NA, 10, NA))
#' ```
#'
#' The argument `IsHyst` is ignored since it should be defined previously with [CreateInputsModel.GRiwrm].
#'
#' @return Depending on the class of `InputsModel` argument (respectively `InputsModel` and `GRiwrmInputsModel` object), the returned value is respectively:
#' - a `CalibOptions` object (See [airGR::CreateCalibOptions])
#' - a `GRiwrmCalibOptions` object which is a [list] of `CalibOptions` object with one item per modeled sub-catchment
#'
#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions <- function(x, FixedParam = NULL, ...) {
  UseMethod("CreateCalibOptions", x)
}

#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions.InputsModel <- function(x, FixedParam = NULL, ...) {
  dots <- list(...)
  # Add FUN_MOD in parameters if carried by InputsModel
  if (!"FUN_MOD" %in% names(dots)) {
    if(!is.null(x$FUN_MOD)) {
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
  if (!is.null(x$model$IsHyst)) dots$IsHyst <- x$model$IsHyst
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
