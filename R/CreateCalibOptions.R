#' Creation of the CalibOptions object
#'
#' This function can be used either for a catchment (with an \emph{InputsModel} object) or for a network (with a \emph{GRiwrmInputsModel} object)
#'
#' @param InputsModel object of class \emph{InputsModel} or \emph{GRwirmInputsModel}. See [CreateInputsModel] for details
#' @param ... arguments passed to [airGR::CreateCalibOptions], see details
#'
#' @details See [airGR::CreateCalibOptions] documentation for a complete list of arguments.
#'
#' With a \emph{GRwirmInputsModel} object, all arguments are applied on each sub-catchments of the network.
#'
#' @return Depending on the class of `InputsModel` argument (respectively `InputsModel` and `GRiwrmInputsModel` object), the returned value is respectively:
#' - a `CalibOptions` object (See [airGR::CreateCalibOptions])
#' - a `GRiwrmCalibOptions` object which is a [list] of `CalibOptions` object with one item per modelled sub-catchment
#'
#' @rdname CreateCalibOptions
#' @export
CreateCalibOptions <- function(InputsModel, ...) {
  UseMethod("CreateCalibOptions", InputsModel)
}
