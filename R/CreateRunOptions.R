#' Creation of the CalibOptions object
#'
#' This function can be used either for a catchment (with an \emph{InputsModel} object) or for a network (with a \emph{GRiwrmInputsModel} object)
#'
#' @param InputsModel object of class \emph{InputsModel} or \emph{GRiwrmInputsModel}. See [CreateInputsModel] for details
#' @param ... arguments passed to [airGR::CreateRunOptions], see details
#'
#' @details See [airGR::CreateRunOptions] documentation for a complete list of arguments.
#'
#' If `InputsModel` argument is a \emph{GRiwrmInputsModel} object, `IniStates` must be a list of [numeric] object of class \emph{IniStates} with one item per modelled sub-catchment.
#'
#' With a \emph{GRiwrmInputsModel} object, all arguments are applied on each sub-catchments of the network.
#'
#' @return Depending on the class of `InputsModel` argument (respectively \emph{InputsModel} and \emph{GRiwrmInputsModel} object), the returned value is respectively:
#' - a `RunOptions` object (See [airGR::CreateRunOptions])
#' - a `GRiwrmRunOptions` object which is a [list] of `RunOptions` object with one item per modelled sub-catchment
#'
#' @rdname CreateRunOptions
#' @export
#' @inherit RunModel.GRiwrmInputsModel return examples
CreateRunOptions <- function(InputsModel, ...) {
  UseMethod("CreateRunOptions", InputsModel)
}
