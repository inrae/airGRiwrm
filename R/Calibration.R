#' Calibration of the parameters of one catchment or a network of sub-catchments
#'
#' Calibration algorithm that optimizes the error criterion selected as objective function using the provided functions.
#'
#' This function can be used either for a catchment (with an \emph{InputsModel} object) or for a network (with a \emph{GRiwrmInputsModel} object)
#'
#' @param InputsModel \[object of class \emph{InputsModel} or \emph{GRiwrmInputsModel}\] see [CreateInputsModel]
#' @param RunOptions \[object of class \emph{RunOptions} or \emph{GRiwrmRunOptions}\] see [CreateRunOptions]
#' @param InputsCrit \[object of class \emph{InputsCrit} or \emph{GRiwrmInputsCrit}\] see [CreateInputsCrit]
#' @param CalibOptions \[object of class \emph{CalibOptions} or \emph{GRiwrmCalibOptions}\] see [CreateCalibOptions] for details
#' @param ... further arguments passed to [airGR::Calibration], see details
#'
#' @details Argument classes should be consistent to the usage:
#' - a `InputsModel` argument of class \emph{InputsModel} must be followed by a `RunOptions` argument of class \emph{RunOptions}, a `InputsCrit` argument of class \emph{InputsCrit} and a `CalibOptions` of class \emph{CalibOptions}
#' - - a `InputsModel` argument of class \emph{GRiwrmInputsModel} must be followed by a `RunOptions` argument of class \emph{GRiwrmRunOptions}, a `InputsCrit` argument of class \emph{GRiwrmInputsCrit} and a `CalibOptions` of class \emph{GRiwrmCalibOptions}
#'
#' See the vignettes for examples.
#'
#' @return Depending on the class of `InputsModel` argument (respectively `InputsModel` and `GRiwrmInputsModel` object), the returned value is respectively:
#' - a `InputsCrit` object (See [airGR::CreateInputsCrit])
#' - a `GRiwrmInputsCrit` object which is a [list] of `InputsCrit` objects with one item per modeled sub-catchment
#'
#' @rdname Calibration
#' @export
Calibration <- function(InputsModel, ...) {
  UseMethod("Calibration", InputsModel)
}
