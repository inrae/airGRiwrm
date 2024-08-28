#' Calibration of the parameters of one catchment or a network of sub-catchments
#'
#' Calibration algorithm that optimizes the error criterion selected as objective
#' function using the provided functions.
#'
#' This function can be used either for a catchment (with an \emph{InputsModel}
#' object), for a network (with a \emph{GRiwrmInputsModel} object), or for an
#' ungauged node cluster (with a \emph{Ungauged} object).
#'
#' @param InputsModel \[object of class \emph{InputsModel} or \emph{GRiwrmInputsModel}\] see [CreateInputsModel]
#' @param RunOptions \[object of class \emph{RunOptions} or \emph{GRiwrmRunOptions}\] see [CreateRunOptions]
#' @param InputsCrit \[object of class \emph{InputsCrit} or \emph{GRiwrmInputsCrit}\] see [CreateInputsCrit]
#' @param CalibOptions \[object of class \emph{CalibOptions} or \emph{GRiwrmCalibOptions}\] see [CreateCalibOptions] for details
#' @param ... further arguments passed to [airGR::Calibration], see details
#'
#' @details Argument classes should be consistent to the usage:
#' - a `InputsModel` argument of class \emph{InputsModel} must be followed by a
#' `RunOptions` argument of class \emph{RunOptions}, a `InputsCrit` argument of
#' class \emph{InputsCrit} and a `CalibOptions` of class \emph{CalibOptions}
#' - a `InputsModel` argument of class \emph{GRiwrmInputsModel} must be followed
#' by a `RunOptions` argument of class \emph{GRiwrmRunOptions}, a `InputsCrit`
#' argument of class \emph{GRiwrmInputsCrit} and a `CalibOptions` of class
#' \emph{GRiwrmCalibOptions}
#'
#' See the vignettes for examples.
#'
#' @return Depending on the class of `InputsModel` argument (respectively
#' `InputsModel` and `GRiwrmInputsModel` object), the returned value is respectively:
#' - a `OutputsCalib` object (See [airGR::Calibration] for more details on this object)
#' - a `GRiwrmOutputsCalib` object which is a [list] of `OutputsCalib` objects with
#' one item per modeled sub-catchment
#'
#' @rdname Calibration
#' @seealso [CreateGRiwrm()], [CreateInputsModel.GRiwrm()], [CreateInputsCrit()], [CreateCalibOptions()]
#' @export
Calibration <- function(InputsModel, ...) {
  UseMethod("Calibration", InputsModel)
}

#' @rdname Calibration
#' @export
Calibration.Ungauged <- function(InputsModel, ...) {
  InputsModel$FUN_MOD <- "RunModel_Ungauged"
  NextMethod()
}
