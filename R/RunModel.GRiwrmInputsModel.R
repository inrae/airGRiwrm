#' RunModel function for \emph{GRiwrmInputsModel} object
#'
#' @param x \[object of class \emph{GRiwrmInputsModel}\] see [CreateInputsModel.GRiwrm] for details
#' @param RunOptions \[object of class \emph{GRiwrmRunOptions}\] see [CreateRunOptions.GRiwrmInputsModel] for details
#' @param Param [list] parameter values. The list item names are the IDs of the sub-basins. Each item is a [numeric] [vector]
#' @param ... Further arguments for compatibility with S3 methods
#'
#' @return [[list] of class \emph{GRiwrmOutputsModel}] list of \emph{OutputsModel} objects (See \[airGR::RunModel]) for each node of the semi-distributed model
#' @export
#' @examples
#' #################################################################
#' # Run the `airGRRunModel_Lag` example in the GRiwrm fashion way #
#' #################################################################
#'
#' # Run the airGR RunModel_Lag example for harvesting necessary data
#' library(airGR)
#' example(RunModel_Lag)
#' # detach the package because otherwise airGR overwrites the airGRiwrm functions
#' detach("package:airGR")
#'
#' # This example is a network of 2 nodes which can be describe like this:
#' db <- data.frame(id = c("Reservoir", "GaugingDown"),
#'                  length = c(LengthHydro, NA),
#'                  down = c("GaugingDown", NA),
#'                  area = c(NA, BasinInfo$BasinArea),
#'                  model = c(NA, "RunModel_GR4J"),
#'                  stringsAsFactors = FALSE)
#'
#' # Create GRiwrm object from the data.frame
#' griwrm <- CreateGRiwrm(db)
#' str(griwrm)
#'
#' # Formatting observations for the hydrological models
#' # Each input data should be a matrix or a data.frame with the good id in the name of the column
#' Precip <- matrix(BasinObs$P, ncol = 1)
#' colnames(Precip) <- "GaugingDown"
#' PotEvap <- matrix(BasinObs$E, ncol = 1)
#' colnames(PotEvap) <- "GaugingDown"
#'
#' # Observed flows are integrated now because we mix:
#' #  - flows that are directly injected in the model
#' #  - flows that could be used for the calibration of the hydrological models
#' Qobs = matrix(c(Qupstream, BasinObs$Qmm), ncol = 2)
#' colnames(Qobs) <- griwrm$id
#' str(Qobs)
#'
#' # Creation of the GRiwrmInputsModel object (= a named list of InputsModel objects)
#' InputsModels <- CreateInputsModel(griwrm,
#'                             DatesR = BasinObs$DatesR,
#'                             Precip = Precip,
#'                             PotEvap = PotEvap,
#'                             Qobs = Qobs)
#' str(InputsModels)
#'
#' # Creation of the GriwmRunOptions object
#' RunOptions2 <- CreateRunOptions(InputsModels,
#'                                 IndPeriod_Run = Ind_Run)
#' str(RunOptions2)
#'
#' # Parameters of the SD models should be encapsulated in a named list
#' Param2 <- list(`GaugingDown` = c(Velocity, Param))
#'
#' # RunModel for the whole network
#' OutputsModels <- RunModel(InputsModels,
#'                           RunOptions = RunOptions2,
#'                           Param = Param2)
#' str(OutputsModels)
#'
#' # Comparison between GRiwrm simulation and airGR simulation
#' plot(OutputsModels, Qobs = data.frame(`GaugingDown` = OutputsModel$Qsim))
RunModel.GRiwrmInputsModel <- function(x, RunOptions, Param, ...) {

  checkRunModelParameters(x, RunOptions, Param)

  OutputsModel <- list()
  class(OutputsModel) <- c("GRiwrmOutputsModel", class(OutputsModel))

  for(id in names(x)) {
    message("RunModel.GRiwrmInputsModel: Treating sub-basin ", x[[id]]$id, "...")

    # Update x[[id]]$Qupstream with simulated upstream flows
    if(any(x[[id]]$UpstreamIsRunoff)) {
      x[[id]] <- UpdateQsimUpstream(x[[id]], RunOptions[[id]]$IndPeriod_Run, OutputsModel)
    }

    # Run the model for the sub-basin
    OutputsModel[[id]] <- RunModel.InputsModel(
      x[[id]],
      RunOptions = RunOptions[[id]],
      Param = Param[[id]]
    )
  }
  attr(OutputsModel, "Qm3s") <- OutputsModelQsim(x, OutputsModel, RunOptions[[1]]$IndPeriod_Run)
  return(OutputsModel)
}
