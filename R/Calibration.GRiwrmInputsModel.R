#' @param useUpstreamQsim boolean describing if simulated (\code{TRUE}) or observed (\code{FALSE}) flows are used for calibration. Default is \code{TRUE}
#' @rdname Calibration
#' @export
Calibration.GRiwrmInputsModel <- function(InputsModel,
                                          RunOptions,
                                          InputsCrit,
                                          CalibOptions,
                                          useUpstreamQsim = TRUE,
                                          ...) {

  # Argument checks

  # We invoke the mandatory arguments here for avoiding
  # a messy error message on "get(x)" if an argument is missing
  InputsModel
  RunOptions
  InputsCrit
  CalibOptions

  # Checking argument classes
  vars2check <- c("InputsModel", "RunOptions", "InputsCrit", "CalibOptions")
  lapply(vars2check, function(x) {
    if (!inherits(get(x), paste0("GRiwrm", x))) {
      stop(sprintf("'%1$s' must be of class GRiwrm%1$s, type '?Create%1$s' for help", x))
    }
  })

  OutputsCalib <- list()
  class(OutputsCalib) <- append("GRiwrmOutputsCalib", class(OutputsCalib))

  OutputsModel <- list()
  class(OutputsModel) <- append("GRiwrmOutputsModel", class(OutputsModel))

  for(IM in InputsModel) {
    message("Calibration.GRiwrmInputsModel: Treating sub-basin ", IM$id, "...")

    if(useUpstreamQsim && any(IM$UpstreamIsRunoff)) {
      # Update InputsModel$Qupstream with simulated upstream flows
      IM <- UpdateQsimUpstream(IM, RunOptions[[IM$id]], OutputsModel)
    }

    if (inherits(InputsCrit[[IM$id]], "InputsCritLavenneFunction")) {
      IC <- getInputsCrit_Lavenne(IM$id, OutputsModel, InputsCrit)
    } else {
      IC <- InputsCrit[[IM$id]]
    }

    OutputsCalib[[IM$id]] <- Calibration(
      InputsModel = IM,
      RunOptions = RunOptions[[IM$id]],
      InputsCrit = IC,
      CalibOptions = CalibOptions[[IM$id]],
      ...
    )

    if(useUpstreamQsim) {
      # Run the model for the sub-basin
      OutputsModel[[IM$id]] <- RunModel(
        x = IM,
        RunOptions = RunOptions[[IM$id]],
        Param = OutputsCalib[[IM$id]]$ParamFinalR
      )
    }

  }

  return(OutputsCalib)

}

#' Create InputsCrit for De Lavenne regularization
#'
#' Internal function that run [airGR::CreateInputsCrit_Lavenne] on-the-fly with a priori upstream
#' sub-catchment parameters grabbed during network calibration process.
#'
#' @param id [character] the id of the current sub-catchment
#' @param OutputsModel \[GRiwrmOutputsModel\] object with simulation results of upstream sub-catchments run with calibrated parameters
#' @param InputsCrit \[InputsCritLavenneFunction\] object internally created by [CreateInputsCrit.GRiwrmInputsModel]
#'
#' @return \[InputsCrit\] object with De Lavenne regularization
#' @import airGR
#' @noRd
#'
getInputsCrit_Lavenne <- function(id, OutputsModel, InputsCrit) {
  if (!inherits(InputsCrit[[id]], "InputsCritLavenneFunction")) {
    stop("'InputsCrit[[id]]' must be of class InputsCritLavenneFunction")
  }
  AprioriId <- attr(InputsCrit[[id]], "AprioriId")
  AprCelerity <- attr(InputsCrit[[id]], "AprCelerity")
  Lavenne_FUN <- attr(InputsCrit[[id]], "Lavenne_FUN")
  AprParamR <- OutputsModel[[AprioriId]]$RunOptions$Param
  if(!inherits(OutputsModel[[AprioriId]], "SD")) {
    # Add default velocity parameter for a priori upstream catchment
    AprParamR <- c(AprCelerity, AprParamR)
  }
  AprCrit <- ErrorCrit(InputsCrit[[AprioriId]], OutputsModel[[AprioriId]])$CritValue
  return(Lavenne_FUN(AprParamR, AprCrit))
}
