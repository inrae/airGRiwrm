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

  b <- sapply(InputsModel, function(IM) !IM$inUngaugedCluster)
  gaugedIds <- names(b[b])

  for (id in gaugedIds) {
    IM <- InputsModel[[id]]

    hasUngauged <- IM$hasUngaugedNodes
    if (hasUngauged) {
      l  <- updateParameters4Ungauged(id,
                                      InputsModel,
                                      RunOptions,
                                      CalibOptions,
                                      OutputsModel,
                                      useUpstreamQsim)
      IM <- l$InputsModel
      message("Calibration.GRiwrmInputsModel: Processing sub-basins '",
              paste(names(IM), collapse = "', '"), "' with '", id, "' as gauged donor...")
      attr(RunOptions[[id]], "GRiwrmRunOptions") <- l$RunOptions
    } else {
      message("Calibration.GRiwrmInputsModel: Processing sub-basin '", id, "'...")
      if (useUpstreamQsim && any(IM$UpstreamIsModeled)) {
        # Update InputsModel$Qupstream with simulated upstream flows
        IM <- UpdateQsimUpstream(IM, RunOptions[[id]], OutputsModel)
      }
    }

    if (inherits(InputsCrit[[id]], "InputsCritLavenneFunction")) {
      IC <- getInputsCrit_Lavenne(id, OutputsModel, InputsCrit)
    } else {
      IC <- InputsCrit[[id]]
    }

    if (!is.null(IM$isReservoir) && IM$isReservoir & any(is.na(CalibOptions[[id]]$FixedParam))) {
      stop("Parameters of node '", id, "' using `RunModel_Reservoir` can't be calibrated",
           "Fix its parameters by using the command:\n",
           "`CalibOptions[['", id, "']]$FixedParam <- c(Vmax, celerity)`")
    }

    if (!hasUngauged && IM$isReceiver) {
      # Ungauged node receiving parameters from upstream or sibling node
      OutputsCalib[[id]] <- list(
        ParamFinalR = transferGRparams(InputsModel,
                                       OutputsCalib[[IM$gaugedId]]$ParamFinalR,
                                       IM$gaugedId,
                                       id,
                                       CalibOptions[[id]]$FixedParam,
                                       verbose = TRUE)
      )
      class(OutputsCalib[[id]]) <- c("OutputsCalib", class(OutputsCalib[[id]]))
    } else {
      # Let's calibrate a gauged node!
      OutputsCalib[[id]] <- Calibration(
        InputsModel = IM,
        RunOptions = RunOptions[[id]],
        InputsCrit = IC,
        CalibOptions = CalibOptions[[id]],
        ...
      )
    }

    if (hasUngauged) {
      Ids <- names(IM)
      Ids <- Ids[Ids != id]
      for (uId in Ids) {
        if (IM[[uId]]$gaugedId == id) {
          # Add OutputsCalib for ungauged nodes
          OutputsCalib[[uId]] <- list(
            ParamFinalR = transferGRparams(InputsModel,
                                           OutputsCalib[[id]]$ParamFinalR,
                                           id,
                                           uId,
                                           verbose = TRUE)
          )
          class(OutputsCalib[[uId]]) <- class(OutputsCalib[[id]])
        } else {
          OutputsCalib[[uId]] <- Calibration(
            InputsModel = IM[[uId]],
            RunOptions = RunOptions[[uId]],
            InputsCrit = IC,
            CalibOptions = CalibOptions[[uId]],
            ...
          )
        }
      }
      if (useUpstreamQsim) {
        OM_subnet <- RunModel_Ungauged(IM,
                                       RunOptions[[id]],
                                       OutputsCalib[[id]]$ParamFinalR,
                                       output.all = TRUE)
        OutputsModel <- c(OutputsModel, OM_subnet)
      }
      IM <- IM[[id]]
    } else if (useUpstreamQsim) {
      # Run the model for the sub-basin
      OutputsModel[[id]] <- RunModel(
        x = IM,
        RunOptions = RunOptions[[id]],
        Param = OutputsCalib[[id]]$ParamFinalR
      )
    }
  }

  return(OutputsCalib)

}
