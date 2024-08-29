runCalibration <- function(nodes = NULL,
                           Qinf = NULL,
                           InputsCrit = NULL,
                           CalibOptions = NULL,
                           FUN_CRIT = ErrorCrit_KGE2,
                           runRunModel = FALSE,
                           IsHyst = FALSE,
                           doCalibration = TRUE) {
  if (is.null(nodes)) {
    griwrm <- NULL
  } else if (inherits(nodes, "GRiwrm")) {
    griwrm <- nodes
  } else {
    griwrm <- CreateGRiwrm(nodes)
  }
  e <- setupRunModel(griwrm = griwrm,
                     runRunModel = runRunModel,
                     Qinf = Qinf,
                     IsHyst = IsHyst)
  for (x in ls(e)) assign(x, get(x, e))
  rm(e)
  np <- getAllNodesProperties(griwrm)

  if (is.null(InputsCrit)) {
    InputsCrit <- CreateInputsCrit(
      InputsModel,
      FUN_CRIT = FUN_CRIT,
      RunOptions = RunOptions,
      Obs = Qobs[IndPeriod_Run, np$id[np$calibration == "Gauged"], drop = FALSE],
    )
  }

  if (is.null(CalibOptions)) {
    CalibOptions <- CreateCalibOptions(InputsModel)
  }
  if (doCalibration) {
    OutputsCalib <- Calibration(InputsModel, RunOptions, InputsCrit, CalibOptions)
    Param <- sapply(OutputsCalib, "[[", "ParamFinalR")
  }
  return(environment())
}
