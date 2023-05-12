runCalibration <- function(nodes = NULL,
                           Qobs2 = NULL,
                           InputsCrit = NULL,
                           FUN_CRIT = ErrorCrit_KGE2,
                           runRunModel = FALSE) {
  if (is.null(nodes)) {
    griwrm <- NULL
  } else {
    griwrm <- CreateGRiwrm(nodes)
  }
  e <- setupRunModel(griwrm = griwrm, runRunModel = runRunModel, Qobs2 = Qobs2)
  for(x in ls(e)) assign(x, get(x, e))
  rm(e)
  np <- getAllNodesProperties(griwrm)

  if (is.null(InputsCrit)) {
    InputsCrit <- CreateInputsCrit(
      InputsModel,
      FUN_CRIT = FUN_CRIT,
      RunOptions = RunOptions,
      Obs = Qobs[IndPeriod_Run, np$id[np$RunOff & np$calibration == "Gauged"], drop = FALSE],
    )
  }

  CalibOptions <- CreateCalibOptions(InputsModel)
  OutputsCalib <- Calibration(InputsModel, RunOptions, InputsCrit, CalibOptions)
  Param <- sapply(OutputsCalib, "[[", "ParamFinalR")
  return(environment())
}
