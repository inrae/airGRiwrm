runCalibration <- function(
  nodes = loadSevernNodes(),
  Qinf = NULL,
  Qrelease = NULL,
  InputsCrit = NULL,
  CalibOptions = NULL,
  FUN_CRIT = ErrorCrit_KGE2,
  use_default_AprioriIds = TRUE,
  runRunModel = FALSE,
  IsHyst = FALSE,
  doCalibration = TRUE
) {
  if (is.null(nodes)) {
    griwrm <- NULL
  } else if (inherits(nodes, "GRiwrm")) {
    griwrm <- nodes
  } else {
    griwrm <- CreateGRiwrm(nodes)
  }
  e <- setupRunModel(
    nodes = nodes,
    griwrm = griwrm,
    runRunModel = runRunModel,
    Qinf = Qinf,
    Qrelease = Qrelease,
    IsHyst = IsHyst
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  rm(e)
  np <- getAllNodesProperties(griwrm)

  if (is.null(InputsCrit)) {
    if (use_default_AprioriIds) {
      AprioriIds <- getDefaultAprioriIds(InputsModel)
    } else {
      AprioriIds <- NULL
    }
    InputsCrit <- CreateInputsCrit(
      InputsModel,
      FUN_CRIT = FUN_CRIT,
      RunOptions = RunOptions,
      Obs = Qobs[
        IndPeriod_Run,
        np$id[np$calibration == "Gauged"],
        drop = FALSE
      ],
      AprioriIds = AprioriIds
    )
  }

  if (is.null(CalibOptions)) {
    CalibOptions <- CreateCalibOptions(InputsModel)
  }
  if (doCalibration) {
    OutputsCalib <- Calibration(
      InputsModel,
      RunOptions,
      InputsCrit,
      CalibOptions
    )
    Param <- extractParam(OutputsCalib)
  }
  return(environment())
}
