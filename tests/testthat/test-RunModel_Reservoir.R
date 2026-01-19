test_that("Checks on GRiwrm object with Runmodel_Reservoir", {
  db <- data.frame(
    id = c("Reservoir", "GaugingDown"),
    length = c(1, NA),
    down = c("GaugingDown", NA),
    area = c(NA, 1),
    model = c("RunModel_Reservoir", "RunModel_GR4J"),
    stringsAsFactors = FALSE
  )
  expect_error(CreateGRiwrm(db), regexp = "upstream node")
})

skip_on_cran()

e <- setupRunModel(runInputsModel = FALSE)
for (x in ls(e)) {
  assign(x, get(x, e))
}

test_that("Calibration with Runmodel_Reservoir works!", {
  g <- CreateGRiwrm(n_rsrvr)

  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qinf = Qinf_rsrvr)
  for (x in ls(e)) {
    assign(x, get(x, e))
  }

  InputsCrit <- CreateInputsCrit(
    InputsModel,
    ErrorCrit_KGE2,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, ]
  )

  expect_warning(CreateCalibOptions(InputsModel), regexp = "FixedParam")

  CalibOptions <- suppressWarnings(CreateCalibOptions(InputsModel))
  expect_error(
    Calibration(
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      InputsCrit = InputsCrit,
      CalibOptions = CalibOptions
    ),
    regexp = "FixedParam"
  )

  CalibOptions <- CreateCalibOptions(
    InputsModel,
    FixedParam = list(Dam = c(650E6, 1))
  )
  OC <- Calibration(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions,
    forceReservoirObs = FALSE
  )

  expect_equal(OC[["Dam"]]$ParamFinalR, CalibOptions[["Dam"]]$FixedParam)
  expect_gt(OC[["54001"]]$CritFinal, 0.96)
})

expect_dam <- function(nodes, Qinf) {
  g <- CreateGRiwrm(nodes)

  expect_equal(g$donor[g$id == "54095" & g$model != "Diversion"], "54001")

  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qinf = Qinf)
  for (x in ls(e)) {
    assign(x, get(x, e))
  }

  InputsCrit <- CreateInputsCrit(
    InputsModel,
    ErrorCrit_KGE2,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, ]
  )
  CalibOptions <- CreateCalibOptions(
    InputsModel,
    FixedParam = list(Dam = c(650E6, 1))
  )
  OC <- Calibration(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions
  )
  # X1, X2, X3 are identical
  expect_equal(OC$`54001`$ParamFinalR[2:4], OC$`54095`$ParamFinalR[1:3])
  expect_equal(OC$Dam$ParamFinalR, CalibOptions[["Dam"]]$FixedParam)
}

test_that("Calibration with ungauged node and reservoir in the middle works", {
  n_rsrvr$model[n_rsrvr$id == "54095"] <- "Ungauged"
  expect_dam(n_rsrvr, Qinf_rsrvr)
})

test_that("Calibration with ungauged node and reservoir filled by a diversion works", {
  Qinf <- cbind(Qinf_rsrvr, rep(0, nrow(Qinf_rsrvr)))
  colnames(Qinf) <- c("Dam", "54095")
  expect_dam(n_derived_rsrvr, Qinf)
})

test_that("Diversion on a reservoir works #146", {
  Qrelease <- data.frame(Dam = rep(3508465, length(DatesR)))
  Param <- c(
    ParamMichel[names(ParamMichel) %in% griwrm$id],
    list(Dam = c(10E6, 1))
  )
  e <- setupRunModel(
    runRunModel = FALSE,
    griwrm = CreateGRiwrm(n_rsrvr),
    Qrelease = Qrelease
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  OM_resOnly <- RunModel(InputsModel, RunOptions = RunOptions, Param = Param)
  nodes <- rbind(
    n_rsrvr,
    data.frame(
      id = "Dam",
      down = NA,
      length = NA,
      area = NA,
      model = "Diversion"
    )
  )
  Qinf <- Qrelease * 0.1
  g <- CreateGRiwrm(nodes)
  e <- setupRunModel(
    griwrm = g,
    runRunModel = FALSE,
    Qinf = Qinf,
    Qrelease = Qrelease
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }

  OM <- RunModel(InputsModel, RunOptions = RunOptions, Param = Param)
  expect_true(max(OM$Dam$Vsim) - min(OM$Dam$Vsim) > 0)
  expect_false(all(OM$Dam$Vsim == OM_resOnly$Dam$Vsim))
})

test_that("Withdrawal on a reservoir works #147", {
  nodes <- rbind(
    n_rsrvr,
    data.frame(
      id = "Irrigation",
      down = "Dam",
      length = 0,
      area = NA,
      model = NA
    )
  )
  Qrelease <- data.frame(Dam = rep(1E6, length(DatesR)))
  Qinf <- data.frame(Irrigation = rep(-1E6, length(DatesR)))
  e <- setupRunModel(
    nodes = nodes,
    runRunModel = FALSE,
    Qinf = Qinf,
    Qrelease = Qrelease
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  Param <- c(
    ParamMichel[names(ParamMichel) %in% griwrm$id],
    list(Dam = c(20E6, 1))
  )
  OM <- RunModel(InputsModel, RunOptions = RunOptions, Param = Param)
  expect_equal(which(OM$Dam$Qsim_m3 < 1E6), which(OM$Dam$Vsim == 0))
  expect_true(all(which(OM$Dam$Qover_m3 > 0) %in% which(OM$Dam$Qsim_m3 < 1E6)))
  expect_equal(OM$`54095`$Qsim_m3, OM$Dam$Qinflows_m3)

  nodes$model[nodes$id == "54095"] <- NA
  Qinf <- cbind(Qinf, "54095" = Qobs[, "54095"])
  e <- setupRunModel(
    nodes = nodes,
    runRunModel = FALSE,
    Qinf = Qinf,
    Qrelease = Qrelease
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  OM <- RunModel(InputsModel, RunOptions = RunOptions, Param = Param)
  expect_equal(which(OM$Dam$Qsim_m3 < 1E6), which(OM$Dam$Vsim == 0))
  expect_true(all(which(OM$Dam$Qover_m3 > 0) %in% which(OM$Dam$Qsim_m3 < 1E6)))
  expect_equal(OM$`54095`$Qsim_m3, OM$Dam$Qinflows_m3)
})

test_that("Reservoir with downstream ungauged node works", {
  g <- reduceGRiwrm(CreateGRiwrm(loadSevernNodes()), "54032")
  g$donor <- NULL
  g$model[g$id %in% c("54001", "54029")] <- "Ungauged"
  g$down[g$id == "54001"] <- "Dam2"
  g <- rbind(
    g,
    data.frame(
      id = "Dam",
      down = "54001",
      length = 0,
      area = NA,
      model = "RunModel_Reservoir"
    ),
    data.frame(
      id = "54001",
      down = "54029",
      length = 0,
      area = NA,
      model = "Diversion"
    ),
    data.frame(
      id = "Dam2",
      down = "54032",
      length = 0,
      area = NA,
      model = "RunModel_Reservoir"
    )
  )
  g$down[g$id == "54095"] <- "Dam"
  g <- CreateGRiwrm(g)
  Qrelease <- data.frame(
    Dam = rep(0, length(DatesR)),
    Dam2 = rep(0, length(DatesR))
  )
  Qinf <- matrix(0, ncol = 1, nrow = length(DatesR))
  colnames(Qinf) <- "54001"
  e <- setupRunModel(
    griwrm = g,
    runRunModel = FALSE,
    Qrelease = Qrelease,
    Qinf = Qinf
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  InputsCrit <- CreateInputsCrit(
    InputsModel,
    ErrorCrit_KGE2,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, ]
  )
  CalibOptions <- CreateCalibOptions(
    InputsModel,
    FixedParam = list(Dam = c(1E6, 1), Dam2 = c(1E6, 1))
  )
  OC <- Calibration(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions,
    forceReservoirObs = FALSE
  )
  expect_true(OC$`54032`$CritFinal > 0.96)
})

test_that("Flow release is not impacted by reservoir volume during calibration", {
  Param <- ParamMichel[n_rsrvr$id[-length(n_rsrvr$id)]]
  Param$Dam <- c(1, 1)
  e <- setupRunModel(
    nodes = n_rsrvr,
    runRunModel = TRUE,
    Qinf = Qinf_rsrvr,
    ParamMichel = Param
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }

  # On transparent reservoir release should be identical to inflow
  expect_equal(OM_GriwrmInputs$Dam$Qsim_m3, OM_GriwrmInputs$`54095`$Qsim_m3)

  attr(RunOptions$Dam, "forceReservoirObs") <- TRUE
  OM <- RunModel(
    InputsModel,
    RunOptions = RunOptions,
    Param = Param
  )
  # In calibration mode, release should still remain unchanged from imposed release
  expect_equal(OM$Dam$Qsim_m3, rep(0, length(OM$Dam$Qsim_m3)))

  # Simple model with upstream reservoir and direct injection
  nodes <- data.frame(
    id = c("DI", "Dam", "54095"),
    down = c("Dam", "54095", NA),
    length = c(0, 0, NA),
    area = c(NA, NA, 3722.68),
    model = c(NA, "RunModel_Reservoir", "RunModel_GR4J")
  )

  test_X2_calib_reservoir <- function(Qinf) {
    Qinf <- data.frame(
      DI = rep(Qinf, length(DatesR))
    )
    Qrelease <- data.frame(
      Dam = rep(0, length(DatesR))
    )

    e <- setupRunModel(
      nodes = nodes,
      runRunModel = FALSE,
      Qinf = Qinf,
      Qrelease = Qrelease
    )
    for (x in ls(e)) {
      assign(x, get(x, e))
    }

    e <- runCalibration(
      nodes = nodes,
      Qinf = Qinf,
      Qrelease = Qrelease,
      FUN_CRIT = ErrorCrit_NSE,
      CalibOptions = CreateCalibOptions(
        InputsModel,
        FixedParam = list(Dam = c(1, 1))
      )
    )
    return(e$OutputsCalib$`54095`$ParamFinalR[3])
  }
  expect_equal(test_X2_calib_reservoir(1E6), test_X2_calib_reservoir(0))
})

test_that("Qrelease = NA is equivalent to transparent reservoir", {
  Param <- ParamMichel[n_rsrvr$id[-length(n_rsrvr$id)]]
  Param$Dam <- c(1E9, 1)
  Qinf_NA <- Qinf_rsrvr
  Qinf_NA[] <- NA_real_
  e <- setupRunModel(
    nodes = n_rsrvr,
    runRunModel = TRUE,
    Qinf = Qinf_NA,
    ParamMichel = Param
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  expect_equal(OM_GriwrmInputs$Dam$Qsim_m3, OM_GriwrmInputs$`54095`$Qsim_m3)
  expect_equal(
    OM_GriwrmInputs$Dam$Vsim,
    rep(0, length(OM_GriwrmInputs$Dam$Vsim))
  )
})
