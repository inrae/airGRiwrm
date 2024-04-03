test_that("Checks on GRiwrm object with Runmodel_Reservoir", {
  db <- data.frame(
    id = c("Reservoir", "GaugingDown"),
    length = c(1, NA),
    down = c("GaugingDown", NA),
    area = c(NA, 1),
    model = c("RunModel_Reservoir", "RunModel_GR4J"),
    stringsAsFactors = FALSE
  )
  expect_error(CreateGRiwrm(db),
               regexp = "upstream node")
})

skip_on_cran()

test_that("Calibration with Runmodel_Reservoir works!", {
  g <- CreateGRiwrm(n_rsrvr)

  e <- setupRunModel(griwrm = g,
                     runRunModel = FALSE,
                     Qobs2 = Qobs_rsrvr)
  for (x in ls(e)) assign(x, get(x, e))

  InputsCrit <- CreateInputsCrit(InputsModel,
                                 ErrorCrit_KGE2,
                                 RunOptions = RunOptions,
                                 Obs = Qobs[IndPeriod_Run,])
  expect_message(
    CreateInputsCrit(
      InputsModel,
      ErrorCrit_KGE2,
      RunOptions = RunOptions,
      Obs = Qobs[IndPeriod_Run,]
    ),
    regexp = "No observations"
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

  CalibOptions <- CreateCalibOptions(InputsModel,
                                     FixedParam = list(Dam = c(650E6, 1)))
  OC <- Calibration(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions
  )

  expect_equal(OC[["Dam"]]$ParamFinalR, CalibOptions[["Dam"]]$FixedParam)
  expect_gt(OC[["54001"]]$CritFinal, 0.96)
})

expect_dam <- function(nodes, Qobs2) {
  g <- CreateGRiwrm(nodes)

  expect_equal(g$donor[g$id == "54095" & g$model != "Diversion"], "54001")

  e <- setupRunModel(griwrm = g,
                     runRunModel = FALSE,
                     Qobs2 = Qobs2)
  for (x in ls(e)) assign(x, get(x, e))

  InputsCrit <- CreateInputsCrit(InputsModel,
                                 ErrorCrit_KGE2,
                                 RunOptions = RunOptions,
                                 Obs = Qobs[IndPeriod_Run, ])
  CalibOptions <- CreateCalibOptions(InputsModel,
                                     FixedParam = list(Dam = c(650E6, 1)))
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

test_that("Calibration with ungauged node and reservoir in the middle works",{
  n_rsrvr$model[n_rsrvr$id == "54095"] <- "Ungauged"
  expect_dam(n_rsrvr, Qobs_rsrvr)
})

test_that("Calibration with ungauged node and reservoir filled by a diversion works",{
  Qobs2 <- cbind(Qobs_rsrvr, rep(0, nrow(Qobs_rsrvr)))
  colnames(Qobs2) <- c("Dam", "54095")
  expect_dam(n_derived_rsrvr, Qobs2)
})

test_that("Diversion on a reservoir works #146", {
  e <- setupRunModel(runInputsModel = FALSE)
  for (x in ls(e)) assign(x, get(x, e))
  Qrelease <- data.frame(Dam = rep(3508465, length(DatesR)))
  Param <- c(ParamMichel[names(ParamMichel) %in% griwrm$id], list(Dam = c(10E6, 1)))
  e <- setupRunModel(runRunModel = FALSE,
                     griwrm = CreateGRiwrm(n_rsrvr),
                     Qrelease = Qrelease)
  for (x in ls(e)) assign(x, get(x, e))
  OM_resOnly <- RunModel(InputsModel,
                         RunOptions = RunOptions,
                         Param = Param)
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
  Qobs2 <- Qrelease * 0.1
  g <- CreateGRiwrm(nodes)
  e <- setupRunModel(griwrm = g,
                     runRunModel = FALSE,
                     Qobs2 = Qobs2,
                     Qrelease = Qrelease)
  for (x in ls(e)) assign(x, get(x, e))

  OM <- RunModel(InputsModel,
                 RunOptions = RunOptions,
                 Param = Param)
  expect_true(max(OM$Dam$Vsim) - min(OM$Dam$Vsim) > 0)
  expect_false(all(OM$Dam$Vsim == OM_resOnly$Dam$Vsim))
})

test_that("Withdrawal on a reservoir works #147", {
  e <- setupRunModel(runInputsModel = FALSE)
  for (x in ls(e)) assign(x, get(x, e))
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
  Qobs2 <- data.frame(Irrigation = rep(-1E6, length(DatesR)))
  g <- CreateGRiwrm(nodes)
  e <- setupRunModel(griwrm = g,
                     runRunModel = FALSE,
                     Qobs2 = Qobs2,
                     Qrelease = Qrelease)
  for (x in ls(e)) assign(x, get(x, e))
  Param <- c(ParamMichel[names(ParamMichel) %in% griwrm$id], list(Dam = c(20E6, 1)))
  OM <- RunModel(InputsModel,
                 RunOptions = RunOptions,
                 Param = Param)
  expect_equal(which(OM$Dam$Qover_m3 > 0), which(OM$Dam$Vsim == 0))
  expect_equal(OM$`54095`$Qsim_m3, OM$Dam$Qinflows_m3)

  nodes$model[nodes$id == "54095"] <- NA
  Qobs2 <- cbind(Qobs2, Qobs[, "54095"])
  e <- setupRunModel(griwrm = g,
                     runRunModel = FALSE,
                     Qobs2 = Qobs2,
                     Qrelease = Qrelease)
  for (x in ls(e)) assign(x, get(x, e))
  expect_equal(which(OM$Dam$Qover_m3 > 0), which(OM$Dam$Vsim == 0))
  expect_equal(OM$`54095`$Qsim_m3, OM$Dam$Qinflows_m3)
})
