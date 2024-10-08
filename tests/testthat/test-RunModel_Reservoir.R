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

e <- setupRunModel(runInputsModel = FALSE)
for (x in ls(e)) assign(x, get(x, e))

test_that("Calibration with Runmodel_Reservoir works!", {
  g <- CreateGRiwrm(n_rsrvr)

  e <- setupRunModel(griwrm = g,
                     runRunModel = FALSE,
                     Qinf = Qinf_rsrvr)
  for (x in ls(e)) assign(x, get(x, e))

  InputsCrit <- CreateInputsCrit(InputsModel,
                                 ErrorCrit_KGE2,
                                 RunOptions = RunOptions,
                                 Obs = Qobs[IndPeriod_Run,])

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

expect_dam <- function(nodes, Qinf) {
  g <- CreateGRiwrm(nodes)

  expect_equal(g$donor[g$id == "54095" & g$model != "Diversion"], "54001")

  e <- setupRunModel(griwrm = g,
                     runRunModel = FALSE,
                     Qinf = Qinf)
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
  expect_dam(n_rsrvr, Qinf_rsrvr)
})

test_that("Calibration with ungauged node and reservoir filled by a diversion works",{
  Qinf <- cbind(Qinf_rsrvr, rep(0, nrow(Qinf_rsrvr)))
  colnames(Qinf) <- c("Dam", "54095")
  expect_dam(n_derived_rsrvr, Qinf)
})

test_that("Diversion on a reservoir works #146", {
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
  Qinf <- Qrelease * 0.1
  g <- CreateGRiwrm(nodes)
  e <- setupRunModel(griwrm = g,
                     runRunModel = FALSE,
                     Qinf = Qinf,
                     Qrelease = Qrelease)
  for (x in ls(e)) assign(x, get(x, e))

  OM <- RunModel(InputsModel,
                 RunOptions = RunOptions,
                 Param = Param)
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
  g <- CreateGRiwrm(nodes)
  e <- setupRunModel(griwrm = g,
                     runRunModel = FALSE,
                     Qinf = Qinf,
                     Qrelease = Qrelease)
  for (x in ls(e)) assign(x, get(x, e))
  Param <- c(ParamMichel[names(ParamMichel) %in% griwrm$id], list(Dam = c(20E6, 1)))
  OM <- RunModel(InputsModel,
                 RunOptions = RunOptions,
                 Param = Param)
  expect_equal(which(OM$Dam$Qsim_m3 < 1E6), which(OM$Dam$Vsim == 0))
  expect_true(all(which(OM$Dam$Qover_m3 > 0) %in% which(OM$Dam$Qsim_m3 < 1E6)))
  expect_equal(OM$`54095`$Qsim_m3, OM$Dam$Qinflows_m3)

  nodes$model[nodes$id == "54095"] <- NA
  g <- CreateGRiwrm(nodes)
  Qinf <- cbind(Qinf, "54095" = Qobs[, "54095"])
  e <- setupRunModel(griwrm = g,
                     runRunModel = FALSE,
                     Qinf = Qinf,
                     Qrelease = Qrelease)
  for (x in ls(e)) assign(x, get(x, e))
  OM <- RunModel(InputsModel,
                 RunOptions = RunOptions,
                 Param = Param)
  expect_equal(which(OM$Dam$Qsim_m3 < 1E6), which(OM$Dam$Vsim == 0))
  expect_true(all(which(OM$Dam$Qover_m3 > 0) %in% which(OM$Dam$Qsim_m3 < 1E6)))
  expect_equal(OM$`54095`$Qsim_m3, OM$Dam$Qinflows_m3)
})

test_that("Reservoir with downstream ungauged node works", {
  g <- reduceGRiwrm(CreateGRiwrm(loadSevernNodes()), "54032")
  g$donor <- NULL
  g$model[g$id %in% c("54001", "54029")] <- "Ungauged"
  g$down[g$id == "54001"] <- "Dam2"
  g <- rbind(g,
             data.frame(id = "Dam", down = "54001", length = 0, area = NA, model = "RunModel_Reservoir"),
             data.frame(id = "54001", down = "54029", length = 0, area = NA, model = "Diversion"),
             data.frame(id = "Dam2", down = "54032", length = 0, area = NA, model = "RunModel_Reservoir"))
  g$down[g$id == "54095"] <- "Dam"
  g <- CreateGRiwrm(g)
  Qrelease <- data.frame(Dam = rep(0, length(DatesR)),
                         Dam2 = rep(0, length(DatesR)))
  Qinf <- matrix(0, ncol = 1, nrow = length(DatesR))
  colnames(Qinf) <- "54001"
  e <- setupRunModel(griwrm = g,
                     runRunModel = FALSE,
                     Qrelease = Qrelease,
                     Qinf = Qinf)
  for (x in ls(e)) assign(x, get(x, e))
  InputsCrit <- CreateInputsCrit(InputsModel,
                                 ErrorCrit_KGE2,
                                 RunOptions = RunOptions,
                                 Obs = Qobs[IndPeriod_Run, ])
  CalibOptions <- CreateCalibOptions(InputsModel,
                                     FixedParam = list(Dam = c(1E6, 1),
                                                       Dam2 = c(1E6, 1)))
  OC <- Calibration(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions
  )
  expect_true(OC$`54032`$CritFinal > 0.96)
})
