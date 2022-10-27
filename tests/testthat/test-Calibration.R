test_that("airGR::Calibration should work", {
  ## loading catchment data
  data(L0123001)

  ## preparation of InputsModel object
  InputsModel <- CreateInputsModel(RunModel_GR4J, DatesR = BasinObs$DatesR,
                                   Precip = BasinObs$P, PotEvap = BasinObs$E)

  ## calibration period selection
  Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1990-01-01"),
                 which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1999-12-31"))
  Ind_WarmUp <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1989-01-01"),
                    which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1989-12-31"))

  ## preparation of RunOptions object
  RunOptions <- CreateRunOptions(RunModel_GR4J,
                                 InputsModel = InputsModel,
                                 IndPeriod_Run = Ind_Run,
                                 IndPeriod_WarmUp = Ind_WarmUp)

  ## calibration criterion: preparation of the InputsCrit object
  InputsCrit <- CreateInputsCrit(ErrorCrit_NSE, InputsModel = InputsModel,
                                 RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])

  ## preparation of CalibOptions object
  CalibOptions <- CreateCalibOptions(RunModel_GR4J, FUN_CALIB = Calibration_Michel)

  ## calibration
  OutputsCalib <- Calibration(InputsModel = InputsModel, RunOptions = RunOptions,
                              InputsCrit = InputsCrit, CalibOptions = CalibOptions,
                              FUN_MOD = RunModel_GR4J,
                              FUN_CALIB = Calibration_Michel)

  expect_length(OutputsCalib$ParamFinalR, 4)
})

# data set up
e <- setupRunModel()
# variables are copied from environment 'e' to the current environment
# https://stackoverflow.com/questions/9965577/r-copy-move-one-environment-to-another
for(x in ls(e)) assign(x, get(x, e))

CalibOptions <- CreateCalibOptions(InputsModel)

test_that("Calibrated parameters remains unchanged", {
  InputsCrit <- CreateInputsCrit(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run,]
  )

  OC <- Calibration(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions
  )

  ParamFinalR <- lapply(OC, "[[", "ParamFinalR")

  lapply(names(ParamFinalR), function(id) expect_equal(ParamFinalR[[id]], ParamMichel[[id]]))

})

test_that("Calibration with regularization is OK", {
  InputsCrit <- CreateInputsCrit(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run,],
    AprioriIds = c(
      "54057" = "54032",
      "54032" = "54001",
      "54001" = "54095"
    ),
    transfo = "sqrt"
  )

  OC <- Calibration(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions
  )

  ParamLavenne <- lapply(OC, "[[", "ParamFinalR")
  expect_equal(OC[["54095"]]$CritFinal, ErrorCrit(
    InputsCrit[["54095"]],
    RunModel(InputsModel, RunOptions, ParamLavenne)[["54095"]]
  )$CritValue)
  OM <- RunModel(InputsModel, RunOptions, ParamLavenne)
  lapply(names(OC), function(id) {
    expect_gt(
      ErrorCrit(
        InputsCrit[[id]],
        OM[[id]]
      )$CritValue,
      0.89
    )
  })
})

test_that("Calibration with Diversion works", {
  n_div <- rbind(nodes,
                 data.frame(id = "54029", down = "54002", length = 50, area = NA, model = "Diversion"))
  g_div <- CreateGRiwrm(n_div)
  Qmin = matrix(1E5, nrow = length(DatesR), ncol = 1)
  colnames(Qmin) = "54029"
  Qdiv <- -Qmin
  IM_div <- CreateInputsModel(g_div, DatesR, Precip, PotEvap, Qobs = Qdiv, Qmin = Qmin)
  RO_div <- setupRunOptions(IM_div)$RunOptions
  P_div <- ParamMichel
  P_div$`54002` <- c(1, ParamMichel$`54002`)
  IC_div <- CreateInputsCrit(
    InputsModel = IM_div,
    RunOptions = RO_div,
    Obs = Qobs[IndPeriod_Run,],
  )
  CO_div <- CreateCalibOptions(IM_div)
  OC <- Calibration(
    InputsModel = IM_div,
    RunOptions = RO_div,
    InputsCrit = IC_div,
    CalibOptions = CO_div
  )
  expect_length(OC$`54002`$ParamFinalR, 5)
})
