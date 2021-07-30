# data set up
e <- setupRunModel()
# variables are copied from environment 'e' to the current environment
# https://stackoverflow.com/questions/9965577/r-copy-move-one-environment-to-another
for(x in ls(e)) assign(x, get(x, e))

context("Calibration.GRiwrmInputsModel")

CalibOptions <- CreateCalibOptions(InputsModel = InputsModel)

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

test_that("Calibration with regularisation is OK", {
  InputsCrit <- CreateInputsCrit(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run,],
    AprioriIds = c(
      "54057" = "54032",
      "54032" = "54001",
      "54001" = "54095"
    )
  )

  OC <- Calibration(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions
  )

  ParamDeLavenne <- lapply(OC, "[[", "ParamFinalR")
  expect_equal(OC[["54095"]]$CritFinal, ErrorCrit(
    InputsCrit[["54095"]],
    RunModel(InputsModel, RunOptions, ParamDeLavenne)[["54095"]]
  )$CritValue)
  OM <- RunModel(InputsModel, RunOptions, ParamDeLavenne)
  lapply(names(OC), function(id) {
    expect_gt(
      ErrorCrit(
        InputsCrit[[id]],
        OM[[id]]
      )$CritValue,
      0.9
    )
  })
})
