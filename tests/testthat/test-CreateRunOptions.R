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
RunOptions <- airGR::CreateRunOptions(RunModel_GR4J,
                                      InputsModel = InputsModel,
                                      IndPeriod_Run = Ind_Run,
                                      IndPeriod_WarmUp = Ind_WarmUp)

test_that("CreateRunOptions.InputsModel works", {
  expect_equal(
    CreateRunOptions(InputsModel,
                     FUN_MOD = RunModel_GR4J,
                     IndPeriod_Run = Ind_Run,
                     IndPeriod_WarmUp = Ind_WarmUp),
    RunOptions)
  InputsModel$FUN_MOD = RunModel_GR4J
  expect_equal(
    CreateRunOptions(InputsModel,
                     IndPeriod_Run = Ind_Run,
                     IndPeriod_WarmUp = Ind_WarmUp),
    RunOptions)
})

test_that("CreateRunOptions.character works", {
  expect_equal(
    CreateRunOptions("RunModel_GR4J",
                     InputsModel = InputsModel,
                     IndPeriod_Run = Ind_Run,
                     IndPeriod_WarmUp = Ind_WarmUp),
    RunOptions)
})

test_that("CreateRunOptions.function works", {
  expect_equal(
    CreateRunOptions(RunModel_GR4J,
                     InputsModel = InputsModel,
                     IndPeriod_Run = Ind_Run,
                     IndPeriod_WarmUp = Ind_WarmUp),
    RunOptions)
})

