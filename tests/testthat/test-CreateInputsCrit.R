test_that("airGR::CreateInputsCrit should works", {
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
  InputsCrit <- airGR::CreateInputsCrit(ErrorCrit_NSE, InputsModel = InputsModel,
                                 RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run])
  expect_equal(CreateInputsCrit(ErrorCrit_NSE, InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run]),
               InputsCrit)
  expect_equal(CreateInputsCrit("ErrorCrit_NSE", InputsModel = InputsModel,
                                RunOptions = RunOptions, Obs = BasinObs$Qmm[Ind_Run]),
               InputsCrit)
})

# data set up
e <- setupRunModel()
# variables are copied from environment 'e' to the current environment
# https://stackoverflow.com/questions/9965577/r-copy-move-one-environment-to-another
for(x in ls(e)) assign(x, get(x, e))

context("CreateInputsCrit.GRiwrmInputsModel")

test_that("Wrong argument class should throw error", {
  expect_error(CreateInputsCrit(InputsModel = InputsModel[[1]],
                                RunOptions = RunOptions,
                                Obs = Qobs[IndPeriod_Run,]))
  expect_error(CreateInputsCrit.GRiwrmInputsModel(InputsModel = InputsModel[[1]],
                                RunOptions = RunOptions,
                                Obs = Qobs[IndPeriod_Run,]),
               regexp = "GRiwrmInputsModel")

  expect_error(CreateInputsCrit(InputsModel = InputsModel,
                                RunOptions = RunOptions[[1]],
                                Obs = Qobs[IndPeriod_Run,]),
               regexp = "GRiwrmRunOptions")
  expect_error(CreateInputsCrit(InputsModel = InputsModel,
                                RunOptions = RunOptions,
                                Obs = 1),
               regexp = "matrix or data.frame")
})

test_that("Using Lavenne criterion with 'weight' should throw error", {
  expect_error(
    CreateInputsCrit(InputsModel = InputsModel,
                     RunOptions = RunOptions,
                     Obs = Qobs[IndPeriod_Run,],
                     AprioriIds = c("54057" = "54032", "54032" = "54001", "54001" = "54095"),
                     Weights = c(0.85)),
    regexp = "Lavenne"
  )
})

test_that("Lavenne criterion without defining `transfo` should throw error", {
  expect_error(CreateInputsCrit(InputsModel = InputsModel,
                                      RunOptions = RunOptions,
                                      Obs = Qobs[IndPeriod_Run,],
                                      AprioriIds = c("54057" = "54032")),
               regexp = "transfo")
})

AprioriIds <- c("54057" = "54032", "54032" = "54001", "54001" = "54095")
IC <- CreateInputsCrit(InputsModel = InputsModel,
                       RunOptions = RunOptions,
                       Obs = Qobs[IndPeriod_Run,],
                       AprioriIds = AprioriIds,
                       transfo = "sqrt")

test_that("Lavenne criterion is OK", {
  expect_s3_class(IC[["54057"]], "InputsCritLavenneFunction")
  Lavenne_FUN <- attr(IC[["54057"]], "Lavenne_FUN")
  IC57 <- Lavenne_FUN(ParamMichel[["54032"]], 0.9)
  expect_s3_class(IC57, "InputsCrit")
  expect_s3_class(IC57, "Compo")
})

test_that("Lavenne embedded data is correct #57", {
  lapply(names(AprioriIds), function(id) {
    p <- as.list(environment(attr(IC[[id]], "Lavenne_FUN")))
    expect_equal(id, p$InputsModel$id)
  })
})

test_that("Lavenne criterion: wrong sub-catchment order should throw error", {
  expect_error(
    CreateInputsCrit(InputsModel = InputsModel,
                     RunOptions = RunOptions,
                     Obs = Qobs[IndPeriod_Run,],
                     AprioriIds = c("54057" = "54032", "54032" = "54001", "54001" = "54029"),
                     transfo = "sqrt"),
    regexp = "is not upstream the node"
  )
})
