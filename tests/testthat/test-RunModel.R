context("RunModel.Supervisor")

test_that("RunModelSupervisor with no regulation should returns same results as RunModel.GRiwrmInputsModel", {
  data(Severn)
  # Network configuration
  nodes <- Severn$BasinsInfo[c(1,2,5), c("gauge_id", "downstream_id", "distance_downstream", "area")]
  nodes$distance_downstream <- nodes$distance_downstream * 1000 # Conversion km -> m
  nodes$model <- NA
  nodes$model[1] <- "RunModel_GR4J"
  griwrm <- GRiwrm(nodes, list(id = "gauge_id", down = "downstream_id", length = "distance_downstream"))
  # InputsModel
  DatesR <- Severn$BasinsObs[[1]]$DatesR
  PrecipTot <- cbind(sapply(Severn$BasinsObs, function(x) {x$precipitation}))
  PotEvapTot <- cbind(sapply(Severn$BasinsObs, function(x) {x$peti}))
  Precip <- ConvertMeteoSD(griwrm, PrecipTot)
  PotEvap <- ConvertMeteoSD(griwrm, PotEvapTot)
  Qobs <- cbind(sapply(Severn$BasinsObs, function(x) {x$discharge_spec}))
  InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap, Qobs)
  # RunOptions
  IndPeriod_Run <- seq(
    length(InputsModel[[1]]$DatesR) - 365,
    length(InputsModel[[1]]$DatesR)
  )
  IndPeriod_WarmUp = seq(IndPeriod_Run[1]-366,IndPeriod_Run[1]-1)
  RunOptions <- CreateRunOptions(
    InputsModel = InputsModel,
    IndPeriod_WarmUp = IndPeriod_WarmUp,
    IndPeriod_Run = IndPeriod_Run
  )
  Param <- list("54057" = c(0.727,  175.493,   -0.082,    0.029,    4.654))
  OM_GriwrmInputs <- RunModel(
    InputsModel,
    RunOptions = RunOptions,
    Param = Param
  )
  supervisor <- CreateSupervisor(InputsModel)
  OM_Supervisor <- RunModel(
    supervisor,
    RunOptions = RunOptions,
    Param = Param
  )
  expect_equal(OM_Supervisor[["54057"]]$Qsim, OM_GriwrmInputs[["54057"]]$Qsim)
})
