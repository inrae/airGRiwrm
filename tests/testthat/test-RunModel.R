# data set up
e <- setupRunModel()
# variables are copied from environment 'e' to the current environment
# https://stackoverflow.com/questions/9965577/r-copy-move-one-environment-to-another
for(x in ls(e)) assign(x, get(x, e))

context("RunModel.GRiwrmInputsModel")

test_that("RunModel.GRiwrmInputsModel should return same result with separated warm-up", {
  RO_WarmUp <- CreateRunOptions(
    InputsModel,
    IndPeriod_WarmUp = 0L,
    IndPeriod_Run = IndPeriod_WarmUp
  )
  OM_WarmUp <- RunModel(
    InputsModel,
    RunOptions = RO_WarmUp,
    Param = ParamMichel
  )
  RO_Run <- CreateRunOptions(
    InputsModel,
    IndPeriod_WarmUp = 0L,
    IndPeriod_Run = IndPeriod_Run,
    IniStates = lapply(OM_WarmUp, "[[", "StateEnd")
  )
  OM_Run <- RunModel(
    InputsModel,
    RunOptions = RO_Run,
    Param = ParamMichel
  )
  lapply(griwrm$id, function(id) {
    # The 2 exclamation marks are for seeing the id in the test result (See ?quasi_label)
    expect_equal(OM_GriwrmInputs[[!!id]]$Qsim, OM_Run[[!!id]]$Qsim)
  })
})

context("RunModel.Supervisor")

test_that("RunModel.Supervisor with no regulation should returns same results as RunModel.GRiwrmInputsModel", {
  sv <- CreateSupervisor(InputsModel)
  OM_Supervisor <- RunModel(
    sv,
    RunOptions = RunOptions,
    Param = ParamMichel
  )
  lapply(griwrm$id, function(id) {
    expect_equal(OM_Supervisor[[!!id]]$Qsim, OM_GriwrmInputs[[!!id]]$Qsim)
  })
})

# Add 2 nodes to the network
griwrm2 <- rbind(griwrm,
                 data.frame(
                   id = c("R1", "R2"),
                   down = "54057",
                   length = 100,
                   area = NA,
                   model = NA
                 ))
# Add Qobs for the 2 new nodes and create InputsModel
Qobs2 <- cbind(Qobs, matrix(data = rep(0, 2*nrow(Qobs)), ncol = 2))
colnames(Qobs2) <- c(colnames(Qobs2)[1:6], "R1", "R2")
InputsModel <- suppressWarnings(
  CreateInputsModel(griwrm2, DatesR, Precip, PotEvap, Qobs2)
)

test_that("RunModel.Supervisor with two regulations that cancel each other out should returns same results as RunModel.GRiwrmInputsModel", {
  # Create Supervisor
  sv <- CreateSupervisor(InputsModel)
  # Function to withdraw half of the measured flow
  fWithdrawal <- function(y) { -y/2 }
  # Function to release half of the the measured flow
  fRelease <- function(y) { y/2 }
  # Controller that withdraw half of the flow measured at node "54002" at location "R1"
  CreateController(sv, "Withdrawal", Y = c("54002"), U = c("R1"), FUN = fWithdrawal)
  # Controller that release half of the flow measured at node "54002" at location "R2"
  CreateController(sv, "Release", Y = c("54002"), U = c("R2"), FUN = fRelease)

  OM_Supervisor <- RunModel(
    sv,
    RunOptions = RunOptions,
    Param = ParamMichel
  )
  expect_equal(OM_Supervisor[["54057"]]$Qsim, OM_GriwrmInputs[["54057"]]$Qsim)
})

test_that("RunModel.Supervisor with multi time steps controller, two regulations in 1 centralised controller that cancel each other out should returns same results as RunModel.GRiwrmInputsModel", {
  sv <- CreateSupervisor(InputsModel, TimeStep = 10L)
  fEverything <- function(y) {
    matrix(c(y[,1]/2, -y[,1]/2), ncol = 2)
  }
  CreateController(sv, "Everything", Y = c("54002", "54032"), U = c("R1", "R2"), FUN = fEverything)
  OM_Supervisor <- RunModel(
    sv,
    RunOptions = RunOptions,
    Param = ParamMichel
  )
  expect_equal(OM_Supervisor[["54057"]]$Qsim, OM_GriwrmInputs[["54057"]]$Qsim)
})

test_that("RunModel.GRiwrmInputsModel handles CemaNeige", {
  l <- setUpCemaNeigeData()
  l$griwrm[l$griwrm$id == "Down", "model"] <- "RunModel_GR4J"
  l$TempMean <- l$TempMean[,1:2]
  l$ZInputs <- l$ZInputs[1:2]
  l$TempMean <- l$TempMean[,1:2]
  l$HypsoData <- l$HypsoData[,1:2]
  InputsModels <- suppressWarnings(
    CreateInputsModel(
      l$griwrm,
      DatesR = l$DatesR,
      Precip = l$Precip,
      PotEvap = l$PotEvap,
      TempMean = l$TempMean,
      ZInputs = l$ZInputs,
      HypsoData = l$HypsoData,
      Qobs = l$Qobs
    )
  )
  ## run period selection
  Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1990-01-01"),
                 which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1999-12-31"))
  ## preparation of the RunOptions object
  RunOptions <- suppressWarnings(CreateRunOptions(InputsModels,
                                                  IndPeriod_Run = Ind_Run))
  ids <- l$griwrm$id
  names(ids) <- ids
  Params <- lapply(ids, function(x) {
    c(X1 = 408.774, X2 = 2.646, X3 = 131.264, X4 = 1.174,
      CNX1 = 0.962, CNX2 = 2.249)
  })
  Params$Down <- c(1, Params$Down[1:4])
  OutputsModel <- RunModel(
    InputsModels,
    RunOptions = RunOptions,
    Param = Params
  )
  expect_named(OutputsModel, l$griwrm$id)
  Qm3s <- attr(OutputsModel, "Qm3s")
  expect_equal(Qm3s[,4], rowSums(Qm3s[,2:3]))
})
