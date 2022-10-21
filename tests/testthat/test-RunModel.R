# data set up
e <- setupRunModel()
# variables are copied from environment 'e' to the current environment
# https://stackoverflow.com/questions/9965577/r-copy-move-one-environment-to-another
for(x in ls(e)) assign(x, get(x, e))

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
nodes2 <- rbind(nodes,
                data.frame(
                  id = c("R1", "R2"),
                  down = "54057",
                  length = 100,
                  area = NA,
                  model = NA
                ))
griwrm2 <- CreateGRiwrm(nodes2)

# Add Qobs for the 2 new nodes and create InputsModel
Qobs <- matrix(data = rep(0, 2*nrow(Qobs)), ncol = 2)
colnames(Qobs) <- c("R1", "R2")
InputsModel <- suppressWarnings(
  CreateInputsModel(griwrm2, DatesR, Precip, PotEvap, Qobs)
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
      HypsoData = l$HypsoData
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

test_that("RunModel.Supervisor with NA values in Qupstream", {
  # Create Supervisor
  InputsModel$`54057`$Qupstream[, c("R1", "R2")] <- NA
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
  expect_equal(OM_Supervisor[["54057"]]$Qsim[1:3], rep(as.double(NA),3))
  expect_equal(OM_Supervisor[["54057"]]$Qsim[4:length(IndPeriod_Run)],
               OM_GriwrmInputs[["54057"]]$Qsim[4:length(IndPeriod_Run)])
})


n_div <- rbind(nodes,
               data.frame(id = "54029", down = "54002", length = 50, area = NA, model = "Diversion"))
g_div <- CreateGRiwrm(n_div)
Qmin = matrix(1E5, nrow = length(DatesR), ncol = 1)
colnames(Qmin) = "54029"
Qobs <- -Qmin
IM_div <- CreateInputsModel(g_div, DatesR, Precip, PotEvap, Qobs = Qobs, Qmin = Qmin)
RO_div <- setupRunOptions(IM_div)$RunOptions
P_div <- ParamMichel
P_div$`54002` <- c(1, ParamMichel$`54002`)

test_that("RunModel_Diversion with zero diversion equals no diversion", {
  Qobs[, ] <- 0
  IM <- CreateInputsModel(g_div, DatesR, Precip, PotEvap, Qobs = Qobs, Qmin = Qmin)
  OM <- RunModel(IM, RunOptions = RO_div, Param = P_div)
  expect_s3_class(OM, "GRiwrmOutputsModel")
  lapply(names(OM), function(id) {
    expect_equal(OM[[!!id]]$Qsim, OM_GriwrmInputs[[!!id]]$Qsim)
    expect_equal(OM[[!!id]]$Qsim_m3, OM_GriwrmInputs[[!!id]]$Qsim_m3)
    expect_equal(OM[[!!id]]$RunOptions$WarmUpQsim, OM_GriwrmInputs[[!!id]]$RunOptions$WarmUpQsim)
    expect_equal(OM[[!!id]]$RunOptions$WarmUpQsim_m3, OM_GriwrmInputs[[!!id]]$RunOptions$WarmUpQsim_m3)
  })

})
