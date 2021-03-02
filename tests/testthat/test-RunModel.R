context("RunModel.Supervisor")

# Load data
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
nTS <- 365
IndPeriod_Run <- seq(
  length(InputsModel[[1]]$DatesR) - nTS + 1,
  length(InputsModel[[1]]$DatesR)
)
IndPeriod_WarmUp = seq(IndPeriod_Run[1]-366,IndPeriod_Run[1]-1)
RunOptions <- CreateRunOptions(
  InputsModel = InputsModel,
  IndPeriod_WarmUp = IndPeriod_WarmUp,
  IndPeriod_Run = IndPeriod_Run
)

# RunModel.GRiwrmInputsModel
Param <- list("54057" = c(0.727,  175.493,   -0.082,    0.029,    4.654))
OM_GriwrmInputs <- RunModel(
  InputsModel,
  RunOptions = RunOptions,
  Param = Param
)

test_that("RunModelSupervisor with no regulation should returns same results as RunModel.GRiwrmInputsModel", {
  sv <- CreateSupervisor(InputsModel)
  OM_Supervisor <- RunModel(
    sv,
    RunOptions = RunOptions,
    Param = Param
  )
  expect_equal(OM_Supervisor[["54057"]]$Qsim, OM_GriwrmInputs[["54057"]]$Qsim)
})

test_that("RunModelSupervisor with two regulations that cancel each other out should returns same results as RunModel.GRiwrmInputsModel", {
  # Add 2 nodes to the network
  griwrm2 <- rbind(griwrm,
                  data.frame(
                    id = c("R1", "R2"),
                    down = "54057",
                    length = 100000,
                    area = NA,
                    model = NA
                  ))
  # Add Qobs for the 2 new nodes
  Qobs2 <- cbind(Qobs, matrix(data = rep(0, 2*nrow(Qobs)), ncol = 2))
  colnames(Qobs2) <- c(colnames(Qobs2)[1:6], "R1", "R2")
  InputsModel <- CreateInputsModel(griwrm2, DatesR, Precip, PotEvap, Qobs2)
  sv <- CreateSupervisor(InputsModel)
  # Function to withdraw half of the measured flow
  fWithdrawal <- function(y) { -y/2 }
  # Function to release half of the the measured flow
  fRelease <- function(y) { y/2 }
  # Controller that withdraw half of the flow measured at node "54002" at location "R1"
  createController(sv, "Withdrawal", Y = c("54002"), U = c("R1"), FUN = fWithdrawal)
  # Controller that release half of the flow measured at node "54002" at location "R2"
  createController(sv, "Release", Y = c("54002"), U = c("R2"), FUN = fRelease)
  OM_Supervisor <- RunModel(
    sv,
    RunOptions = RunOptions,
    Param = Param
  )
  expect_equal(OM_Supervisor[["54057"]]$Qsim, OM_GriwrmInputs[["54057"]]$Qsim)
})

