# data set up
e <- setupRunModel()
# variables are copied from environment 'e' to the current environment
# https://stackoverflow.com/questions/9965577/r-copy-move-one-environment-to-another
for(x in ls(e)) assign(x, get(x, e))

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
InputsModel <-
  CreateInputsModel(griwrm2, DatesR, Precip, PotEvap, Qobs)

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

test_that("RunModel.Supervisor with multi time steps controller, two regulations
          in 1 centralised controller that cancel each other out should returns
          same results as RunModel.GRiwrmInputsModel", {
  sv <- CreateSupervisor(InputsModel, TimeStep = 10L)
  fEverything <- function(y) {
    m <- matrix(c(y[,1]/2, -y[,1]/2), ncol = 2)
  }
  CreateController(sv, "Everything", Y = c("54002", "54032"), U = c("R1", "R2"), FUN = fEverything)
  OM_Supervisor <- RunModel(
    sv,
    RunOptions = RunOptions,
    Param = ParamMichel
  )
  expect_equal(OM_Supervisor[["54057"]]$Qsim, OM_GriwrmInputs[["54057"]]$Qsim)
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

