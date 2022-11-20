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
Qobs <- matrix(data = rep(0, 2*length(DatesR)), ncol = 2)
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

test_that("RunModel.Supervisor with diversion node should not produce NAs", {
  nodes_div <- nodes
  nodes_div <- rbind(nodes_div, data.frame(id = "54001",
                                           down = "54029",
                                           length = 25,
                                           model = "Diversion",
                                           area = NA))
  g_div <- CreateGRiwrm(nodes_div)
  Qobs2 <- matrix(data = rep(0, length(DatesR)), ncol = 1)
  colnames(Qobs2) <- "54001"
  e <- setupRunModel(griwrm = g_div, runRunModel = FALSE, Qobs2 = Qobs2)
  for(x in ls(e)) assign(x, get(x, e))
  sv <- CreateSupervisor(InputsModel, TimeStep = 1L)
  logicFunFactory <- function(sv) {
    #' @param Y Flow measured at "54002" the previous time step
    function(Y) {
      Qnat <- Y
      #  We need to remove the diverted flow to compute the natural flow at "54002"
      lastU <- sv$controllers[[sv$controller.id]]$U
      if (length(lastU) > 0) {
        Qnat <- max(0, Y + lastU)
      }
      return(-max(5.3 * 86400 - Qnat, 0))
    }
  }
  CreateController(sv,
                   ctrl.id = "Low flow support",
                   Y = "54029",
                   U = "54001",
                   FUN = logicFunFactory(sv))
  ParamMichel$`54029` <- c(1, ParamMichel$`54029`)
  OM_Supervisor <- RunModel(
    sv,
    RunOptions = RunOptions,
    Param = ParamMichel
  )
  expect_true(all(OM_Supervisor$`54001`$Qdiv_m3 >= 0))
  lapply(OM_Supervisor, function(OM) {
    expect_false(any(is.na(OM$Qsim)))
    expect_false(any(is.na(OM$Qsim_m3)))
  })
})
