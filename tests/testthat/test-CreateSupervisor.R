# data set up
e <- setupRunModel(runInputsModel = FALSE)
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
sv <- CreateSupervisor(InputsModel)

test_that("Checks in CreateSupervisor", {
  expect_error(CreateSupervisor(InputsModel, 3.14), regexp  = "integer")
  expect_error(CreateSupervisor(InputsModel, 0L), regexp = "positive")
  expect_s3_class(sv, "Supervisor")
})

test_that("Checks in CreateController",  {
  FUN <- function(Y) return(0)
  expect_error(CreateController(sv, 0, "54057", "R1", FUN),
               regexp = "character")
  expect_error(CreateController(sv, c("toto1", "toto2"), "54057", "R1", FUN),
               regexp = "length")
  expect_error(CreateController(sv, "toto", "fake", "R1", FUN),
               regexp = "GRiwrm")
  expect_error(CreateController(sv, "toto", "54057", "54057", FUN),
               regexp = "DirectInjection")
  CreateController(sv, "toto", "54057", "R1", FUN)
  expect_s3_class(sv$controllers, "Controllers")
  expect_s3_class(sv$controllers$toto, "Controller")
})

test_that("CreateSupervisor using reservoir and diversion", {
  nodes <- loadSevernNodes()
  nodes <- rbind(nodes, data.frame(
    id     = c("54029"     , "Reservoir"         ),
    down   = c("Reservoir" , "54032"             ),
    length = c(20          , 15                  ),
    area   = c(NA, NA                 ),
    model  = c("Diversion" , "RunModel_Reservoir")
  ))
  g <- CreateGRiwrm(nodes)
  # Add Qobs for the 2 new nodes and create InputsModel
  Qobs <- matrix(data = 0, ncol = 2, nrow = length(DatesR))
  colnames(Qobs) <- c("54029", "Reservoir")
  InputsModel <- suppressWarnings(
      CreateInputsModel(g, DatesR, Precip, PotEvap, Qobs)
  )
  sv <- CreateSupervisor(InputsModel)
  expect_equal(sv$griwrm4U$id, c("54029", "Reservoir"))
})
