test_that("airGR::CreateCalibOptions should works", {
  ## preparation of CalibOptions object
  CalibOptions <- airGR::CreateCalibOptions(RunModel_GR4J, FUN_CALIB = Calibration_Michel)
  expect_equal(CreateCalibOptions(RunModel_GR4J, FUN_CALIB = Calibration_Michel), CalibOptions)
  expect_equal(CreateCalibOptions("RunModel_GR4J", FUN_CALIB = Calibration_Michel), CalibOptions)
})

nodes <- loadSevernNodes()
nodes <- nodes[nodes$id %in% c("54057", "54032", "54001"), ]
nodes$model[2:3] <- "RunModel_CemaNeigeGR4J"
griwrm <- CreateGRiwrm(nodes)

e <- suppressWarnings(
  setupRunModel(griwrm = griwrm, runRunModel = FALSE, IsHyst = TRUE)
)
for(x in ls(e)) assign(x, get(x, e))

test_that("IsHyst is not handle by CreateCalibOptions.GRiwrmInputsModel", {
  expect_warning(CreateCalibOptions(InputsModel, IsHyst = TRUE))
})

