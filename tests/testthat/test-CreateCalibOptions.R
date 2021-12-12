test_that("airGR::CreateCalibOptions should works", {
  ## preparation of CalibOptions object
  CalibOptions <- airGR::CreateCalibOptions(RunModel_GR4J, FUN_CALIB = Calibration_Michel)
  expect_equal(CreateCalibOptions(RunModel_GR4J, FUN_CALIB = Calibration_Michel), CalibOptions)
  expect_equal(CreateCalibOptions("RunModel_GR4J", FUN_CALIB = Calibration_Michel), CalibOptions)
})
