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

test_that("FixedParam works on various models", {
  # 54057 GR4J + Lag / 54032 GR4J + Neige + Lag / 54001 GR4J + Neige
  FixedParam <- c(NA,   # C      (lag)
                  NA,   # X1     (GR4J)
                  NA,   # X2     (GR4J)
                  NA,   # X3     (GR4J)
                  NA,   # X4     (GR4J)
                  0.25, # cT     (CemaNeige)
                  NA,   # Kf     (CemaNeige)
                  10,   # Gacc   (CemaNeige)
                  NA)  # Gseuil (CemaNeige)
  CO <- CreateCalibOptions(InputsModel, FixedParam = FixedParam)
  expect_equal(lapply(CO, "[[", "FixedParam"),
               list(`54057` = FixedParam[1:5],
                    `54032` = FixedParam,
                    `54001` = FixedParam[2:9]))

})
