test_that("airGR::CreateCalibOptions should works", {
  ## preparation of CalibOptions object
  CalibOptions <- airGR::CreateCalibOptions(RunModel_GR4J, FUN_CALIB = Calibration_Michel)
  expect_equal(CreateCalibOptions(RunModel_GR4J, FUN_CALIB = Calibration_Michel), CalibOptions)
  expect_equal(CreateCalibOptions("RunModel_GR4J", FUN_CALIB = Calibration_Michel), CalibOptions)
})

nodes <- loadSevernNodes()
nodes <- nodes[nodes$id %in% c("54057", "54032", "54001"), ]
nodes$model[2:3] <- "RunModel_CemaNeigeGR4J"
# 54057 GR4J + Lag / 54032 GR4J + Neige + Lag / 54001 GR4J + Neige
griwrm <- CreateGRiwrm(nodes)

e <- suppressWarnings(
  setupRunModel(griwrm = griwrm, runRunModel = FALSE, IsHyst = TRUE)
)
for(x in ls(e)) assign(x, get(x, e))

test_that("IsHyst is not handle by CreateCalibOptions.GRiwrmInputsModel", {
  expect_warning(CreateCalibOptions(InputsModel, IsHyst = TRUE))
})

test_that("`FixedParam` should be a character or a list", {
  expect_error(CreateCalibOptions(InputsModel, FixedParam = rep("3",4)))
  expect_error(CreateCalibOptions(InputsModel, FixedParam = list(`54057`= "wrong")))
})

FixedParam <- c(NA,   # C      (lag)
                NA,   # X1     (GR4J)
                NA,   # X2     (GR4J)
                NA,   # X3     (GR4J)
                NA,   # X4     (GR4J)
                0.25, # cT     (CemaNeige)
                NA,   # Kf     (CemaNeige)
                10,   # Gacc   (CemaNeige)
                NA)  # Gseuil (CemaNeige)

test_that("List FixedParam should have correct ids", {
  expect_error(CreateCalibOptions(InputsModel,
                                  FixedParam = list(`54057` = FixedParam,
                                                    `54002` = FixedParam)),
               regexp = "54002")
})

test_that("FixedParam works on various models", {

  CO <- CreateCalibOptions(InputsModel, FixedParam = FixedParam)
  expect_equal_map(lapply(CO, "[[", "FixedParam"),
                   list(`54057` = as.logical(FixedParam[1:5]),
                        `54032` = FixedParam,
                        `54001` = FixedParam[2:9]))
  CO2 <- CreateCalibOptions(InputsModel,
                            FixedParam = list(`54057` = c(NA, 1:4),
                                              `*` = FixedParam))
  expect_equal_map(lapply(CO2, "[[", "FixedParam"),
               list(`54057` = c(NA, 1:4),
                    `54032` = FixedParam,
                    `54001` = FixedParam[2:9]))
})
