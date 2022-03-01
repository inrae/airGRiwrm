test_that("airGR::CreateInputsModel should work", {
  ## loading catchment data
  data(L0123001)

  ## preparation of InputsModel object
  InputsModel <- airGR::CreateInputsModel(FUN_MOD = RunModel_GR4J, DatesR = BasinObs$DatesR,
                                   Precip = BasinObs$P, PotEvap = BasinObs$E)

  expect_equal(CreateInputsModel(RunModel_GR4J,
                                 DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P,
                                 PotEvap = BasinObs$E),
               InputsModel)
  expect_equal(CreateInputsModel("RunModel_GR4J",
                                 DatesR = BasinObs$DatesR,
                                 Precip = BasinObs$P,
                                 PotEvap = BasinObs$E),
               InputsModel)
})


l <- setUpCemaNeigeData()

test_that("CemaNeige data should be in InputsModel", {
  InputsModels <- suppressWarnings(
    CreateInputsModel(l$griwrm,
                      DatesR = l$DatesR,
                      Precip = l$Precip,
                      PotEvap = l$PotEvap,
                      TempMean = l$TempMean,
                      ZInputs = l$ZInputs,
                      HypsoData = l$HypsoData)
  )
  l$DatesR <- as.data.frame(l$DatesR)
  lapply(InputsModels, function(IM) {
    lapply(c("DatesR", "Precip", "PotEvap"), function(varName) {
      expect_equal(IM[[varName]], l[[varName]][, 1])
    })
    expect_named(IM$LayerPrecip, paste0("L", seq(1, 5)))
    expect_named(IM$LayerTempMean, paste0("L", seq(1, 5)))
    expect_named(IM$LayerFracSolidPrecip, paste0("L", seq(1, 5)))
  })
})

test_that("downstream sub-catchment area should be positive", {
  l$griwrm$area[3] <- 360
  expect_error(CreateInputsModel(l$griwrm,
                                 DatesR = l$DatesR,
                                 Precip = l$Precip,
                                 PotEvap = l$PotEvap,
                                 TempMean = l$TempMean,
                                 ZInputs = l$ZInputs,
                                 HypsoData = l$HypsoData),
               regexp = "must be greater than the sum of the areas")
})

test_that("handles mix of with and without CemaNeige nodes", {
  l$griwrm[l$griwrm$id == "Down", "model"] <- "RunModel_GR4J"
  l$TempMean <- l$TempMean[, 1:2]
  l$ZInputs <- l$ZInputs[1:2]
  l$TempMean <- l$TempMean[, 1:2]
  l$HypsoData <- l$HypsoData[, 1:2]
  InputsModels <- suppressWarnings(
    CreateInputsModel(l$griwrm,
                      DatesR = l$DatesR,
                      Precip = l$Precip,
                      PotEvap = l$PotEvap,
                      TempMean = l$TempMean,
                      ZInputs = l$ZInputs,
                      HypsoData = l$HypsoData)
  )
  expect_false(inherits(InputsModels$Down, "CemaNeige"))
  expect_null(InputsModels$Down$LayerPrecip)
})

test_that("throws error on wrong column name", {
  colnames(l$Precip)[1] <- "Up0"
  expect_error(CreateInputsModel(l$griwrm,
                                 DatesR = l$DatesR,
                                 Precip = l$Precip,
                                 PotEvap = l$PotEvap,
                                 TempMean = l$TempMean,
                                 ZInputs = l$ZInputs,
                                 HypsoData = l$HypsoData),
               regexp = "column names must be included in")
  colnames(l$Precip) <- NULL
  expect_error(CreateInputsModel(l$griwrm,
                                 DatesR = l$DatesR,
                                 Precip = l$Precip,
                                 PotEvap = l$PotEvap,
                                 TempMean = l$TempMean,
                                 ZInputs = l$ZInputs,
                                 HypsoData = l$HypsoData),
               regexp = "must have column names")
})

test_that("throw error on missing column in inputs", {
  l$Precip <- l$Precip[, -1]
  expect_error(CreateInputsModel(l$griwrm,
                                 DatesR = l$DatesR,
                                 Precip = l$Precip,
                                 PotEvap = l$PotEvap,
                                 TempMean = l$TempMean,
                                 ZInputs = l$ZInputs,
                                 HypsoData = l$HypsoData),
               regexp = "Precip is missing")
})

test_that("throw error on wrong number of rows in inputs", {
  l$Precip <- l$Precip[-1, ]
  expect_error(CreateInputsModel(l$griwrm,
                                 DatesR = l$DatesR,
                                 Precip = l$Precip,
                                 PotEvap = l$PotEvap,
                                 TempMean = l$TempMean,
                                 ZInputs = l$ZInputs,
                                 HypsoData = l$HypsoData),
               regexp = "number of rows and the length of 'DatesR' must be equal")
})


test_that("throws error when missing CemaNeige data", {
  expect_error(CreateInputsModel(l$griwrm,
                                 DatesR = l$DatesR,
                                 Precip = l$Precip,
                                 PotEvap = l$PotEvap),
               regexp = "'TempMean' is missing")
})

test_that("throws error when missing Qobs on node not related to an hydrological model", {
  l$griwrm$model[1] <- NA
  expect_error(CreateInputsModel(l$griwrm,
                                 DatesR = l$DatesR,
                                 Precip = l$Precip,
                                 PotEvap = l$PotEvap),
               regexp = "'Qobs' column names must at least contain")

  expect_error(CreateInputsModel(l$griwrm,
                                 DatesR = l$DatesR,
                                 Precip = l$Precip,
                                 PotEvap = l$PotEvap,
                                 Qobs = l$Qobs[, -1]),
               regexp = "'Qobs' column names must at least contain")
})

test_that("must works with node not related to an hydrological model", {
  l$griwrm$model[1] <- NA
  IM <- suppressWarnings(CreateInputsModel(
    l$griwrm,
    DatesR = l$DatesR,
    Precip = l$Precip,
    PotEvap = l$PotEvap,
    Qobs = l$Qobs[, 1, drop = FALSE],
    TempMean = l$TempMean,
    ZInputs = l$ZInputs,
    HypsoData = l$HypsoData
  ))
  expect_equal(IM[[2]]$Qupstream[, "Up1"], l$Qobs[, "Up1"] * l$griwrm[1, "area"] * 1E3)
  expect_equal(colnames(IM[[2]]$Qupstream), c("Up1", "Up2"))
})

test_that("negative observed flow on catchment should throw error", {
  l$Qobs[100, 1] <- -99
  expect_error(CreateInputsModel(l$griwrm,
                                 DatesR = l$DatesR,
                                 Precip = l$Precip,
                                 PotEvap = l$PotEvap,
                                 Qobs = l$Qobs),
               regexp = "Negative flow found")
  l$griwrm$model[1] <- NA
  expect_s3_class(suppressWarnings(
    CreateInputsModel(
      l$griwrm,
      DatesR = l$DatesR,
      Precip = l$Precip,
      PotEvap = l$PotEvap,
      Qobs = l$Qobs,
      TempMean = l$TempMean,
      ZInputs = l$ZInputs,
      HypsoData = l$HypsoData
    )
  ),
  "GRiwrmInputsModel")
})
