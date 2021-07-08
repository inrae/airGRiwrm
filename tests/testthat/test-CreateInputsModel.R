context("CreateInputsModel")

l <- setUpCemaNeigeData()

test_that("CemaNeige data should be in InputsModel", {
  InputsModels <- CreateInputsModel(l$griwrm,
                                    DatesR = l$DatesR,
                                    Precip = l$Precip,
                                    PotEvap = l$PotEvap,
                                    TempMean = l$TempMean,
                                    ZInputs = l$ZInputs,
                                    HypsoData = l$HypsoData,
                                    Qobs = l$Qobs)
  l$DatesR <- as.data.frame(l$DatesR)
  lapply(InputsModels, function(IM) {
    lapply(c("DatesR", "Precip", "PotEvap"), function(varName) {
      expect_equal(IM[[varName]], l[[varName]][,1])
    })
    expect_named(IM$LayerPrecip, paste0("L", seq(1,5)))
    expect_named(IM$LayerTempMean, paste0("L", seq(1,5)))
    expect_named(IM$LayerFracSolidPrecip, paste0("L", seq(1,5)))
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
                                 HypsoData = l$HypsoData,
                                 Qobs = l$Qobs))
})

test_that("handles mix of with and without CemaNeige nodes", {
  l$griwrm[l$griwrm$id == "Down", "model"] <- "RunModel_GR4J"
  l$TempMean <- l$TempMean[,1:2]
  l$ZInputs <- l$ZInputs[1:2]
  l$TempMean <- l$TempMean[,1:2]
  l$HypsoData <- l$HypsoData[,1:2]
  InputsModels <- CreateInputsModel(l$griwrm,
                                    DatesR = l$DatesR,
                                    Precip = l$Precip,
                                    PotEvap = l$PotEvap,
                                    TempMean = l$TempMean,
                                    ZInputs = l$ZInputs,
                                    HypsoData = l$HypsoData,
                                    Qobs = l$Qobs)
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
                                 HypsoData = l$HypsoData,
                                 Qobs = l$Qobs))
  colnames(l$Precip) <- NULL
  expect_error(CreateInputsModel(l$griwrm,
                                 DatesR = l$DatesR,
                                 Precip = l$Precip,
                                 PotEvap = l$PotEvap,
                                 TempMean = l$TempMean,
                                 ZInputs = l$ZInputs,
                                 HypsoData = l$HypsoData,
                                 Qobs = l$Qobs))
})

test_that("throws error when missing CemaNeige data", {
  expect_error(CreateInputsModel(l$griwrm,
                                 DatesR = l$DatesR,
                                 Precip = l$Precip,
                                 PotEvap = l$PotEvap,
                                 Qobs = l$Qobs))
})
