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

test_that("throws error when missing Qobs on node Direct Injection node", {
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

test_that("Qobs on hydrological nodes should throw a warning", {
  expect_warning(CreateInputsModel(l$griwrm,
                                 DatesR = l$DatesR,
                                 Precip = l$Precip,
                                 PotEvap = l$PotEvap,
                                 Qobs = l$Qobs,
                                 TempMean = l$TempMean,
                                 ZInputs = l$ZInputs,
                                 HypsoData = l$HypsoData),
               regexp = "columns in 'Qobs' are ignored since they don't match with")
  l$griwrm$model[1] <- NA
  expect_s3_class(suppressWarnings(
    CreateInputsModel(
      l$griwrm,
      DatesR = l$DatesR,
      Precip = l$Precip,
      PotEvap = l$PotEvap,
      Qobs = l$Qobs[,1, drop = F],
      TempMean = l$TempMean,
      ZInputs = l$ZInputs,
      HypsoData = l$HypsoData
    )
  ),
  "GRiwrmInputsModel")
})

# data set up
e <- setupRunModel(runInputsModel = FALSE)
# variables are copied from environment 'e' to the current environment
# https://stackoverflow.com/questions/9965577/r-copy-move-one-environment-to-another
for(x in ls(e)) assign(x, get(x, e))

test_that("Ungauged node should inherits its FUN_MOD from the downstream gauged node", {

  nodes$model[nodes$id == "54032"] <- "Ungauged"
  griwrmV05 <- CreateGRiwrm(nodes)
  IM <- suppressWarnings(
    CreateInputsModel(griwrmV05, DatesR, Precip, PotEvap)
  )
  expect_equal(IM[["54032"]]$FUN_MOD, "RunModel_GR4J")
})

test_that("Network with Diversion works", {
  n_div <- rbind(nodes, data.frame(id = "54029",
                                   down = "54002",
                                   length = 20,
                                   model = "Diversion",
                                   area = NA))
  g <- CreateGRiwrm(n_div)
  Qobs = matrix(-1, nrow = length(DatesR), ncol = 1)
  colnames(Qobs) = "54029"
  IM <- suppressWarnings(
    CreateInputsModel(g, DatesR, Precip, PotEvap, Qobs)
  )
  expect_equal(IM[["54032"]]$UpstreamNodes, c("54001", "54029"))
  expect_equal(IM[["54032"]]$UpstreamVarQ , c("54001" = "Qsim_m3", "54029" = "Qsim_m3"))
  expect_equal(IM[["54002"]]$UpstreamNodes, "54029")
  expect_equal(IM[["54002"]]$UpstreamIsModeled  , c("54029" = TRUE))
  expect_equal(IM[["54002"]]$UpstreamVarQ , c("54029" = "Qdiv_m3"))
  expect_equivalent(IM$`54029`$Qmin, matrix(0, nrow = length(DatesR), ncol = 1))
})

test_that("Diversion node: checks about 'Qmin'", {
  n_div <- rbind(nodes,
                 data.frame(id = "54029", down = "54002", length = 50, area = NA, model = "Diversion"))
  g <- CreateGRiwrm(n_div)
  Qobs = matrix(-1, nrow = length(DatesR), ncol = 1)
  colnames(Qobs) = "54029"
  expect_warning(CreateInputsModel(g, DatesR, Precip, PotEvap, Qobs = Qobs),
                 regexp = "Zero values")
  Qmin <- -Qobs
  IM <- CreateInputsModel(g, DatesR, Precip, PotEvap, Qobs = Qobs, Qmin = Qmin)
  expect_equivalent(IM$`54029`$Qmin, Qmin)
  QminNA <- Qmin
  QminNA[1] <- NA
  expect_error(CreateInputsModel(g, DatesR, Precip, PotEvap, Qobs = Qobs, Qmin = QminNA),
               regexp = "NA")
  QminBadCol <- Qmin
  colnames(QminBadCol) = "54002"
  expect_error(CreateInputsModel(g, DatesR, Precip, PotEvap, Qobs = Qobs, Qmin = QminBadCol),
               regexp = "columns that does not match with IDs of Diversion nodes")
})

test_that("Node with upstream nodes having area = NA should return correct BasinsAreas", {
  nodes <- loadSevernNodes()
  # Reduce the network
  nodes <- nodes[nodes$id %in% c("54095", "54001"), ]
  nodes$down[nodes$id == "54001"] <- NA
  nodes$length[nodes$id == "54001"] <- NA
  # Insert a dam downstream the location the gauging station 54095
  # The dam is a direct injection node
  nodes$down[nodes$id == "54095"] <- "Dam"
  nodes$length[nodes$id == "54095"] <- 0
  nodes <- rbind(nodes,
                 data.frame(id = "Dam",
                            down = "54001",
                            length = 42,
                            area = NA,
                            model = "RunModel_Reservoir"))
  g <- CreateGRiwrm(nodes)
  Qobs2 <- data.frame(
    Dam = rep(0,11536)
  )
  e <- setupRunModel(griwrm = g, runInputsModel = FALSE, Qobs2 = Qobs2)
  for(x in ls(e)) assign(x, get(x, e))
  InputsModel <-
    suppressWarnings(CreateInputsModel(g, DatesR, Precip, PotEvap, Qobs = Qobs2))
  expect_equal(sum(InputsModel$`54001`$BasinAreas),
               g$area[g$id == "54001"])
})
