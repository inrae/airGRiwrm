skip_on_cran()

# data set up
test_that("RunModel_Ungauged should act as RunModel", {
  nodes <- loadSevernNodes()
  nodes <- nodes[nodes$id %in% c("54001", "54095"), ]
  nodes[nodes$id == "54001", c("down", "length")] <- c(NA, NA)
  nodes$model[nodes$id == "54095"] <- "Ungauged"
  g <- CreateGRiwrm(nodes)
  e <- setupRunModel(runRunModel = FALSE, griwrm = g)
  for (x in ls(e)) assign(x, get(x, e))

  Param <- ParamMichel["54001"]
  Param[["54095"]] <-
    ParamMichel[["54001"]][InputsModel[["54095"]]$model$indexParamUngauged]
  donorArea <- tail(InputsModel[["54001"]]$BasinAreas, 1)
  X4 <- Param[["54001"]][InputsModel[["54001"]]$model$iX4]
  Param[["54095"]][InputsModel[["54095"]]$model$iX4] <-
    max(
      X4 * (tail(InputsModel[["54095"]]$BasinAreas, 1) / donorArea) ^ 0.3,
      0.5
    )
  OM <- RunModel(InputsModel, RunOptions = RunOptions, Param = Param)
  attr(RunOptions[["54001"]], "GRiwrmRunOptions") <- RunOptions
  OMU <- RunModel_Ungauged(InputsModel,
                           RunOptions = RunOptions[["54001"]],
                           Param = Param[["54001"]],
                           output.all = TRUE)
  expect_equal(OMU, OM)
})

# data set up
nodes <- loadSevernNodes()

nodes <- nodes[nodes$id %in% c("54001", "54029", "54032"), ]
nodes[nodes$id == "54032", c("down", "length")] <- c(NA, NA)
nodes$model[nodes$id == "54029"] <- "Ungauged"

e <- runCalibration(nodes)
for(x in ls(e)) assign(x, get(x, e))
OC <- OutputsCalib

test_that("RunModel_Ungauged works for intermediate basin with ungauged station", {
  expect_true(all(sapply(OC, "[[", "CritFinal") > 0.95))
})

Param <- sapply(OC, "[[", "ParamFinalR")
OM <- RunModel(
  InputsModel,
  RunOptions = RunOptions,
  Param = Param
)
CritValue <- ErrorCrit_KGE2(
  InputsCrit = InputsCrit$`54032`,
  OutputsModel = OM$`54032`
)$CritValue

test_that("Ungauged node with gauged upstream node should works", {
  expect_equal(OC$`54032`$CritFinal, CritValue)
})

test_that("RunModel_Ungauged works with a diversion as donor (#110)", {
  nodes <- rbind(nodes,
                 data.frame(id = "54032", down = NA, length = NA, area = NA, model = "Diversion"))
  Qobs2 <- matrix(0, ncol = 1, nrow = 11536)
  colnames(Qobs2) <- "54032"
  e <- runCalibration(nodes, Qobs2 = Qobs2)
  for (x in ls(e)) assign(x, get(x, e))
  OCdiv <- OutputsCalib
  expect_equal(OCdiv, OC)
})

# 3 nodes on one branch with ungauged node in the middle
nodes <- loadSevernNodes()
nodes <- nodes[!nodes$id %in% c("54002", "54057", "54029"), ]
nodes[nodes$id == "54032", c("down", "length")] <- c(NA, NA)
nodes$model[nodes$id == "54001"] <- "Ungauged"
e <- runCalibration(nodes)
for(x in ls(e)) assign(x, get(x, e))
np <- getAllNodesProperties(griwrm)

OC_ref <- OutputsCalib
ParamRef <- Param
OM <- RunModel(
  InputsModel,
  RunOptions = RunOptions,
  Param = ParamRef
)
CritValue <- ErrorCrit_KGE2(
  InputsCrit = InputsCrit$`54032`,
  OutputsModel = OM$`54032`
)$CritValue

test_that("Ungauged node with gauged upstream node should works", {
  expect_equal(OC$`54032`$CritFinal, CritValue, tolerance = 1E3)
})

test_that("RunModel_Ungauged works with a diversion as upstream node (#113)", {
  nodes <- rbind(nodes,
                 data.frame(id = "54095", down = "54032", length = 100, area = NA, model = "Diversion"))
  Qobs2 <- matrix(0, ncol = 1, nrow = 11536)
  colnames(Qobs2) <- "54095"
  e <- runCalibration(nodes, Qobs2 = Qobs2)
  for (x in ls(e)) assign(x, get(x, e))
  expect_equal(OutputsCalib$`54032`$CritFinal, CritValue)
})

test_that("RunModel_Ungauged works with a diversion as upstream node (#113)", {
  nodes <- rbind(nodes,
                 data.frame(id = "54095", down = "54001", length = 100, area = NA, model = "Diversion"))
  Qobs2 <- matrix(0, ncol = 1, nrow = 11536)
  colnames(Qobs2) <- "54095"
  e <- runCalibration(nodes, Qobs2 = Qobs2)
  for (x in ls(e)) assign(x, get(x, e))
  expect_equal(OutputsCalib$`54032`$CritFinal, CritValue)
})

test_that("Ungauged node with diversion outside the sub-network should work", {
  nodes <- loadSevernNodes()
  nodes <- nodes[!nodes$id %in% c("54002", "54057", "54029"), ]
  nodes[nodes$id == "54032", c("down", "length")] <- c(NA, NA)
  nodes$model[nodes$id == "54095"] <- "Ungauged"

  # First without Diversion
  e <- runCalibration(nodes, Qobs2 = Qobs2)
  for (x in ls(e)) assign(x, get(x, e))
  OC1 <- OutputsCalib
  Param1 <- Param
  OM1 <- RunModel(
    InputsModel,
    RunOptions = RunOptions,
    Param = Param1
  )
  sapply(c("54001", "54032"), function(id) {
    CritValue <- ErrorCrit_KGE2(
      InputsCrit = InputsCrit[[id]],
      OutputsModel = OM1[[id]]
    )$CritValue
    expect_equal(OC1[[id]]$CritFinal, CritValue)
  })

  # Second with Diversion with zero flow diverted for comparison
  nodes <- rbind(nodes,
                 data.frame(id = "54095", down = "54032", length = 100,
                            area = NA, model = "Diversion"))
  Qobs2 <- matrix(0, ncol = 1, nrow = 11536)
  colnames(Qobs2) <- "54095"
  e <- runCalibration(nodes, Qobs2 = Qobs2)
  for (x in ls(e)) assign(x, get(x, e))
  OC2 <- OutputsCalib
  expect_equal(OC2$`54001`$CritFinal, OC1$`54001`$CritFinal)
  expect_equal(OC2$`54032`$CritFinal, OC1$`54032`$CritFinal)
  Param2 <- Param
  expect_equal(Param2, Param1)
  OM2 <- RunModel(
    InputsModel,
    RunOptions = RunOptions,
    Param = Param2
  )
  sapply(c("54001", "54032"), function(id) {
    CritValue <- ErrorCrit_KGE2(
      InputsCrit = InputsCrit[[id]],
      OutputsModel = OM2[[id]]
    )$CritValue
    expect_equal(OC1[[id]]$CritFinal, CritValue)
  })
})

test_that("Ungauged node with upstream node with diversion should work", {
  nodes <- loadSevernNodes()
  nodes <- nodes[nodes$id %in% c("54095", "54001", "54032"), ]
  nodes[nodes$id == "54032", c("down", "length")] <- c(NA, NA)
  nodes$model[nodes$id == "54001"] <- "Ungauged"
  nodes$down[nodes$id == "54095"] <- "P"
  nodes$length[nodes$id == "54095"] <- 0
  nodes <- rbind(nodes,
                 data.frame(id = "P", down = "54001", length = 42, area = NA, model = "RunModel_Lag"),
                 data.frame(id = c("54095", "P", "54001", "54032"),
                            down = NA,
                            length = NA,
                            area = NA,
                            model = "Diversion"))
  g <- CreateGRiwrm(nodes)
  Qobs2 <- matrix(0, ncol = length(g$id[g$model == "Diversion"]), nrow = 11536)
  colnames(Qobs2) <- g$id[g$model == "Diversion"]
  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qobs2 = Qobs2)
  for (x in ls(e)) assign(x, get(x, e))

  ParamRef[["P"]] <- 1
  OM <- RunModel(InputsModel,
                 RunOptions = RunOptions,
                 Param = ParamRef)

  l <- updateParameters4Ungauged(GaugedId = "54032",
                                 InputsModel = InputsModel,
                                 RunOptions = RunOptions,
                                 CalibOptions = CO,
                                 OutputsModel = OM,
                                 useUpstreamQsim = TRUE)
  g_reduced <- attr(l$InputsModel, "GRiwrm")

  expect_true(!any(g_reduced$id == "P" & !is.na(g_reduced$model) & g_reduced$model == "Diversion"))
  expect_true(any(g_reduced$id == "54001" & !is.na(g_reduced$model) & g_reduced$model == "Diversion"))
  expect_true(any(g_reduced$id == "54032" & !is.na(g_reduced$model) & g_reduced$model == "Diversion"))

  np <- getAllNodesProperties(g)

  IC <- CreateInputsCrit(
    InputsModel,
    FUN_CRIT = ErrorCrit_KGE2,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, np$id[np$RunOff & np$calibration == "Gauged"], drop = FALSE],
  )

  CO <- CreateCalibOptions(InputsModel)
  CO[["P"]]$FixedParam = 1
  OC_Lag <- Calibration(InputsModel, RunOptions, IC, CO)
  Param_Lag <- sapply(OC_Lag, "[[", "ParamFinalR")
  expect_equal(Param_Lag, ParamRef[names(Param_Lag)])
})

test_that("Donor node with diversion should work", {
  nodes <- rbind(nodes,
                 data.frame(id = "54032",
                            down = NA,
                            length = NA,
                            area = NA,
                            model = "Diversion"))
  g <- CreateGRiwrm(nodes)
  Qobs2 <- matrix(0, ncol = length(g$id[g$model == "Diversion"]), nrow = 11536)
  colnames(Qobs2) <- g$id[g$model == "Diversion"]
  e <- runCalibration(nodes, Qobs2 = Qobs2)
  for (x in ls(e)) assign(x, get(x, e))
  expect_equal(OC_ref$`54032`$CritFinal, OutputsCalib$`54032`$CritFinal, tolerance = 1E-3)
})

test_that("Cemaneige with hysteresis works",  {
  nodes <- loadSevernNodes()
  nodes <- nodes[nodes$id %in% c("54057", "54032", "54001"), ]
  nodes$model <- "RunModel_CemaNeigeGR4J"
  nodes$model[nodes$id != 54057] <- "Ungauged"
  griwrm <- CreateGRiwrm(nodes)

  # # The custom ErrorCrit function !!!
  ErrorCrit_KGE3 <- function(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE) {
    OutputsCritQ <- suppressMessages(
      ErrorCrit_KGE2(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)
    )
    InputsCrit$Obs <- InputsCrit$SCA #a adapter
    OutputsModel$Qsim <- OutputsModel$CemaNeigeLayers[[1]]$Gratio #a adapter
    OutputsCritSCA <- suppressMessages(
      ErrorCrit_KGE2(InputsCrit, OutputsModel, warnings = TRUE, verbose = TRUE)
    )
    OutputsCritQ$CritValue <-
      (OutputsCritQ$CritValue + OutputsCritSCA$CritValue) / 2
    OutputsCritQ$CritName <- "(0.5 * KGE2[Q] + 0.5 * KGE2[SCA])"
    return(OutputsCritQ)
  }
  class(ErrorCrit_KGE3) <- c("FUN_CRIT", class(ErrorCrit_KGE3))

  e <- suppressWarnings(
    setupRunModel(griwrm = griwrm, runRunModel = FALSE, IsHyst = TRUE)
  )
  for (x in ls(e)) assign(x, get(x, e))

  expect_true(all(sapply(InputsModel, function(x) x$model$hasX4)))

  np <- getAllNodesProperties(griwrm)
  InputsCrit <- CreateInputsCrit(
    InputsModel,
    FUN_CRIT = ErrorCrit_KGE3,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, np$id[np$RunOff & np$calibration == "Gauged"], drop = FALSE],
  )
  InputsCrit$`54057`$SCA <- runif(length(IndPeriod_Run)) # Fake SCA
  CalibOptions <- CreateCalibOptions(InputsModel)
  CO <- lapply(CalibOptions, function(x) {
    x$StartParamList <- matrix(
      c(0.605,  320.596,   -0.042,   37.991,    2.221,    0.705,    6.764,   85.000,    0.850),
      nrow = 1)
    x$StartParamDistrib <- NULL
    x
  })
  class(CO) <- class(CalibOptions)
  e <- suppressWarnings(
    runCalibration(nodes, InputsCrit = InputsCrit, CalibOptions = CO, IsHyst = TRUE)
  )
  for (x in ls(e)) assign(x, get(x, e))
  expect_equal(sapply(Param, length),
               c("54057" = 9, "54032" = 9, "54001" = 8))
})

test_that("Ungauged node with derivation to reservoir should work", {
  testDerivedUngauged(FALSE)
})

test_that("Ungauged node with donor by derivation through reservoir should work", {
  testDerivedUngauged(TRUE)
})
