skip_on_cran()

# data set up
test_that("RunModel_Ungauged should act as RunModel", {
  nodes <- loadSevernNodes()
  nodes <- nodes[nodes$id %in% c("54001", "54095"), ]
  nodes[nodes$id == "54001", c("down", "length")] <- c(NA, NA)
  nodes$model[nodes$id == "54095"] <- "Ungauged"
  g <- CreateGRiwrm(nodes)
  e <- setupRunModel(runRunModel = FALSE, griwrm = g)
  for(x in ls(e)) assign(x, get(x, e))

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

g <- CreateGRiwrm(nodes)
e <- setupRunModel(runRunModel = FALSE, griwrm = g)
for(x in ls(e)) assign(x, get(x, e))

np <- getAllNodesProperties(griwrm)

IC <- CreateInputsCrit(
  InputsModel,
  FUN_CRIT = ErrorCrit_KGE2,
  RunOptions = RunOptions,
  Obs = Qobs[IndPeriod_Run, np$id[np$RunOff & np$calibration == "Gauged"]],
)

CO <- CreateCalibOptions(InputsModel)
OC <- Calibration(InputsModel, RunOptions, IC, CO)

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
  InputsCrit = IC$`54032`,
  OutputsModel = OM$`54032`
)$CritValue

test_that("Ungauged node with gauged upstream node should works", {
  expect_equal(OC$`54032`$CritFinal, CritValue)
})

test_that("RunModel_Ungauged works with a diversion as donor (#110)", {
  nodes <- rbind(nodes,
                 data.frame(id = "54032", down = NA, length = NA, area = NA, model = "Diversion"))
  g <- CreateGRiwrm(nodes)
  Qobs2 <- matrix(0, ncol = 1, nrow = 11536)
  colnames(Qobs2) <- "54032"
  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qobs2 = Qobs2)
  for(x in ls(e)) assign(x, get(x, e))
  np <- getAllNodesProperties(griwrm)

  IC <- CreateInputsCrit(
    InputsModel,
    FUN_CRIT = ErrorCrit_KGE2,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, np$id[np$RunOff & np$calibration == "Gauged"], drop = FALSE],
  )

  CO <- CreateCalibOptions(InputsModel)
  OCdiv <- Calibration(InputsModel, RunOptions, IC, CO)
  expect_equal(OCdiv, OC)
})

# 3 nodes on one branch with ungauged node in the middle
nodes <- loadSevernNodes()
nodes <- nodes[!nodes$id %in% c("54002", "54057", "54029"), ]
nodes[nodes$id == "54032", c("down", "length")] <- c(NA, NA)
nodes$model[nodes$id == "54001"] <- "Ungauged"
g <- CreateGRiwrm(nodes)
e <- setupRunModel(griwrm = g, runRunModel = FALSE)
for(x in ls(e)) assign(x, get(x, e))
np <- getAllNodesProperties(griwrm)

IC <- CreateInputsCrit(
  InputsModel,
  FUN_CRIT = ErrorCrit_KGE2,
  RunOptions = RunOptions,
  Obs = Qobs[IndPeriod_Run, np$id[np$RunOff & np$calibration == "Gauged"], drop = FALSE],
)

CO <- CreateCalibOptions(InputsModel)
OC <- Calibration(InputsModel, RunOptions, IC, CO)
Param <- sapply(OC, "[[", "ParamFinalR")
OM <- RunModel(
  InputsModel,
  RunOptions = RunOptions,
  Param = Param
)
CritValue <- ErrorCrit_KGE2(
  InputsCrit = IC$`54032`,
  OutputsModel = OM$`54032`
)$CritValue

test_that("Ungauged node with gauged upstream node should works", {
  expect_equal(OC$`54032`$CritFinal, CritValue)
})

test_that("RunModel_Ungauged works with a diversion as upstream node (#113)", {
  nodes <- rbind(nodes,
                 data.frame(id = "54095", down = "54032", length = 100, area = NA, model = "Diversion"))
  g <- CreateGRiwrm(nodes)
  Qobs2 <- matrix(0, ncol = 1, nrow = 11536)
  colnames(Qobs2) <- "54095"
  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qobs2 = Qobs2)
  for(x in ls(e)) assign(x, get(x, e))
  np <- getAllNodesProperties(griwrm)
  OCdiv <- Calibration(InputsModel, RunOptions, IC, CO)
  expect_equal(OCdiv$`54032`$CritFinal, CritValue)
})

test_that("RunModel_Ungauged works with a diversion as upstream node (#113)", {
  nodes <- rbind(nodes,
                 data.frame(id = "54095", down = "54001", length = 100, area = NA, model = "Diversion"))
  g <- CreateGRiwrm(nodes)
  Qobs2 <- matrix(0, ncol = 1, nrow = 11536)
  colnames(Qobs2) <- "54095"
  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qobs2 = Qobs2)
  for(x in ls(e)) assign(x, get(x, e))
  np <- getAllNodesProperties(griwrm)
  OCdiv <- Calibration(InputsModel, RunOptions, IC, CO)
  expect_equal(OCdiv$`54032`$CritFinal, CritValue)
})

test_that("Ungauged node with diversion outside the sub-network should work", {
  nodes <- loadSevernNodes()
  nodes <- nodes[!nodes$id %in% c("54002", "54057", "54029"), ]
  nodes[nodes$id == "54032", c("down", "length")] <- c(NA, NA)
  nodes$model[nodes$id == "54095"] <- "Ungauged"

  # First without Diversion
  g <- CreateGRiwrm(nodes)
  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qobs2 = Qobs2)
  for(x in ls(e)) assign(x, get(x, e))
  np <- getAllNodesProperties(griwrm)

  IC <- CreateInputsCrit(
    InputsModel,
    FUN_CRIT = ErrorCrit_KGE2,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, np$id[np$RunOff & np$calibration == "Gauged"], drop = FALSE],
  )

  CO <- CreateCalibOptions(InputsModel)
  OC1 <- Calibration(InputsModel, RunOptions, IC, CO)
  Param1 <- sapply(OC1, "[[", "ParamFinalR")
  OM <- RunModel(
    InputsModel,
    RunOptions = RunOptions,
    Param = Param1
  )
  sapply(c("54001", "54032"), function(id) {
    CritValue <- ErrorCrit_KGE2(
      InputsCrit = IC[[id]],
      OutputsModel = OM[[id]]
    )$CritValue
    expect_equal(OC1[[id]]$CritFinal, CritValue)
  })

  # Second with Diversion with zero flow diverted for comparison
  nodes <- rbind(nodes,
                 data.frame(id = "54095", down = "54032", length = 100,
                            area = NA, model = "Diversion"))
  g <- CreateGRiwrm(nodes)
  Qobs2 <- matrix(0, ncol = 1, nrow = 11536)
  colnames(Qobs2) <- "54095"
  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qobs2 = Qobs2)
  for(x in ls(e)) assign(x, get(x, e))
  np <- getAllNodesProperties(griwrm)

  IC <- CreateInputsCrit(
    InputsModel,
    FUN_CRIT = ErrorCrit_KGE2,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, np$id[np$RunOff & np$calibration == "Gauged"], drop = FALSE],
  )

  CO <- CreateCalibOptions(InputsModel)
  OC2 <- Calibration(InputsModel, RunOptions, IC, CO)
  expect_equal(OC2$`54001`$CritFinal, OC1$`54001`$CritFinal)
  expect_equal(OC2$`54032`$CritFinal, OC1$`54032`$CritFinal)
  Param2 <- sapply(OC2, "[[", "ParamFinalR")
  expect_equal(Param2, Param1)
  OM <- RunModel(
    InputsModel,
    RunOptions = RunOptions,
    Param = Param2
  )
  sapply(c("54001", "54032"), function(id) {
    CritValue <- ErrorCrit_KGE2(
      InputsCrit = IC[[id]],
      OutputsModel = OM[[id]]
    )$CritValue
    expect_equal(OC1[[id]]$CritFinal, CritValue)
  })
})

test_that("Ungauged node with upstream node with diversion  should work", {
  nodes <- loadSevernNodes()
  nodes <- nodes[nodes$id %in% c("54095", "54001", "54032"), ]
  nodes[nodes$id == "54032", c("down", "length")] <- c(NA, NA)
  nodes$model[nodes$id == "54001"] <- "Ungauged"
  nodes$down[nodes$id == "54095"] <- "P"
  nodes <- rbind(nodes,
                 data.frame(id = "P", down = "54001", length = 0, area = NA, model = "RunModel_Lag"),
                 data.frame(id = c("54095", "P", "54001"),
                            down = NA,
                            length = NA,
                            area = NA,
                            model = "Diversion"))
  g <- CreateGRiwrm(nodes)
  Qobs2 <- matrix(0, ncol = length(g$id[g$model == "Diversion"]), nrow = 11536)
  colnames(Qobs2) <- g$id[g$model == "Diversion"]
  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qobs2 = Qobs2)
  for(x in ls(e)) assign(x, get(x, e))
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
  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qobs2 = Qobs2)
  for(x in ls(e)) assign(x, get(x, e))
  np <- getAllNodesProperties(g)

  IC <- CreateInputsCrit(
    InputsModel,
    FUN_CRIT = ErrorCrit_KGE2,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, np$id[np$RunOff & np$calibration == "Gauged"], drop = FALSE],
  )

  CO <- CreateCalibOptions(InputsModel)
  OC_Div <- Calibration(InputsModel, RunOptions, IC, CO)
  Param_Div <- sapply(OC_Div, "[[", "ParamFinalR")
  expect_equal(Param_Div, Param)
  expect_equal(OC$`54032`$CritFinal, OC_Div$`54032`$CritFinal)
})
