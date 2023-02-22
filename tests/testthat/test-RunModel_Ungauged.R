skip_on_cran()

# data set up
nodes <- loadSevernNodes()

nodes <- nodes[!nodes$id %in% c("54002", "54057", "54095"), ]
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
  AprioriIds = c("54032" = "54001"),
  transfo = "sqrt",
  k = 0.15
)

CO <- CreateCalibOptions(InputsModel)
OC <- Calibration(InputsModel, RunOptions, IC, CO)

test_that("RunModel_Ungauged works for intermediate basin with ungauged station", {
  expect_true(all(sapply(OC, "[[", "CritFinal") > 0.96))
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
    AprioriIds = c("54032" = "54001"),
    transfo = "sqrt",
    k = 0.15
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
  transfo = "sqrt",
  k = 0.15
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

test_that("Ungauged node with diversion outside the sub-network shoudl work", {
  nodes <- loadSevernNodes()
  nodes <- nodes[!nodes$id %in% c("54002", "54057", "54029"), ]
  nodes[nodes$id == "54032", c("down", "length")] <- c(NA, NA)
  nodes$model[nodes$id == "54095"] <- "Ungauged"
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
    transfo = "sqrt",
    k = 0.15
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

})
