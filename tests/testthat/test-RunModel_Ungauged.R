
test_that("RunModel_Ungauged works for intermediate basin with ungauged station", {
  # data set up
  e <- setupRunModel(runInputsModel = FALSE)
  for(x in ls(e)) assign(x, get(x, e))

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
    Obs = Qobs[IndPeriod_Run, np$id[np$hydrology == "Gauged"]],
    AprioriIds = c("54032" = "54001"),
    transfo = "sqrt",
    k = 0.15
  )

  CO <- CreateCalibOptions(InputsModel)
  OC <- suppressWarnings(Calibration(InputsModel, RunOptions, IC, CO))
  expect_true(all(sapply(OC, "[[", "CritFinal") > 0.96))
})
