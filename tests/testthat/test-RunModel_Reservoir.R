test_that("Checks on GRiwrm object with Runmodel_Reservoir", {
  db <- data.frame(id = c("Reservoir", "GaugingDown"),
                   length = c(1, NA),
                   down = c("GaugingDown", NA),
                   area = c(NA, 1),
                   model = c("RunModel_Reservoir", "RunModel_GR4J"),
                   stringsAsFactors = FALSE)
  expect_error(CreateGRiwrm(db),
               regexp = "upstream node")
})

skip_on_cran()

test_that("Calibration with Runmodel_Reservoir works!", {
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
  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qobs2 = Qobs2)
  for(x in ls(e)) assign(x, get(x, e))

  InputsCrit <- CreateInputsCrit(InputsModel,
                                 ErrorCrit_KGE2,
                                 RunOptions = RunOptions,
                                 Obs = Qobs[IndPeriod_Run, ])
  expect_message(CreateInputsCrit(InputsModel,
                                  ErrorCrit_KGE2,
                                  RunOptions = RunOptions,
                                  Obs = Qobs[IndPeriod_Run, ]),
                 regexp = "No observations")

  CalibOptions <- CreateCalibOptions(InputsModel)
  expect_message(CreateCalibOptions(InputsModel), regexp = "FixedParam")

  expect_error(Calibration(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions
  ), regexp = "FixedParam")

  CalibOptions[["Dam"]]$FixedParam <- c(650E6, 1)

  OC <- Calibration(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    InputsCrit = InputsCrit,
    CalibOptions = CalibOptions
  )

  expect_equal(OC[["Dam"]]$ParamFinalR, CalibOptions[["Dam"]]$FixedParam)
  expect_gt(OC[["54001"]]$CritFinal, 0.96)
})
