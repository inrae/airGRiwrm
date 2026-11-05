test_that("airGR::CreateInputsCrit should works", {
  ## loading catchment data
  data(L0123001)

  ## preparation of InputsModel object
  InputsModel <- CreateInputsModel(
    RunModel_GR4J,
    DatesR = BasinObs$DatesR,
    Precip = BasinObs$P,
    PotEvap = BasinObs$E
  )

  ## calibration period selection
  Ind_Run <- seq(
    which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1990-01-01"),
    which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1999-12-31")
  )
  Ind_WarmUp <- seq(
    which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1989-01-01"),
    which(format(BasinObs$DatesR, format = "%Y-%m-%d") == "1989-12-31")
  )

  ## preparation of RunOptions object
  RunOptions <- CreateRunOptions(
    RunModel_GR4J,
    InputsModel = InputsModel,
    IndPeriod_Run = Ind_Run,
    IndPeriod_WarmUp = Ind_WarmUp
  )

  ## calibration criterion: preparation of the InputsCrit object
  InputsCrit <- airGR::CreateInputsCrit(
    ErrorCrit_NSE,
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    Obs = BasinObs$Qmm[Ind_Run]
  )
  expect_equal(
    CreateInputsCrit(
      ErrorCrit_NSE,
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = BasinObs$Qmm[Ind_Run]
    ),
    InputsCrit
  )
  expect_equal(
    CreateInputsCrit(
      "ErrorCrit_NSE",
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = BasinObs$Qmm[Ind_Run]
    ),
    InputsCrit
  )
})

# data set up
e <- setupRunModel()
# variables are copied from environment 'e' to the current environment
# https://stackoverflow.com/questions/9965577/r-copy-move-one-environment-to-another
for (x in ls(e)) {
  assign(x, get(x, e))
}
rm(e)

context("CreateInputsCrit.GRiwrmInputsModel")

test_that("Wrong argument class should throw error", {
  expect_error(CreateInputsCrit(
    InputsModel = InputsModel[[1]],
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, ]
  ))
  expect_error(
    CreateInputsCrit.GRiwrmInputsModel(
      InputsModel = InputsModel[[1]],
      RunOptions = RunOptions,
      Obs = Qobs[IndPeriod_Run, ]
    ),
    regexp = "GRiwrmInputsModel"
  )

  expect_error(
    CreateInputsCrit(
      InputsModel = InputsModel,
      RunOptions = RunOptions[[1]],
      Obs = Qobs[IndPeriod_Run, ]
    ),
    regexp = "GRiwrmRunOptions"
  )
  expect_error(
    CreateInputsCrit(
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = 1
    ),
    regexp = "matrix or data.frame"
  )
})

test_that("Using Lavenne criterion with 'weight' should throw error", {
  expect_error(
    CreateInputsCrit(
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = Qobs[IndPeriod_Run, ],
      AprioriIds = c("54057" = "54032", "54032" = "54001", "54001" = "54095"),
      Weights = c(0.85)
    ),
    regexp = "Lavenne"
  )
})

AprioriIds <- c("54057" = "54032", "54032" = "54001", "54001" = "54095")
IC <- CreateInputsCrit(
  InputsModel = InputsModel,
  RunOptions = RunOptions,
  Obs = Qobs[IndPeriod_Run, ],
  AprioriIds = AprioriIds,
  transfo = "sqrt"
)

test_that("Lavenne criterion is OK", {
  expect_s3_class(IC[["54057"]], "InputsCritLavenneFunction")
  Lavenne_FUN <- attr(IC[["54057"]], "Lavenne_FUN")
  IC57 <- Lavenne_FUN(ParamMichel[["54032"]], 0.9)
  expect_s3_class(IC57, "InputsCrit")
  expect_s3_class(IC57, "Compo")
})

test_that("Lavenne embedded data is correct #57", {
  lapply(names(AprioriIds), function(id) {
    p <- as.list(environment(attr(IC[[id]], "Lavenne_FUN")))
    expect_equal(id, p$InputsModel$id)
  })
})

test_that("Lavenne criterion: wrong sub-catchment order should throw error", {
  expect_error(
    CreateInputsCrit(
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = Qobs[IndPeriod_Run, ],
      AprioriIds = c("54057" = "54032", "54032" = "54001", "54095" = "54029"),
      transfo = "sqrt"
    ),
    regexp = "is not calibrated before the node"
  )
})

test_that("Lavenne criterion: not upstream a priori nodes are allow if processed before #156", {
  IC156 <- CreateInputsCrit(
    InputsModel = InputsModel,
    FUN_CRIT = ErrorCrit_KGE2,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, ],
    AprioriIds = c(
      "54057" = "54032",
      "54032" = "54001",
      "54029" = "54095"
    ),
    transfo = "sqrt"
  )
  expect_equal(attr(IC156$`54029`, "AprioriIds"), "54095")
  e <- runCalibration(
    nodes = nodes,
    Qinf = NULL,
    InputsCrit = IC156,
    CalibOptions = NULL,
    runRunModel = FALSE,
    IsHyst = FALSE
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  # 54029 not processed as ungauged
  expect_false(is.null(OutputsCalib$`54029`$CritFinal))
})

test_that("Lavenne criterion: redefined calibration order works #157", {
  nodes$donor <- nodes$id
  nodes$donor[nodes$id == "54095"] <- "54029"
  e <- setupRunModel(runRunModel = FALSE, nodes = nodes)
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  IC157 <- CreateInputsCrit(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, ],
    AprioriIds = c(
      "54057" = "54032",
      "54032" = "54001",
      "54095" = "54029"
    ),
    transfo = "sqrt"
  )
  e <- runCalibration(
    nodes = nodes,
    Qinf = NULL,
    InputsCrit = IC157,
    CalibOptions = NULL,
    FUN_CRIT = ErrorCrit_KGE2,
    runRunModel = FALSE,
    IsHyst = FALSE
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  expect_false(is.null(OutputsCalib$`54095`$CritFinal))
})

test_that("Lavenne criterion: current node and a priori node must use the same model", {
  InputsModel[["54032"]]$FUN_MOD <- RunModel_GR6J
  expect_error(
    CreateInputsCrit(
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = Qobs[IndPeriod_Run, ],
      AprioriIds = AprioriIds,
      transfo = "sqrt"
    ),
    regexp = "must use the same hydrological model"
  )
})

test_that("Ungauged node as Apriori node should throw an error", {
  nodes$model[nodes$id == "54001"] <- "Ungauged"
  griwrm <- CreateGRiwrm(nodes)
  InputsModel <- CreateInputsModel(griwrm, DatesR, Precip, PotEvap)
  expect_error(
    CreateInputsCrit(
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = Qobs[IndPeriod_Run, ],
      AprioriIds = c("54057" = "54032", "54032" = "54001", "54001" = "54029"),
      transfo = "sqrt"
    ),
    regexp = "\"54001\" is ungauged"
  )
})

test_that("Lavenne criterion: multiple Apriori nodes works", {
  ICmult <- CreateInputsCrit(
    InputsModel = InputsModel,
    FUN_CRIT = ErrorCrit_KGE2,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, ],
    AprioriIds = list(
      "54057" = c("54032", "54002"),
      "54032" = c("54001", "54029"),
      "54001" = "54095"
    ),
    transfo = "sqrt"
  )
  expect_equal(attr(ICmult$`54057`, "AprioriIds"), c("54032", "54002"))
  e <- runCalibration(
    nodes = nodes,
    Qinf = NULL,
    InputsCrit = ICmult,
    CalibOptions = NULL,
    runRunModel = FALSE,
    IsHyst = FALSE
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
})

test_that("getDefaultAprioriIds works", {
  expect_equal(
    getDefaultAprioriIds(InputsModel),
    list(
      "54001" = "54095",
      "54032" = c("54029", "54001"),
      "54057" = c("54002", "54032")
    )
  )
})

test_that("getDefaultAprioriIds works with reservoirs", {
  g <- CreateGRiwrm(n_rsrvr)

  e <- setupRunModel(griwrm = g, runRunModel = FALSE, Qinf = Qinf_rsrvr)
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  expect_equal(getDefaultAprioriIds(InputsModel), list("54001" = "54095"))
})

test_that("Lavenne criterion: user-defined a priori parameter is accepted #61", {
  userParam54057 <- c(1, 50, -1, 3, 2)
  IC61 <- CreateInputsCrit(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, ],
    AprioriIds = list(
      "54057" = userParam54057,
      "54032" = "54001",
      "54001" = "54095"
    ),
    transfo = "sqrt"
  )
  # The node with a user-defined a priori parameter keeps the Lavenne criterion
  expect_s3_class(IC61[["54057"]], "InputsCritLavenneFunction")
  # The user-defined a priori parameter is stored on the node's InputsCrit
  expect_equal(attr(IC61[["54057"]], "AprioriIds"), userParam54057)
  # Node-based a priori parameters are unchanged
  expect_equal(attr(IC61[["54032"]], "AprioriIds"), "54001")
})

test_that("Lavenne criterion: user-defined a priori parameter is used in the criterion #61", {
  # A user-defined a priori parameter, different from the reference parameters
  userParam54057 <- c(0.7, 60, -1, 1.2, 2.8)
  IC61 <- CreateInputsCrit(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, ],
    AprioriIds = list(
      "54057" = userParam54057,
      "54032" = "54001",
      "54001" = "54095"
    ),
    transfo = "sqrt"
  )
  IM <- InputsModel[["54057"]]
  RO <- RunOptions[["54057"]]
  OM_sim <- RunModel(IM, RO, ParamMichel[["54057"]])
  # A positive a priori criterion so that the regularization (GAPX) term,
  # weighted by k * max(0, AprCrit) in airGR::CreateInputsCrit_Lavenne, is active
  AprCrit <- 0.9
  # Composed criterion built by the package from the user-defined a priori parameter
  IC_compo <- attr(IC61[["54057"]], "Lavenne_FUN")(userParam54057, AprCrit)
  Crit_user <- ErrorCrit(IC_compo, OM_sim)$CritValue
  # Hand computation: airGR::CreateInputsCrit_Lavenne called with the same a priori parameter
  IC_hand <- airGR::CreateInputsCrit_Lavenne(
    FUN_CRIT = ErrorCrit_KGE2,
    InputsModel = IM,
    RunOptions = RO,
    Obs = Qobs[IndPeriod_Run, "54057", drop = TRUE],
    AprParamR = userParam54057,
    AprCrit = AprCrit,
    k = 0.15,
    transfo = "sqrt"
  )
  Crit_hand <- ErrorCrit(IC_hand, OM_sim)$CritValue
  # The user-defined a priori parameter is effectively used in the criterion
  expect_equal(Crit_user, Crit_hand)
  # The criterion output changes when the a priori parameter changes
  Crit_other <- ErrorCrit(
    attr(IC61[["54057"]], "Lavenne_FUN")(ParamMichel[["54057"]], AprCrit),
    OM_sim
  )$CritValue
  expect_false(isTRUE(all.equal(Crit_user, Crit_other)))
})

test_that("Lavenne criterion: user-defined a priori parameter is used in calibration #61", {
  # The user provides the reference (calibrated) parameters of the node as a priori
  userParam54057 <- ParamMichel[["54057"]]
  IC61 <- CreateInputsCrit(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, ],
    AprioriIds = list(
      "54057" = userParam54057,
      "54032" = "54001",
      "54001" = "54095"
    ),
    transfo = "sqrt"
  )
  e <- runCalibration(
    nodes = nodes,
    Qinf = NULL,
    InputsCrit = IC61,
    CalibOptions = NULL,
    runRunModel = FALSE,
    IsHyst = FALSE
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  OC_user <- OutputsCalib
  expect_false(is.null(OC_user$`54057`$CritFinal))
  # Same calibration with a node-based a priori parameter for comparison
  IC_node <- CreateInputsCrit(
    InputsModel = InputsModel,
    RunOptions = RunOptions,
    Obs = Qobs[IndPeriod_Run, ],
    AprioriIds = c("54057" = "54032", "54032" = "54001", "54001" = "54095"),
    transfo = "sqrt"
  )
  e <- runCalibration(
    nodes = nodes,
    Qinf = NULL,
    InputsCrit = IC_node,
    CalibOptions = NULL,
    runRunModel = FALSE,
    IsHyst = FALSE
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  OC_node <- OutputsCalib
  # The user-defined a priori parameter (pulling toward the user-provided
  # parameters) changes the criterion output of the node compared to a
  # node-based a priori parameter (pulling toward the upstream node parameters)
  expect_false(isTRUE(all.equal(
    OC_user$`54057`$CritFinal,
    OC_node$`54057`$CritFinal
  )))
})

test_that("Lavenne criterion: user-defined a priori parameter with wrong format should throw error #61", {
  # Wrong length: node 54057 (GR4J with celerity) has 5 parameters
  expect_error(
    CreateInputsCrit(
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = Qobs[IndPeriod_Run, ],
      AprioriIds = list(
        "54057" = c(1, 2, 3),
        "54032" = "54001",
        "54001" = "54095"
      )
    ),
    regexp = "length of 5"
  )
  # NA values are not allowed
  expect_error(
    CreateInputsCrit(
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = Qobs[IndPeriod_Run, ],
      AprioriIds = list("54057" = c(1, NA, 3, 4, 5))
    ),
    regexp = "must not contain NA"
  )
  # An entry must be a character vector or a numeric vector
  expect_error(
    CreateInputsCrit(
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = Qobs[IndPeriod_Run, ],
      AprioriIds = list("54057" = list(1, 2, 3, 4, 5))
    ),
    regexp = "or numeric vectors"
  )
  # An atomic numeric vector is ambiguous: a named list is required
  expect_error(
    CreateInputsCrit(
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      Obs = Qobs[IndPeriod_Run, ],
      AprioriIds = c("54057" = c(1, 2, 3, 4, 5))
    ),
    regexp = "user-defined a priori parameters"
  )
})
