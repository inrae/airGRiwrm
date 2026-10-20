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
## airGRiwrm's CreateRunOptions adds airGRiwrm-specific outputs (Qsim_m3,
## QsimDown) to Outputs_Sim.  Build a reference RunOptions that includes them
## so the equality tests below pass.
RunOptions <- airGR::CreateRunOptions(
  RunModel_GR4J,
  InputsModel = InputsModel,
  IndPeriod_Run = Ind_Run,
  IndPeriod_WarmUp = Ind_WarmUp,
  warnings = FALSE
)
## Restore airGRiwrm-specific outputs that airGR doesn't know about
RunOptions$Outputs_Sim <- c(
  "DatesR",
  "WarmUpQsim",
  "Qsim",
  "Qsim_m3",
  "QsimDown",
  "Param",
  "StateEnd"
)


test_that("CreateRunOptions.InputsModel works", {
  expect_equal(
    CreateRunOptions(
      InputsModel,
      FUN_MOD = RunModel_GR4J,
      IndPeriod_Run = Ind_Run,
      IndPeriod_WarmUp = Ind_WarmUp
    ),
    RunOptions
  )
  InputsModel$FUN_MOD <- RunModel_GR4J
  expect_equal(
    CreateRunOptions(
      InputsModel,
      IndPeriod_Run = Ind_Run,
      IndPeriod_WarmUp = Ind_WarmUp
    ),
    RunOptions
  )
})

test_that("CreateRunOptions.character works", {
  expect_equal(
    CreateRunOptions(
      "RunModel_GR4J",
      InputsModel = InputsModel,
      IndPeriod_Run = Ind_Run,
      IndPeriod_WarmUp = Ind_WarmUp
    ),
    RunOptions
  )
})

test_that("CreateRunOptions.function works", {
  expect_equal(
    CreateRunOptions(
      RunModel_GR4J,
      InputsModel = InputsModel,
      IndPeriod_Run = Ind_Run,
      IndPeriod_WarmUp = Ind_WarmUp
    ),
    RunOptions
  )
})

## Network level: validation of the per-node Outputs_Sim selection (#141)
e_net <- setupRunModel(
  runInputsModel = TRUE,
  runRunOptions = FALSE,
  runRunModel = FALSE
)
InputsModel_net <- e_net$InputsModel
Ind_Run_net <- seq(
  length(InputsModel_net[[1]]$DatesR) - 365 + 1,
  length(InputsModel_net[[1]]$DatesR)
)
Ind_WarmUp_net <- seq(Ind_Run_net[1] - 365, Ind_Run_net[1] - 1)

test_that("CreateRunOptions.GRiwrmInputsModel validates Outputs_Sim", {
  # A per-node list must cover every node of the network
  expect_error(
    CreateRunOptions(
      InputsModel_net,
      IndPeriod_Run = Ind_Run_net,
      IndPeriod_WarmUp = Ind_WarmUp_net,
      Outputs_Sim = setNames(
        list(c("Qsim", "Param")),
        names(InputsModel_net)[1]
      )
    ),
    "must define outputs for every node"
  )
  # A per-node list with invalid node IDs is rejected
  expect_error(
    CreateRunOptions(
      InputsModel_net,
      IndPeriod_Run = Ind_Run_net,
      IndPeriod_WarmUp = Ind_WarmUp_net,
      Outputs_Sim = setNames(
        lapply(names(InputsModel_net), function(id) c("Qsim", "Param")),
        paste0("9", names(InputsModel_net))
      )
    ),
    "contains invalid node IDs"
  )
  # A complete per-node selection is applied to each node. On this
  # semi-distributed network the selection is completed with the outputs
  # required to propagate the simulated flows between nodes (#98).
  expected <- unique(c(c("Qsim", "Param"), MinimalOutputsSim))
  RunOptionsNet <- CreateRunOptions(
    InputsModel_net,
    IndPeriod_Run = Ind_Run_net,
    IndPeriod_WarmUp = Ind_WarmUp_net,
    Outputs_Sim = setNames(
      lapply(names(InputsModel_net), function(id) c("Qsim", "Param")),
      names(InputsModel_net)
    )
  )
  expect_true(all(vapply(
    RunOptionsNet,
    function(x) identical(x$Outputs_Sim, expected),
    logical(1)
  )))
})

test_that("An explicit Outputs_Sim subset includes the propagation outputs on a semi-distributed network (#98)", {
  # Scenario A of research/issue-98-factcheck.R: with an explicit
  # selection lacking "WarmUpQsim", the run used to fail in airGR's
  # semi-distributed aggregation ("Time series WarmUpQsim in
  # 'QcontribDown' should have the same length as
  # 'RunOptions$IndPeriod_WarmUp'"). CreateRunOptions now completes the
  # explicit selection with the minimal propagation set (same set as the
  # default selection, #141).
  RunOptionsQsim <- CreateRunOptions(
    InputsModel_net,
    IndPeriod_Run = Ind_Run_net,
    IndPeriod_WarmUp = Ind_WarmUp_net,
    Outputs_Sim = c("Qsim")
  )
  expect_true(all(vapply(
    RunOptionsQsim,
    function(x) all(MinimalOutputsSim %in% x$Outputs_Sim),
    logical(1)
  )))
  expect_true(all(vapply(
    RunOptionsQsim,
    function(x) "Qsim" %in% x$Outputs_Sim,
    logical(1)
  )))
})

test_that("An explicit Outputs_Sim subset is not completed on a non semi-distributed network (#98)", {
  # Without semi-distributed node, no propagation output is required:
  # the explicit selection is applied as is.
  nodes_ns <- loadSevernNodes()
  nodes_ns <- nodes_ns[nodes_ns$id == "54095", ]
  nodes_ns[nodes_ns$id == "54095", c("down", "length")] <- c(NA, NA)
  e_ns <- setupRunModel(
    nodes = nodes_ns,
    runInputsModel = TRUE,
    runRunOptions = FALSE,
    runRunModel = FALSE
  )
  RunOptionsNs <- CreateRunOptions(
    e_ns$InputsModel,
    IndPeriod_Run = Ind_Run_net,
    IndPeriod_WarmUp = Ind_WarmUp_net,
    Outputs_Sim = c("Qsim")
  )
  expect_true(all(vapply(
    RunOptionsNs,
    function(x) identical(x$Outputs_Sim, c("Qsim")),
    logical(1)
  )))
})

test_that("Outputs_Sim = 'all' restores the previous behaviour (#141)", {
  RunOptionsAll <- CreateRunOptions(
    InputsModel_net,
    IndPeriod_Run = Ind_Run_net,
    IndPeriod_WarmUp = Ind_WarmUp_net,
    Outputs_Sim = "all",
    warnings = FALSE
  )
  # The legacy string "all" must not leak into the object: airGR::RunModel
  # selects the run-time outputs from this vector, and an "all" string
  # matches no output name (it would drop every output).
  expect_false(any(vapply(
    RunOptionsAll,
    function(ro) any(ro$Outputs_Sim == "all"),
    logical(1)
  )))
  OMAll <- suppressWarnings(RunModel(
    InputsModel_net,
    RunOptionsAll,
    e_net$ParamMichel
  ))
  # The routing step needs the upstream Qsim_m3
  expect_false(is.null(OMAll[[1]]$Qsim_m3))
  expect_false(is.null(OMAll[[1]]$Qsim))
})
