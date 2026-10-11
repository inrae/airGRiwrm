skip_on_cran()

data(Severn)

test_that("RunModel.GRiwrmOutputsModel with a Supervisor takes the Yinit of the
            first supervision step from the previous outputs", {
  # Network with a direct-injection node controlled by a supervisor
  nodes <- loadSevernNodes()
  nodes <- rbind(
    nodes,
    data.frame(id = "R1", down = "54057", length = 100, area = NA, model = NA)
  )
  griwrm <- CreateGRiwrm(nodes)
  Qinf <- matrix(0, nrow = length(Severn$BasinsObs[[1]]$DatesR), ncol = 1)
  colnames(Qinf) <- "R1"
  e <- setupRunModel(griwrm = griwrm, Qinf = Qinf, runRunModel = FALSE)
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  rm(e)

  # Phase 1: supervised run with warm-up period
  sv1 <- CreateSupervisor(InputsModel)
  CreateController(sv1, "C1", Y = "54002", U = "R1", FUN = function(Y) -Y)
  OM1 <- RunModel(sv1, RunOptions = RunOptions, Param = ParamMichel)
  n1 <- nrow(attr(OM1, "Qm3s"))

  # Phase 2: hot restart with a supervisor for the next 31 days
  nDays2 <- 31L
  DatesR2 <- seq(
    OM1[["54057"]]$DatesR[n1] + 86400,
    by = "day",
    length.out = nDays2
  )
  Qinf2 <- matrix(0, nrow = nDays2, ncol = 1)
  colnames(Qinf2) <- "R1"
  InputsModel2 <- CreateInputsModel(
    griwrm,
    DatesR2,
    Precip[(length(DatesR) - nDays2 + 1):length(DatesR), ],
    PotEvap[(length(DatesR) - nDays2 + 1):length(DatesR), ],
    Qinf = Qinf2
  )
  sv2 <- CreateSupervisor(InputsModel2)
  CreateController(sv2, "C1", Y = "54002", U = "R1", FUN = function(Y) -Y)
  RO2 <- CreateRunOptions(
    InputsModel2,
    IndPeriod_WarmUp = 0L,
    IndPeriod_Run = seq_len(nDays2)
  )

  # Yinit must be the last simulated flow at the Y node of the previous run
  # (controllers observe Qsim in m3 per time step, i.e. `Qsim_m3`)
  Y_last <- OM1[["54002"]]$Qsim_m3[n1]
  expect_false(is.na(Y_last))

  OM2 <- RunModel(OM1, sv2, RunOptions = RO2, IndPeriod_Run = seq_len(nDays2))

  # First supervision step of the new run uses the Y taken from the previous
  # outputs (U = -Y written to the direct-injection node R1)
  expect_equal(
    attr(OM2, "Qm3s")[n1 + 1, "R1"],
    -Y_last / attr(OM2, "TimeStep")
  )
  # The new run starts the time step right after the previous run
  expect_equal(
    OM2[["54057"]]$DatesR[n1 + 1],
    OM1[["54057"]]$DatesR[n1] + 86400
  )
  # The outputs of the previous run are kept by the merge
  expect_equal(attr(OM2, "Qm3s")[1:n1, ], attr(OM1, "Qm3s"))
  expect_equal(length(OM2[["54057"]]$Qsim), n1 + nDays2)
  # The warm-up data of the first run is kept in the merged outputs
  expect_equal(
    OM2[["54057"]]$RunOptions$WarmUpQsim_m3,
    OM1[["54057"]]$RunOptions$WarmUpQsim_m3
  )
  # No NA in the new run
  expect_false(any(is.na(OM2[["54057"]]$Qsim[(n1 + 1):(n1 + nDays2)])))
})

test_that("Single node returns same result as RunModel.GRiwrmInputsModel", {
  nodes <- loadSevernNodes()
  nodes <- nodes[nodes$id == "54029", ]
  nodes$down <- NA_character_
  nodes$length <- NA_real_
  e <- setupRunModel(
    runRunOptions = FALSE,
    griwrm = CreateGRiwrm(nodes)
  )
  for (x in ls(e)) {
    assign(x, get(x, e))
  }
  ROref <- CreateRunOptions(
    InputsModel,
    IndPeriod_WarmUp = 1:364,
    IndPeriod_Run = 365:366
  )
  OMref <- RunModel(
    InputsModel,
    RunOptions = ROref,
    Param = ParamMichel
  )
  ROwarmUp <- CreateRunOptions(
    InputsModel,
    IndPeriod_WarmUp = 1:364,
    IndPeriod_Run = 365L
  )
  OMwarmUp <- RunModel(
    InputsModel,
    RunOptions = ROwarmUp,
    Param = ParamMichel
  )
  ROhotStart <- CreateRunOptions(
    InputsModel,
    IniStates = lapply(OMwarmUp, "[[", "StateEnd"),
    IndPeriod_WarmUp = 0L,
    IndPeriod_Run = 366L
  )
  ROhotStart$`54029`$IniResLevels <- NULL
  # State Initiation
  ROtest <- ROwarmUp
  for (id in names(ROtest)) {
    # Run model for the sub-basin and one time step
    ROtest[[id]]$IniResLevels <- NULL
    ROtest[[id]]$IniStates <- serializeIniStates(
      OMwarmUp[[id]]$StateEnd,
      InputsModel[[id]]
    )
    ROtest[[id]]$IndPeriod_WarmUp <- 0L
    ROtest[[id]]$IndPeriod_Run <- 366L
  }
  expect_equal(ROtest$`54029`, ROhotStart$`54029`)
  OMtest <- RunModel(
    OMwarmUp,
    InputsModel = InputsModel,
    RunOptions = ROwarmUp,
    IndPeriod_Run = 366L
  )
  expect_equal(OMtest$`54029`, OMref$`54029`)
})

# Setup model
griwrm <- CreateGRiwrm(rbind(
  n_derived_rsrvr,
  data.frame(
    id = "WD",
    down = "Dam",
    length = 0,
    area = NA,
    model = NA
  )
))
DatesR <- Severn$BasinsObs[[1]]$DatesR
Qinf <- data.frame(
  # Diversion to the dam
  `54095` = rep(-1E6, length(DatesR)),
  # Withdrawal in the dam
  WD = rep(-250000, length(DatesR))
)
names(Qinf)[1] <- "54095"
# Release of the dam back to the river
Qrelease <- data.frame(Dam = rep(100E3, length(DatesR)))
# Diversion limited by fixed minimum flow to let in the river
Qmin <- data.frame("54095" = rep(3E6, length(DatesR)))
names(Qmin) <- "54095"
e <- setupRunModel(
  runRunModel = FALSE,
  griwrm = griwrm,
  Qinf = Qinf,
  Qrelease = Qrelease,
  Qmin = Qmin
)
for (x in ls(e)) {
  assign(x, get(x, e))
}

# Simulation periods up to 31/12/1986
dfTS <- data.frame(
  DatesR = DatesR,
  yearmonth = format(DatesR, "%Y-%m")
)
dfTS <- dfTS[1:(which(dfTS$yearmonth == "1987-01")[1] - 1), ]

# Run simulation in "normal" mode
Param <- c(
  ParamMichel[names(ParamMichel) %in% griwrm$id],
  list(Dam = c(100E6, 1))
)
ROref <- CreateRunOptions(
  InputsModel,
  IndPeriod_WarmUp = 1:364,
  IndPeriod_Run = 365:nrow(dfTS),
  warnings = FALSE # Warn: model states initialisation not defined: default configuration used
)
OMref <- RunModel(InputsModel, ROref, Param)

# Set up initial conditions
ROO <- CreateRunOptions(
  InputsModel,
  IndPeriod_WarmUp = 1:364,
  IndPeriod_Run = 365L,
  warnings = FALSE # Warn: model states initialisation not defined: default configuration used
)
OM <- RunModel(InputsModel, ROO, Param)

test_that("RunModel.GRiwrmOutputsModel works with InputsModel", {
  for (ym in unique(dfTS$yearmonth[dfTS$DatesR > OM[[1]]$DatesR])) {
    # Preparing extract of Qinf for the current run
    ym_IndPeriod_Run <- which(dfTS$yearmonth == ym)
    ym_Qinf <- Qinf[ym_IndPeriod_Run, , drop = FALSE]
    ym_Qrelease <- Qrelease[ym_IndPeriod_Run, , drop = FALSE]

    # 50% Restriction on reservoir withdrawals if remaining less than 90 days of water
    # nb_remain_days <- OM$Dam$StateEnd$Reservoir$V / (-ym_Qinf$`WD`[1] + ym_Qrelease$Dam[1])
    # if (nb_remain_days < 180) {
    #   ym_Qinf$`WD` <- -(max(0, OM$Dam$StateEnd$Reservoir$V - sum(ym_Qrelease$Dam))) / 365
    # }
    OM <- RunModel(
      OM,
      InputsModel = InputsModel,
      RunOptions = RunOptions,
      IndPeriod_Run = ym_IndPeriod_Run,
      Qinf = ym_Qinf
    )
  }

  expect_equal(nrow(attr(OM, "Qm3s")), nrow(dfTS) - 364)
  expect_equal(length(OM[[1]]$DatesR), nrow(dfTS) - 364)
  expect_equal(attr(OM, "Qm3s"), attr(OMref, "Qm3s"))
})

test_that("RunModel.GRiwrmOutputsModel works with Supervisor", {
  sv <- CreateSupervisor(InputsModel)

  curve <- approx(
    x = c(31 * 11 - 365, 30 * 6, 31 * 11, 366 + 30 * 6),
    y = c(20E6, 90E6, 20E6, 90E6),
    xout = 1:366
  )$y

  fn_guide_curve_factory <- function(sv, curve) {
    function(y) {
      # How much to release for reaching the filling curve ?
      deltaV <- sv$OutputsModel$Dam$Vsim - curve[lubridate::yday(sv$ts.date)]
      # Minimum 500L/s and max 1E6 m3
      return(max(86400 / 2, min(1E6, deltaV)))
    }
  }
  fn_guide_curve <- fn_guide_curve_factory(sv, curve)

  CreateController(sv, "dam_filling_curve", Y = NULL, U = "Dam", fn_guide_curve)

  for (ym in unique(dfTS$yearmonth[dfTS$DatesR > OM[[1]]$DatesR])) {
    message("Processing period ", ym)
    # Preparing extract of Qinf for the current run
    ym_IndPeriod_Run <- which(dfTS$yearmonth == ym)
    ym_Qinf <- Qinf[ym_IndPeriod_Run, , drop = FALSE]
    ym_Qrelease <- Qrelease[ym_IndPeriod_Run, , drop = FALSE]

    # 50% Restriction on reservoir withdrawals if remaining less than 90 days of water
    nb_remain_days <- OM$Dam$StateEnd$Reservoir$V /
      (-ym_Qinf$`WD`[1] + ym_Qrelease$Dam[1])
    if (nb_remain_days < 180) {
      ym_Qinf$`WD` <- -(max(
        0,
        OM$Dam$StateEnd$Reservoir$V - sum(ym_Qrelease$Dam)
      )) /
        365
    }
    OM <- RunModel(
      OM,
      InputsModel = sv,
      RunOptions = RunOptions,
      IndPeriod_Run = ym_IndPeriod_Run,
      Qinf = ym_Qinf
    )
  }
  expect_gte(sum(attr(OM, "Qm3s")$WD), sum(Qinf$WD[1:(nrow(dfTS) - 364)]))
})
