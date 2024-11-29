skip_on_cran()
test_that("RunModel.GRiwrmOutputsModel works", {
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
  data(Severn)
  DatesR <-  Severn$BasinsObs[[1]]$DatesR
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
    griwrm = griwrm,
    Qinf = Qinf,
    Qrelease = Qrelease,
    Qmin = Qmin,
    runRunOptions = FALSE
  )
  for (x in ls(e)) assign(x, get(x, e))

  # Set up initial conditions
  RunOptions <- CreateRunOptions(InputsModel, IndPeriod_WarmUp = 1:364, IndPeriod_Run = 365L)
  Param <- c(ParamMichel[names(ParamMichel) %in% griwrm$id], list(Dam = c(100E6, 1)))
  OM <- RunModel(InputsModel, RunOptions, Param)

  # Loop over periods months periods
  dfTS <- data.frame(
    DatesR = DatesR,
    yearmonth = format(DatesR, "%Y-%m")
  )

  for(ym in unique(dfTS$yearmonth[dfTS$DatesR > OM[[1]]$DatesR])) {

    # Preparing extract of Qinf for the current run
    ym_IndPeriod_Run <- which(dfTS$yearmonth == ym)
    ym_Qinf <- Qinf[ym_IndPeriod_Run, , drop = FALSE]
    ym_Qrelease <- Qrelease[ym_IndPeriod_Run, , drop = FALSE]

    # 50% Restriction on reservoir withdrawals if remaining less than 90 days of water
    nb_remain_days <- OM$Dam$StateEnd$Reservoir$V / (-ym_Qinf$`WD`[1] + ym_Qrelease$Dam[1])
    if (nb_remain_days < 180) {
      ym_Qinf$`WD` <- -(max(0, OM$Dam$StateEnd$Reservoir$V - sum(ym_Qrelease$Dam))) / 365
    }
    OM <- RunModel(OM,
                   InputsModel = InputsModel,
                   RunOptions = RunOptions,
                   IndPeriod_Run = ym_IndPeriod_Run,
                   Qinf = ym_Qinf)
  }

  expect_equal(nrow(attr(OM, "Qm3s")), length(DatesR) - 364)
  expect_equal(length(OM[[1]]$DatesR), length(DatesR) - 364)
})
