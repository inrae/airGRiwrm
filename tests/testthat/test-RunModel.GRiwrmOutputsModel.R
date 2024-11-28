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
    `54095` = rep(-1500000, length(DatesR)),
    WD = rep(-250000, length(DatesR))
  )
  names(Qinf)[1] <- "54095"
  Qrelease <- data.frame(Dam = rep(100000, length(DatesR)))
  Qmin <- data.frame("54095" = rep(1000000, length(DatesR)))
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
  Qm3s <- attr(OM, "Qm3s")
  for(ym in unique(dfTS$yearmonth[dfTS$DatesR > OM[[1]]$DatesR])) {
    OM <- RunModel(OM,
                   InputsModel = InputsModel,
                   RunOptions = RunOptions,
                   IndPeriod_Run = which(dfTS$yearmonth == ym))
    Qm3s <- rbind(Qm3s, attr(OM, "Qm3s"))
  }
  expect_equal(nrow(Qm3s), length(DatesR) - 364)
})
