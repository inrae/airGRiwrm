# data set up
e <- setupRunModel()
# variables are copied from environment 'e' to the current environment
# https://stackoverflow.com/questions/9965577/r-copy-move-one-environment-to-another
for(x in ls(e)) assign(x, get(x, e))

test_that("RunModel.GRiwrmInputsModel should return same result with separated warm-up", {
  RO_WarmUp <- CreateRunOptions(
    InputsModel,
    IndPeriod_WarmUp = 0L,
    IndPeriod_Run = IndPeriod_WarmUp
  )
  OM_WarmUp <- RunModel(
    InputsModel,
    RunOptions = RO_WarmUp,
    Param = ParamMichel
  )
  RO_Run <- CreateRunOptions(
    InputsModel,
    IndPeriod_WarmUp = 0L,
    IndPeriod_Run = IndPeriod_Run,
    IniStates = lapply(OM_WarmUp, "[[", "StateEnd")
  )
  OM_Run <- RunModel(
    InputsModel,
    RunOptions = RO_Run,
    Param = ParamMichel
  )
  lapply(griwrm$id, function(id) {
    # The 2 exclamation marks are for seeing the id in the test result (See ?quasi_label)
    expect_equal(OM_GriwrmInputs[[!!id]]$Qsim, OM_Run[[!!id]]$Qsim)
  })
})

test_that("RunModel.Supervisor with no regulation should returns same results as RunModel.GRiwrmInputsModel", {
  sv <- CreateSupervisor(InputsModel)
  OM_Supervisor <- RunModel(
    sv,
    RunOptions = RunOptions,
    Param = ParamMichel
  )
  lapply(griwrm$id, function(id) {
    expect_equal(OM_Supervisor[[!!id]]$Qsim, OM_GriwrmInputs[[!!id]]$Qsim)
  })
})

test_that("RunModel.GRiwrmInputsModel handles CemaNeige", {
  l <- setUpCemaNeigeData()
  l$griwrm[l$griwrm$id == "Down", "model"] <- "RunModel_GR4J"
  l$TempMean <- l$TempMean[,1:2]
  l$ZInputs <- l$ZInputs[1:2]
  l$TempMean <- l$TempMean[,1:2]
  l$HypsoData <- l$HypsoData[,1:2]
  InputsModels <- suppressWarnings(
    CreateInputsModel(
      l$griwrm,
      DatesR = l$DatesR,
      Precip = l$Precip,
      PotEvap = l$PotEvap,
      TempMean = l$TempMean,
      ZInputs = l$ZInputs,
      HypsoData = l$HypsoData
    )
  )
  ## run period selection
  Ind_Run <- seq(which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1990-01-01"),
                 which(format(BasinObs$DatesR, format = "%Y-%m-%d")=="1999-12-31"))
  ## preparation of the RunOptions object
  RunOptions <- suppressWarnings(CreateRunOptions(InputsModels,
                                                  IndPeriod_Run = Ind_Run))
  ids <- l$griwrm$id
  names(ids) <- ids
  Params <- lapply(ids, function(x) {
    c(X1 = 408.774, X2 = 2.646, X3 = 131.264, X4 = 1.174,
      CNX1 = 0.962, CNX2 = 2.249)
  })
  Params$Down <- c(1, Params$Down[1:4])
  OutputsModel <- RunModel(
    InputsModels,
    RunOptions = RunOptions,
    Param = Params
  )
  expect_named(OutputsModel, l$griwrm$id)
  Qm3s <- attr(OutputsModel, "Qm3s")
  expect_equal(Qm3s[,4], rowSums(Qm3s[,2:3]))
})

n_div <- rbind(nodes,
               data.frame(id = "54029", down = "54002", length = 50, area = NA, model = "Diversion"))
g_div <- CreateGRiwrm(n_div)
Qmin = matrix(1E5, nrow = length(DatesR), ncol = 1)
colnames(Qmin) = "54029"
Qinf <- -Qmin
IM_div <- CreateInputsModel(g_div, DatesR, Precip, PotEvap, Qinf = Qinf, Qmin = Qmin)
RO_div <- setupRunOptions(IM_div)$RunOptions
P_div <- ParamMichel
P_div$`54002` <- c(1, ParamMichel$`54002`)

test_that("RunModel_Diversion with zero diversion equals no diversion", {
  Qinf[, ] <- 0
  IM <- CreateInputsModel(g_div, DatesR, Precip, PotEvap, Qinf = Qinf, Qmin = Qmin)
  OM <- RunModel(IM, RunOptions = RO_div, Param = P_div)
  expect_s3_class(OM, "GRiwrmOutputsModel")
  lapply(names(OM), function(id) {
    expect_equal(OM[[!!id]]$Qsim, OM_GriwrmInputs[[!!id]]$Qsim)
    expect_equal(OM[[!!id]]$Qsim_m3, OM_GriwrmInputs[[!!id]]$Qsim_m3)
    expect_equal(OM[[!!id]]$RunOptions$WarmUpQsim, OM_GriwrmInputs[[!!id]]$RunOptions$WarmUpQsim)
    expect_equal(OM[[!!id]]$RunOptions$WarmUpQsim_m3, OM_GriwrmInputs[[!!id]]$RunOptions$WarmUpQsim_m3)
  })
})

test_that("Huge diversion would result in Qsim_m3 == Qmin", {
  Qinf[, ] <- -1E12
  IM <- CreateInputsModel(g_div, DatesR, Precip, PotEvap, Qinf = Qinf, Qmin = Qmin)
  OM <- RunModel(IM, RunOptions = RO_div, Param = P_div)
  expect_equal(OM[["54029"]]$Qsim_m3, Qmin[RunOptions[[1]]$IndPeriod_Run])
  expect_equal(OM[["54029"]]$Qsim,
               Qmin[RunOptions[[1]]$IndPeriod_Run] /
                 g_div$area[g_div$id == "54029" & g_div$model != "Diversion"] / 1E3)
})

test_that("Huge minimum remaining flow results in Qdiv = 0", {
  Qinf[, ] <- -1000
  Qmin[, ] <- 1E12
  IM <- CreateInputsModel(g_div, DatesR, Precip, PotEvap, Qinf = Qinf, Qmin = Qmin)
  OM <- RunModel(IM, RunOptions = RO_div, Param = P_div)
  expect_equal(OM[["54029"]]$Qsim, OM[["54029"]]$Qnat)
  expect_equal(OM[["54029"]]$Qdiv_m3, rep(0, length(IndPeriod_Run)))
})

test_that("RunModel_Lag should work", {
  # This example is a network of 2 nodes which can be describe like this:
  db <- data.frame(id = c("54095", "DownLag"),
                   length = c(1, NA),
                   down = c("DownLag", NA),
                   area = as.double(c(3722.68, NA)),
                   model = c("RunModel_GR4J", "RunModel_Lag"),
                   stringsAsFactors = FALSE)
  g <- CreateGRiwrm(db)
  IM <- CreateInputsModel(g,
                          DatesR = DatesR,
                          Precip = Precip[, "54095", drop = FALSE],
                          PotEvap = PotEvap[, "54095", drop = FALSE])
  RO <- CreateRunOptions(IM,
                         IndPeriod_Run = IndPeriod_Run,
                         IndPeriod_WarmUp = IndPeriod_WarmUp)
  P <- ParamMichel["54095"]
  P$DownLag <- 1
  OM <- RunModel(IM, RO, P)
  expect_s3_class(OM, "GRiwrmOutputsModel")
  expect_true(all(!is.na(attr(OM, "Qm3s"))))
})

test_that("Upstream node - equal Diversion should return same results", {
  n_2ol <- nodes[nodes$id %in% c("54095", "54001"), ]
  n_2ol[n_2ol$id %in% c("54095", "54001"), c("down", "length")] <- c(NA, NA)
  meteoIds <- n_2ol$id
  n_2ol$area[n_2ol$id == "54001"] <-
    n_2ol$area[n_2ol$id == "54001"] - n_2ol$area[n_2ol$id == "54095"]
  n_2ol <- rbind(n_2ol,
                 data.frame(id = "54095", down = "54001", length = 42, area = NA, model = "Diversion"),
                 data.frame(id = "upstream", down = "54095", length = 0, area = NA, model = NA))
  g_2ol <- CreateGRiwrm(n_2ol)

  # Add upstream flow on 54095 that is removed by the Diversion
  # and derive previously simulated flow in order to get the same Qsim as before
  Qinf = matrix(0, nrow = length(DatesR), ncol = 2)
  Qinf[IndPeriod_Run, 1] <- OM_GriwrmInputs[["54095"]]$Qsim_m3
  Qinf[IndPeriod_Run, 2] <- - OM_GriwrmInputs[["54095"]]$Qsim_m3
  Qinf[IndPeriod_WarmUp, 1] <- OM_GriwrmInputs[["54095"]]$RunOptions$WarmUpQsim_m3
  Qinf[IndPeriod_WarmUp, 2] <- - OM_GriwrmInputs[["54095"]]$RunOptions$WarmUpQsim_m3

  colnames(Qinf) <- c("upstream", "54095")

  Qmin = matrix(0, nrow = length(DatesR), ncol = 1)
  colnames(Qmin) <- "54095"

  IM_2ol <- CreateInputsModel(g_2ol,
                              DatesR,
                              Precip[, meteoIds],
                              PotEvap[, meteoIds],
                              Qinf = Qinf,
                              Qmin = Qmin)

  RO_2ol <- setupRunOptions(IM_2ol)$RunOptions
  P_2ol <- ParamMichel[names(IM_2ol)]
  P_2ol[["54095"]] <- c(1, P_2ol[["54095"]])
  OM_2ol <- RunModel(IM_2ol, RO_2ol, P_2ol)

  # Is the diversion correctly taken into account?
  expect_equal(
    OM_2ol[["54095"]]$Qdiv_m3,
    OM_GriwrmInputs[["54095"]]$Qsim_m3
  )

  # Is 54001 InputsModel correctly updated?
  IM_54001_div <- UpdateQsimUpstream(IM_2ol[["54001"]],
                            RO_2ol[["54001"]],
                            OM_2ol)
  IM_54001 <- UpdateQsimUpstream(InputsModel[["54001"]],
                                 RunOptions[["54001"]],
                                 OM_GriwrmInputs)
  expect_equal(
    IM_54001_div$Qupstream,
    IM_54001$Qupstream
  )

  # All simulated flows with or without div must be equal
  sapply(names(IM_2ol), function(id) {
    expect_equal(OM_2ol[[!!id]]$Qsimdown, OM_GriwrmInputs[[!!id]]$Qsimdown)
    expect_equal(OM_2ol[[!!id]]$Qsim_m3, OM_GriwrmInputs[[!!id]]$Qsim_m3)
  })

  id <- "54095"
  IM_54095_div <- IM_2ol[[id]]
  class(IM_54095_div) <- setdiff(class(IM_54095_div), "SD")

  OM_airGR <- airGR::RunModel(IM_54095_div,
                              RunOptions = RO_2ol[[id]],
                              Param = P_2ol[[id]],
                              FUN_MOD = RunModel.InputsModel)

  expect_equal(OM_airGR$Qsimdown, OM_GriwrmInputs[[!!id]]$Qsimdown)
  expect_equal(OM_airGR$Qsim_m3, OM_GriwrmInputs[[!!id]]$Qsim_m3)
})

test_that("A transparent upstream node with area=NA should return same result #124", {
  nodes <- loadSevernNodes()
  nodes <- nodes[nodes$id %in% c("54095", "54001"), ]
  nodes[nodes$id == "54001", c("down", "length")] <- c(NA, NA)
  nodes$down[nodes$id == "54095"] <- "P"
  nodes$length[nodes$id == "54095"] <- 0
  nodes <- rbind(
    nodes,
    data.frame(id = "P", down = "54001", length = 42, area = NA, model = "RunModel_Lag")
  )
  g <- CreateGRiwrm(nodes)
  e <- setupRunModel(griwrm = g, runRunModel = FALSE)
  for (x in ls(e)) assign(x, get(x, e))
  Param <- ParamMichel[c("54095", "54001")]
  Param[["P"]] <- 1
  OM <- RunModel(InputsModel,
                 RunOptions = RunOptions,
                 Param = Param)
  expect_equal(OM[["P"]]$Qsim_m3, OM[["54095"]]$Qsim_m3)
  names(OM[["54001"]]$StateEnd$SD[[1]]) <- "54095" # For matching upstream IDs with ref
  expect_equal(OM[["54001"]], OM_GriwrmInputs[["54001"]])
})

test_that("RunModel should return water deficit (Qover_m3)", {
  nodes <- loadSevernNodes()
  nodes <- nodes[nodes$id %in% c("54095", "54001"), ]
  nodes[nodes$id == "54001", c("down", "length")] <- c(NA, NA)
  nodes <- rbind(
    nodes,
    data.frame(id = "P", down = "54001", length = 10, area = NA, model = NA)
  )
  g <- CreateGRiwrm(nodes)
  Qinf <- data.frame(P = rep(-2E6, length(DatesR)))
  expect_warning(e <- setupRunModel(griwrm = g, runRunModel = TRUE, Qinf = Qinf))
  for (x in ls(e)) assign(x, get(x, e))
  expect_false(any(OM_GriwrmInputs$`54001`$Qsim_m3 < 0))
  expect_true(all(OM_GriwrmInputs$`54001`$Qover_m3 >= 0))
  sv <- CreateSupervisor(InputsModel)
  OM_sv <- RunModel(sv, RunOptions, ParamMichel)
  expect_equal(OM_sv$`54001`$Qsim_m3, OM_GriwrmInputs$`54001`$Qsim_m3)
  expect_equal(OM_sv$`54001`$Qover_m3, OM_GriwrmInputs$`54001`$Qover_m3)
})
