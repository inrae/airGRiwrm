# data set up
e <- setupRunModel(runRunModel = FALSE)
# variables are copied from environment 'e' to the current environment
for (x in ls(e)) {
  assign(x, get(x, e))
}

# Run the model with the meteorological data included in the outputs
# (previous default behaviour, reduced by #141)
RunOptions_all <- CreateRunOptions(
  InputsModel,
  IndPeriod_WarmUp = IndPeriod_WarmUp,
  IndPeriod_Run = IndPeriod_Run,
  Outputs_Sim = "all",
  warnings = FALSE
)
OM <- suppressMessages(
  RunModel(InputsModel, RunOptions = RunOptions_all, Param = ParamMichel)
)
gr <- attr(OM, "GRiwrm")

# Synthetic chain: A (area 100) -> B (area 300)
nds <- data.frame(
  id = c("A", "B"),
  down = c("B", NA),
  length = c(10, NA),
  area = c(100, 300),
  model = c("RunModel_GR4J", "RunModel_GR4J"),
  stringsAsFactors = FALSE
)
g <- CreateGRiwrm(nds)

test_that("getNodeLocalArea returns the area of the local sub-basin", {
  # A has no upstream node: its local area is its own area
  expect_equal(getNodeLocalArea("A", g), 100)
  # The local area of B is its catchment area minus the catchment area
  # of its upstream node A: 300 - 100 = 200
  expect_equal(getNodeLocalArea("B", g), 200)
})

test_that("getNodeLocalArea and catchmentMeteoMean are graceful on a Diversion twin", {
  # A Diversion node shares the id of the model node it is attached to:
  # two rows of the network have the same id (see CreateGRiwrm)
  nds_div <- data.frame(
    id = c("A", "B", "A"),
    down = c("B", NA, "B"),
    length = c(10, NA, 50),
    area = c(100, 300, NA_real_),
    model = c("RunModel_GR4J", "RunModel_GR4J", "Diversion"),
    stringsAsFactors = FALSE
  )
  g_div <- CreateGRiwrm(nds_div)
  # The local sub-basin area of the shared id cannot be computed
  expect_equal(getNodeLocalArea("A", g_div), NA_real_)
  # The local area of the downstream node is not affected: 300 - 100 = 200
  expect_equal(getNodeLocalArea("B", g_div), 200)
  # The twin is excluded from the weighted mean without any error: the
  # mean at B falls back on the time series of B itself
  P_div <- cbind(A = c(10, 20, 30), B = c(1, 2, 3))
  expect_no_error(catchmentMeteoMean(g_div, "B", P_div))
  expect_equal(catchmentMeteoMean(g_div, "B", P_div), P_div[, "B"])
})

test_that("catchmentMeteoMean computes the area-weighted mean of the catchment", {
  P <- cbind(A = c(10, 20, 30), B = c(1, 2, 3))
  # Weighted mean at B over the whole catchment (A + B):
  # (100 * P_A + 200 * P_B) / 300 = c(4, 8, 12)
  expect_equal(catchmentMeteoMean(g, "B", P), c(4, 8, 12))
  # A node without upstream node keeps its own time series
  expect_equal(catchmentMeteoMean(g, "A", P), P[, "A"])
})

test_that("catchmentMeteoMean handles missing data and unknown areas", {
  # NA in the series of A: A does not contribute at time step 2
  P_NA <- cbind(A = c(10, NA, 30), B = c(1, 2, 3))
  expect_equal(catchmentMeteoMean(g, "B", P_NA), c(4, 2, 12))
  # No known local sub-basin area: falls back to the time series of the
  # node itself (graceful behaviour)
  nds_na <- nds
  nds_na$area <- NA_real_
  nds_na$model <- "RunModel_Lag"
  g_na <- CreateGRiwrm(nds_na)
  P2 <- cbind(A = c(1, 2, 3), B = c(4, 5, 6))
  expect_equal(catchmentMeteoMean(g_na, "B", P2), P2[, "B"])
})

test_that("adjustMeteoToCatchment replaces the meteo data at the catchment scale", {
  # Catchment of 54032: 54095, 54001, 54029, 54032
  # Local sub-basin areas (Severn):
  #   54095: 3722.68 (no upstream)
  #   54001: 4329.90 - 3722.68 = 607.22
  #   54029: 1483.65 (no upstream)
  #   54032: 6864.88 - 4329.90 - 1483.65 = 1051.33
  Precip_54032 <- OM[["54032"]]$Precip
  OM_adj <- adjustMeteoToCatchment(OM[["54032"]], "54032", OM, gr)
  expect_equal(
    OM_adj$Precip,
    (
      3722.68 * OM[["54095"]]$Precip +
        607.22 * OM[["54001"]]$Precip +
        1483.65 * OM[["54029"]]$Precip +
        1051.33 * OM[["54032"]]$Precip
    ) / 6864.88
  )
  expect_equal(
    OM_adj$PotEvap,
    (
      3722.68 * OM[["54095"]]$PotEvap +
        607.22 * OM[["54001"]]$PotEvap +
        1483.65 * OM[["54029"]]$PotEvap +
        1051.33 * OM[["54032"]]$PotEvap
    ) / 6864.88
  )
  # A node without upstream node keeps its own time series
  OM_adj_leaf <- adjustMeteoToCatchment(OM[["54095"]], "54095", OM, gr)
  expect_equal(OM_adj_leaf$Precip, OM[["54095"]]$Precip)
  # The original object is not modified
  expect_equal(OM[["54032"]]$Precip, Precip_54032)
})

test_that("plot.GRiwrmOutputsModel displays the meteo data at the catchment scale (#178)", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  # airGR::plot.OutputsModel warns when Qobs is missing for the Error/CorQQ
  # panels: this warning is expected and unrelated to #178
  expect_no_error(suppressWarnings(plot(OM, unit = "mm")))
  expect_no_error(plot(OM, Qobs = Qobs[IndPeriod_Run, ], unit = "mm"))
})

test_that("plot.GRiwrmOutputsModel is graceful when the meteo data are not available (#178)", {
  # Mimic a run with the default Outputs_Sim (see #141): no meteo data
  # in the OutputsModel
  OM_min <- lapply(OM, function(om) {
    om$Precip <- NULL
    om$PotEvap <- NULL
    om
  })
  attributes(OM_min) <- attributes(OM)
  expect_true(all(sapply(names(OM_min), function(id) {
    is.null(OM_min[[id]]$Precip) && is.null(OM_min[[id]]$PotEvap)
  })))
  # The adjustment is a no-op on such an object
  expect_identical(
    adjustMeteoToCatchment(OM_min[["54032"]], "54032", OM_min, gr),
    OM_min[["54032"]]
  )
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_error(suppressWarnings(plot(OM_min, unit = "mm")))
})

# CemaNeige network used by the tests of the per-layer adjustment of the
# meteorological data at the whole-catchment scale (#178).
# A chain of 2 CemaNeige nodes built on the airGR example data set L0123001:
#   Upstream (local area 360 km2) -> Downstream (local area 360 km2)
# The local sub-basin areas are equal, so the area-weighted mean of a layer
# series at Downstream is the arithmetic mean of the two nodes.
e_cn <- setUpCemaNeigeData()
g_cn <- CreateGRiwrm(data.frame(
  id = c("Upstream", "Downstream"),
  down = c("Downstream", NA),
  length = c(0, NA),
  area = c(360, 720),
  model = rep("RunModel_CemaNeigeGR4J", 2),
  stringsAsFactors = FALSE
))
# The fixture provides the time series of 3 nodes ("Up1", "Up2", "Down"):
# the 2 nodes of the network reuse the series of "Up1" (Upstream) and "Down"
# (Downstream). The fixture replicates the same meteorological data on its 3
# nodes, so the layer series simulated on the 2 nodes are identical: the test
# of the per-layer adjustment shifts the layer series of Upstream afterwards
# so that the area-weighted mean differs from the series of Downstream.
cnSeries <- function(x) {
  x <- x[, c("Up1", "Down"), drop = FALSE]
  colnames(x) <- c("Upstream", "Downstream")
  return(x)
}
IM_cn <- suppressWarnings(CreateInputsModel(
  g_cn,
  DatesR = e_cn$DatesR,
  Precip = cnSeries(e_cn$Precip),
  PotEvap = cnSeries(e_cn$PotEvap),
  TempMean = cnSeries(e_cn$TempMean),
  ZInputs = setNames(e_cn$ZInputs[c("Up1", "Down")], c("Upstream", "Downstream")),
  HypsoData = cnSeries(e_cn$HypsoData)
))
# Short run period to keep the test suite fast
RO_cn <- suppressWarnings(CreateRunOptions(IM_cn, IndPeriod_Run = seq(1, 30)))
Param_cn <- list(
  Upstream = c(
    X1 = 408.774, X2 = 2.646, X3 = 131.264, X4 = 1.174,
    CNX1 = 0.962, CNX2 = 2.249
  ),
  # Downstream receives the simulated flow of Upstream: the first parameter
  # is the celerity of the routing model (See airGR::RunModel_Lag)
  Downstream = c(
    1, X1 = 408.774, X2 = 2.646, X3 = 131.264, X4 = 1.174,
    CNX1 = 0.962, CNX2 = 2.249
  )
)
OM_cn <- suppressWarnings(RunModel(IM_cn, RunOptions = RO_cn, Param = Param_cn))
gr_cn <- attr(OM_cn, "GRiwrm")

# Hand-made GRiwrmOutputsModel object with controlled per-layer series.
# The layer series of the 2 nodes differ, so that the expected area-weighted
# mean (the local sub-basin areas of the 2 nodes are 360 km2 each, i.e. the
# arithmetic mean) is known exactly and differs from the series of the node
# itself.
synTS <- 3
synDates <- as.POSIXct("2000-01-01", tz = "UTC") + seq(0, by = 86400, length.out = synTS)
MakeSynNode <- function(
  shifts,
  nLayers = length(shifts),
  fields = c("Temp", "SnowPack", "Psol")
) {
  Layers <- lapply(seq_len(nLayers), function(i) {
    series <- lapply(seq_along(fields), function(j) {
      seq_len(synTS) + shifts[i] + j * 10
    })
    names(series) <- fields
    series
  })
  c(
    list(DatesR = synDates, Qsim = rep(1, synTS)),
    list(CemaNeigeLayers = Layers)
  )
}
MakeSynOM <- function(Upstream, Downstream) {
  x <- list(Upstream = Upstream, Downstream = Downstream)
  attr(x, "GRiwrm") <- gr_cn
  class(x) <- c("GRiwrmOutputsModel", "list")
  return(x)
}
synAreas <- c(360, 360)
# Expected area-weighted mean of a layer series of the 2 nodes
synMean <- function(OM, field, i) {
  meteo <- cbind(
    Upstream = OM[["Upstream"]]$CemaNeigeLayers[[i]][[field]],
    Downstream = OM[["Downstream"]]$CemaNeigeLayers[[i]][[field]]
  )
  rowSums(sweep(meteo, 2, synAreas, FUN = `*`)) / sum(synAreas)
}

test_that("adjustMeteoToCatchment adjusts the CemaNeige layer series (#178)", {
  # The layer series of CemaNeige are available by default on a CemaNeige node
  # (see CreateRunOptions)
  expect_true(all(
    c("Temp", "SnowPack", "Psol") %in%
      names(OM_cn[["Downstream"]]$CemaNeigeLayers[[1]])
  ))
  # The fixture replicates the same meteorological data on its 3 nodes, so the
  # layer series simulated on Upstream and Downstream are identical: the layer
  # series of Upstream are shifted here (a plausible +10 % and +1 mm/degree,
  # capped at 0 for Psol and SnowPack) so that the area-weighted mean differs
  # from the series of Downstream itself. A silent no-op adjustment would
  # therefore fail the assertions below. The shift is applied to a local copy
  # of `OM_cn` so that the other tests keep the unmodified fixture.
  OM_cn_shifted <- OM_cn
  for (field in c("Temp", "SnowPack", "Psol")) {
    for (i in seq_along(OM_cn_shifted[["Upstream"]]$CemaNeigeLayers)) {
      shifted <- OM_cn_shifted[["Upstream"]]$CemaNeigeLayers[[i]][[field]] *
        1.1 +
        1
      if (field %in% c("Psol", "SnowPack")) {
        shifted <- pmax(shifted, 0)
      }
      OM_cn_shifted[["Upstream"]]$CemaNeigeLayers[[i]][[field]] <- shifted
    }
  }
  # The 2 nodes now carry different layer series
  expect_false(isTRUE(all.equal(
    OM_cn_shifted[["Upstream"]]$CemaNeigeLayers[[1]]$Temp,
    OM_cn_shifted[["Downstream"]]$CemaNeigeLayers[[1]]$Temp
  )))
  # The exported function works with the default `griwrm` argument read from
  # the "GRiwrm" attribute of the GRiwrmOutputsModel object
  OM_cn_adj <- adjustMeteoToCatchment(
    OM_cn_shifted[["Downstream"]],
    "Downstream",
    OM_cn_shifted
  )
  OM_cn_adj_explicit <- adjustMeteoToCatchment(
    OM_cn_shifted[["Downstream"]],
    "Downstream",
    OM_cn_shifted,
    gr_cn
  )
  expect_identical(OM_cn_adj, OM_cn_adj_explicit)
  expect_length(OM_cn_adj$CemaNeigeLayers, 5)
  # Local sub-basin areas (the two nodes have the same local area 360 km2)
  areas <- c(
    Upstream = getNodeLocalArea("Upstream", gr_cn),
    Downstream = getNodeLocalArea("Downstream", gr_cn)
  )
  for (field in c("Temp", "SnowPack", "Psol")) {
    for (i in seq_along(OM_cn_adj$CemaNeigeLayers)) {
      meteo <- cbind(
        Upstream = OM_cn_shifted[["Upstream"]]$CemaNeigeLayers[[i]][[field]],
        Downstream = OM_cn_shifted[["Downstream"]]$CemaNeigeLayers[[i]][[
          field
        ]]
      )
      expected <- rowSums(sweep(meteo, 2, areas, FUN = `*`)) / sum(areas)
      expect_equal(
        OM_cn_adj$CemaNeigeLayers[[i]][[field]],
        expected,
        info = paste("layer", i, "field", field)
      )
      # The layer series of the 2 nodes differ, so the weighted mean differs
      # from the series of the node Downstream itself
      expect_false(isTRUE(all.equal(
        OM_cn_adj$CemaNeigeLayers[[i]][[field]],
        OM_cn_shifted[["Downstream"]]$CemaNeigeLayers[[i]][[field]]
      )))
    }
  }
  # The other items of the OutputsModel object are preserved
  expect_identical(OM_cn_adj$DatesR, OM_cn_shifted[["Downstream"]]$DatesR)
  expect_identical(OM_cn_adj$Qsim, OM_cn_shifted[["Downstream"]]$Qsim)
})

test_that("adjustMeteoToCatchment computes the weighted mean of the CemaNeige layers (#178)", {
  OM_syn <- MakeSynOM(
    Upstream = MakeSynNode(c(0, 10, 20)),
    Downstream = MakeSynNode(c(100, 110, 120))
  )
  adjusted <- adjustMeteoToCatchment(
    OM_syn[["Downstream"]],
    "Downstream",
    OM_syn
  )
  Fields <- c("Temp", "SnowPack", "Psol")
  for (field in Fields) {
    for (i in seq_len(3)) {
      expected <- synMean(OM_syn, field, i)
      expect_equal(
        adjusted$CemaNeigeLayers[[i]][[field]],
        expected
      )
      # The mean is really the mean of the 2 nodes: it differs from the
      # series of the node Downstream itself
      expect_false(isTRUE(all.equal(
        adjusted$CemaNeigeLayers[[i]][[field]],
        OM_syn[["Downstream"]]$CemaNeigeLayers[[i]][[field]]
      )))
    }
  }
  # `Precip` and `PotEvap` are not available in this object: they are left
  # unchanged without any error
  expect_null(adjusted$Precip)
  expect_null(adjusted$PotEvap)
})

test_that("adjustMeteoToCatchment is exported and callable as a public API (#178)", {
  # `adjustMeteoToCatchment` is part of the public API of the package
  expect_true("adjustMeteoToCatchment" %in% getNamespaceExports("airGRiwrm"))
  expect_true(is.function(adjustMeteoToCatchment))
  Args <- names(formals(adjustMeteoToCatchment))
  expect_identical(Args, c("OM_id", "id", "OutputsModel", "griwrm"))
  # The example of the documentation of the function
  PublicOM <- adjustMeteoToCatchment(OM_cn[["Downstream"]], "Downstream", OM_cn)
  expect_length(PublicOM$CemaNeigeLayers, 5)
})

test_that("adjustMeteoToCatchment leaves the CemaNeige layers of a head node unchanged (#178)", {
  # Upstream has no upstream node: the adjustment is a no-op (graceful
  # behaviour, same as for Precip and PotEvap)
  expect_identical(
    adjustMeteoToCatchment(OM_cn[["Upstream"]], "Upstream", OM_cn),
    OM_cn[["Upstream"]]
  )
})

test_that("adjustMeteoToCatchment handles a null local sub-basin area (#178)", {
  # A chain whose downstream node has a null local sub-basin area (no node can
  # contribute to its weighted mean) and whose upstream node has a null local
  # area as well (so it cannot contribute to the mean of the downstream node)
  g_null <- CreateGRiwrm(data.frame(
    id = c("Upstream", "Downstream"),
    down = c("Downstream", NA),
    length = c(0, NA),
    area = c(360, 360),
    model = rep("RunModel_CemaNeigeGR4J", 2),
    stringsAsFactors = FALSE
  ))
  expect_equal(getNodeLocalArea("Downstream", g_null), 0)
  OM_null <- MakeSynOM(
    Upstream = MakeSynNode(c(0, 10, 20)),
    Downstream = MakeSynNode(c(100, 110, 120))
  )
  attr(OM_null, "GRiwrm") <- g_null
  # No node can contribute to the mean of Downstream: its series are unchanged
  res_null <- adjustMeteoToCatchment(
    OM_null[["Downstream"]],
    "Downstream",
    OM_null
  )
  expect_identical(res_null, OM_null[["Downstream"]])
  # Upstream cannot contribute to the mean of Downstream: the mean falls back
  # on the series of Downstream itself
  meteo <- cbind(Downstream = OM_null[["Downstream"]]$CemaNeigeLayers[[1]]$Temp)
  expect_equal(
    catchmentMeteoMean(g_null, "Downstream", meteo),
    OM_null[["Downstream"]]$CemaNeigeLayers[[1]]$Temp
  )
})

test_that("adjustMeteoToCatchment does not mutate the object of origin (#178)", {
  OM_cn_ref <- OM_cn
  invisible(adjustMeteoToCatchment(OM_cn[["Downstream"]], "Downstream", OM_cn))
  # No item of the GRiwrmOutputsModel object, including the CemaNeige layer
  # series, is modified
  expect_identical(OM_cn, OM_cn_ref)
})

test_that("adjustMeteoToCatchment is graceful with heterogeneous CemaNeige layers (#178)", {
  # A node without CemaNeige (e.g. a GR4J node): its series are left unchanged
  OM_mix <- MakeSynOM(
    Upstream = c(
      list(DatesR = synDates, Qsim = rep(1, synTS)),
      list(CemaNeigeLayers = NULL)
    ),
    Downstream = MakeSynNode(c(100, 110, 120))
  )
  expect_identical(
    adjustMeteoToCatchment(OM_mix[["Upstream"]], "Upstream", OM_mix),
    OM_mix[["Upstream"]]
  )
  adjusted_mix <- adjustMeteoToCatchment(
    OM_mix[["Downstream"]],
    "Downstream",
    OM_mix
  )
  # Upstream does not contribute any layer series: the weighted mean is
  # computed on the contributing nodes only (here Downstream itself)
  expect_equal(
    adjusted_mix$CemaNeigeLayers[[1]]$Temp,
    OM_mix[["Downstream"]]$CemaNeigeLayers[[1]]$Temp
  )
  # A node with fewer layers than the node of interest does not contribute to
  # the upper layer indices: the adjustment stays on the available layers
  OM_few <- MakeSynOM(
    Upstream = MakeSynNode(c(0, 10)),
    Downstream = MakeSynNode(c(100, 110, 120, 130, 140))
  )
  adjusted_few <- adjustMeteoToCatchment(
    OM_few[["Downstream"]],
    "Downstream",
    OM_few
  )
  expect_length(adjusted_few$CemaNeigeLayers, 5)
  # Layers 1 and 2 are the weighted means of the 2 nodes
  expected_few <- OM_few[["Downstream"]]$CemaNeigeLayers
  for (field in c("Temp", "SnowPack", "Psol")) {
    for (i in 1:2) {
      expected_few[[i]][[field]] <- synMean(OM_few, field, i)
    }
  }
  expect_equal(adjusted_few$CemaNeigeLayers, expected_few)
  # Upstream does not have layers 3 to 5: the series of Downstream is
  # unchanged for these layers
  for (i in 3:5) {
    expect_equal(
      adjusted_few$CemaNeigeLayers[[i]],
      OM_few[["Downstream"]]$CemaNeigeLayers[[i]]
    )
  }
  # A contributor with only some of the fields (e.g. a node whose simulation
  # did not request `Psol`) does not contribute to this field
  OM_partial <- MakeSynOM(
    Upstream = MakeSynNode(c(0, 10, 20), fields = c("Temp", "SnowPack")),
    Downstream = MakeSynNode(c(100, 110, 120))
  )
  adjusted_partial <- adjustMeteoToCatchment(
    OM_partial[["Downstream"]],
    "Downstream",
    OM_partial
  )
  expect_equal(
    adjusted_partial$CemaNeigeLayers[[1]]$Temp,
    synMean(OM_partial, "Temp", 1)
  )
  expect_equal(
    adjusted_partial$CemaNeigeLayers[[1]]$SnowPack,
    synMean(OM_partial, "SnowPack", 1)
  )
  # `Psol` is only available on Downstream: its series is unchanged
  expect_equal(
    adjusted_partial$CemaNeigeLayers[[1]]$Psol,
    OM_partial[["Downstream"]]$CemaNeigeLayers[[1]]$Psol
  )
})

test_that("plot.GRiwrmOutputsModel displays the CemaNeige series at the catchment scale (#178)", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  # The whole default dashboard of a CemaNeige node is displayed with the
  # default `Outputs_Sim`: CemaNeige nodes also carry `Precip` (forced by
  # CreateRunOptions, see there), so airGR's monthly regime panel, which
  # requires `Precip` in the OutputsModel as soon as the per-layer `Psol`
  # is present (upstream airGR `ylimP` limitation), can be drawn without
  # any workaround. Note that the standalone airGR bug still exists on the
  # airGR side (the fix is being prepared there): without `Precip` in the
  # OutputsModel, the `plot.OutputsModel` of a CemaNeige node crashes with
  # "object 'ylimP' not found". The default Outputs_Sim of airGRiwrm makes
  # the affected path unreachable (before the mitigation, only the
  # `which = "ts"` and `which = "Temp"` panels, or a run with
  # `Outputs_Sim = "all"`, could be displayed on a CemaNeige node).
  expect_no_error(suppressWarnings(plot(OM_cn, unit = "mm", which = "synth")))
  expect_no_error(suppressWarnings(plot(OM_cn, unit = "mm", which = "all")))
})

