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
