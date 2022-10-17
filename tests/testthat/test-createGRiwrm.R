test_that("extra columns work (#64)", {
  text = "id_amont	lambert2.x	lambert2.y	area	nom	id_aval	distance_aval	model
H8100021	537912.994	2455749.314	64420.94	La Seine à Vernon	NA	NA	RunModel_CemaNeigeGR4J
H7900010	578113	2437649	61642.28	La Seine à Poissy	H8100021	76.28	RunModel_CemaNeigeGR4J
H5920010	602213	2427449	43824.66	La Seine à Paris [Austerlitz après création lacs]	H7900010	82.26	RunModel_CemaNeigeGR4J"

  BS_reseau <- read.csv(text = text, sep = "\t")

  expect_s3_class(CreateGRiwrm(
    BS_reseau,
    cols = list(
      id = "id_amont",
      down = "id_aval",
      length = "distance_aval"
    ),
    keep_all = TRUE
  ),
  "GRiwrm")

})

# Setup a simple data.frame for GRiwrm
data(Severn)
nodes <-
  Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
names(nodes) <- c("id", "down", "length", "area")
nodes$model <- "RunModel_GR4J"

test_that("Hydrological model nodes must have numeric area", {
  nodes$area[nodes$id == "54057"] <- NA
  expect_error(CreateGRiwrm(nodes),
               regexp = "hydrological")
})

test_that("Duplicated nodes",  {
  nodes <- rbind(nodes, nodes[4,])
  expect_error(CreateGRiwrm(nodes),
               regexp = "Duplicated nodes detected")
})

test_that("NA or Ungauged nodes at downstream should throw an error", {
  nodes$model[nodes$id == "54057"] <- "Ungauged"
  expect_error(CreateGRiwrm(nodes),
               regexp = "downstream node")
  nodes$model[nodes$gauge_id == "54057"] <- NA
  expect_error(CreateGRiwrm(nodes),
               regexp = "downstream node")
})

test_that("Diversion node", {
  nodes <- rbind(nodes,
                 data.frame(id = "54029", down = "54002", length = 50, area = NA, model = "Diversion"))
  expect_s3_class(CreateGRiwrm(nodes), "GRiwrm")
  n99 <- nodes
  n99$area[n99$model == "Diversion"] <- 99
  expect_error(CreateGRiwrm(n99),
               regexp = "Diversion node must have its area")
  nodes$id[nodes$model == "Diversion"] <- "54999"
  expect_error(CreateGRiwrm(nodes),
               regexp = "Diversion node must have the same `id` of")
})
