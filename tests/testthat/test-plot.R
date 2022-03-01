test_that("plot.GRiwrm should have all styles correctly filled (#73)", {
  data(Severn)
  nodes <- Severn$BasinsInfo[, c("gauge_id", "downstream_id", "distance_downstream", "area")]
  nodes$distance_downstream <- nodes$distance_downstream #je ne comprends pas cette ligne, elle semble inutile
  nodes$model <- "RunModel_GR4J"
  griwrm <- CreateGRiwrm(nodes, list(id = "gauge_id", down = "downstream_id", length = "distance_downstream"))
  code_mermaid <-plot(griwrm, display = FALSE)
  expect_length(grep("style  fill", code_mermaid), 0)
})
