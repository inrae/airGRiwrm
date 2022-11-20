# Setup a simple data.frame for GRiwrm
nodes <- loadSevernNodes()

test_that("All nodes should have property: Diversion and DirectInjection == FALSE", {
  griwrm <- CreateGRiwrm(nodes)
  lapply(c("Diversion", "DirectInjection"), function(p) {
    prop_value <- sapply(griwrm$id,
                                function(id) getNodeProperties(id, griwrm)[[p]])
    expect_equal(all(prop_value), FALSE)
  })
  prop_hydrology <- sapply(griwrm$id,
                           function(id) getNodeProperties(id, griwrm)$hydrology)
  expect_equal(as.character(prop_hydrology), rep("Gauged", nrow(griwrm)))
})

test_that("Ungauged station has 'hydrology:Ungauged' property", {
  nodes$model[nodes$id == "54029"] <- "Ungauged"
  expect_equal(getNodeProperties("54029", CreateGRiwrm(nodes))$hydrology, "Ungauged")
})

test_that("Direct injection node has 'hydrology:DirectInjection' property", {
  nodes$model[nodes$id == "54002"] <- NA
  np <- getNodeProperties("54002", CreateGRiwrm(nodes))
  expect_equal(np$hydrology, "DirectInjection")
  expect_equal(np$DirectInjection, TRUE)
})

test_that("An upstream node outlet of a diversion should be intermediate node", {
  nodes_div <- nodes
  nodes_div$model[nodes_div$id == "54002"] <- NA
  nodes_div <- rbind(nodes_div, data.frame(id = "54001",
                                           down = "54029",
                                           length = 20,
                                           model = "Diversion",
                                           area = NA))
  griwrm_div <- CreateGRiwrm(nodes_div)
  np <- getNodeProperties("54029", griwrm_div)
  expect_equal(np$position, "Intermediate")
  expect_equal(np$Upstream, FALSE)
  dfNP <- getAllNodesProperties(griwrm_div)
  expect_s3_class(dfNP, "data.frame")

})
