# Setup a simple data.frame for GRiwrm
nodes <- loadSevernNodes()

test_that("All nodes should have property: Diversion=FALSE", {
  griwrm <- CreateGRiwrm(nodes)
  diversionProperty <- sapply(griwrm$id,
                              function(id) getNodeProperties(id, griwrm)$Diversion)
  expect_equal(all(diversionProperty), FALSE)
})

test_that("Ungauged station has 'hydrology:Ungauged' property", {
  nodes_div <- nodes
  nodes_div$model[nodes_div$id == "54029"] <- "Ungauged"
  nodes_div <- rbind(nodes_div, data.frame(id = "54029",
                                           down = "54001",
                                           length = 20,
                                           model = "Diversion",
                                           area = NA))
  griwrm_div <- CreateGRiwrm(nodes_div)
  expect_equal(getNodeProperties("54029", griwrm_div)$hydrology, "Ungauged")
})
