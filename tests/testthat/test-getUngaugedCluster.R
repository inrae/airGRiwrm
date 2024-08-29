test_that("Reservoir between ungauged and gauged node should be in ungauged cluster", {
  # Reservoir between Ungauged and gauged nodes
  n_rsrvr$model[n_rsrvr$id == "54095"] <- "Ungauged"
  g <- CreateGRiwrm(n_rsrvr) # Network provided by helper_RunModel_Reservoir.R
  expect_equal(unique(g$donor), "54001")
})

test_that("Reservoir supplied by derivated ungauged node should be included in the ungauged cluster", {
  # Reservoir between Ungauged and gauged nodes
  g <- CreateGRiwrm(n_derived_rsrvr) # Network provided by helper_RunModel_Reservoir.R
  expect_equal(g$donor[g$id == "Dam"], "54001")
  g2 <- getUngaugedCluster(g, "54001")
  expect_equal(g2$id, c("54095", "54095", "Dam", "54001"))
  expect_equal(g2$model[g2$id == "Dam"], "RunModel_Reservoir")
})


test_that("Reservoir and Diversion on reservoir should be in ungauged cluster", {
  nodes <- n_rsrvr
  nodes[nodes$id == "Dam", c("down", "length")] <- NA
  nodes$model[nodes$id == "54095"] <- "Ungauged"
  nodes <- rbind(nodes,
                 data.frame(id = "Dam", down = "54001", length = 42, area = NA, model = "Diversion"))
  g <- CreateGRiwrm(nodes)
  g2 <- getUngaugedCluster(g, "54001")
  expect_equal(g2$id,
               c("54095", "Dam", "Dam", "54001"))
})

