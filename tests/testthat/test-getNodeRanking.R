nodes <- loadSevernNodes()

test_that("Check ranking on Severn example", {
  g <- CreateGRiwrm(nodes)
  expected_rank <- c("54095", "54002", "54029", "54001", "54032", "54057")
  expect_equal(getNodeRanking(g), expected_rank)
  gs <- sort(g)
  expect_s3_class(gs, "GRiwrm")
  expect_equal(gs$id, expected_rank)
})

test_that("Check ranking with direct injection node", {
  nodes$model[nodes$id == "54029"] <- NA
  g <- CreateGRiwrm(nodes)
  expected_rank <- c("54095", "54002", "54001", "54032", "54057")
  expect_equal(getNodeRanking(g), expected_rank)
  gs <- sort(g)
  expect_equal(nrow(gs), nrow(g))
  expect_equal(gs$id, c(expected_rank, "54029"))
})

test_that("Check ranking with Diversion", {
  n_div <- rbind(nodes, data.frame(id = "54029",
                                   down = "54002",
                                   length = 20,
                                   model = "Diversion",
                                   area = NA))
  g <- CreateGRiwrm(n_div)
  r <- getNodeRanking(g)
  expect_lt(which(r == "54029"), which(r == "54002"))
  gs <- sort(g)
  expect_equal(nrow(gs), nrow(g))
  expect_equal(gs$id, c(r[1:2], r[2], r[3:6]))
})

test_that("Check ranking with Ungauged node, reservoir, and Diversion #130", {
  g <- getGriwrmDerivedReservoirUngauged(FALSE)
  expect_equal(getNodeRanking(g), c("54095", "54001", "Dam", "54029", "54032"))
})

test_that("Check ranking with Ungauged node, reservoir, and Diversion #130", {
  g <- getGriwrmDerivedReservoirUngauged(TRUE)
  expect_equal(getNodeRanking(g), c("54095", "Dam", "54029", "54001", "54032"))
})

test_that("Warning case detected: ungauged node cluster with diversion to a gauged upstream node of the donor", {
  nodes_div <- nodes
  nodes_div$model[nodes_div$id == "54029"] <- "Ungauged"
  nodes_div <- rbind(nodes_div, data.frame(id = "54029",
                                           down = "54001",
                                           length = 20,
                                           model = "Diversion",
                                           area = NA))
  expect_warning(CreateGRiwrm(nodes_div),
               regexp = "Node '54001' is included in the ungauged node cluster '54032'")
})

test_that("donor of ungauged cluster is processed before sibling ungauged nodes (#155)", {
  n155 <- data.frame(id   = c("UngSib", "UngUp", "Donor", "Down"),
                     down = c("Down",   "Donor", "Down", NA),
                     length = c(rep(1, 3), NA),
                     model = c("Ungauged", "Ungauged", "RunModel_GR4J", "RunModel_GR4J"),
                     area = rep(1,4),
                     donor = c("Donor", NA, NA, NA))
  g155 <- CreateGRiwrm(n155)
  expect_equal(getNodeRanking(g155), c("UngUp", "Donor", "UngSib", "Down"))
})
