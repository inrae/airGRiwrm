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
  g <- getGriwrmDerivedReservoirUngauged(TRUE, inconsistent = TRUE)
  expect_error(getNodeRanking(g), regexp = "Inconstancy")
  g <- getGriwrmDerivedReservoirUngauged(TRUE, inconsistent = FALSE)
  expect_equal(getNodeRanking(g), c("54095", "Dam", "54029", "54001", "54032"))
})

test_that("Impossible case detected: ungauged node with diversion to an upstream node of the donor", {
  nodes_div <- nodes
  nodes_div$model[nodes_div$id == "54029"] <- "Ungauged"
  nodes_div <- rbind(nodes_div, data.frame(id = "54029",
                                           down = "54001",
                                           length = 20,
                                           model = "Diversion",
                                           area = NA))
  expect_error(CreateGRiwrm(nodes_div))
})
