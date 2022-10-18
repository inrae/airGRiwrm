nodes <- loadSevernNodes()

test_that("Check ranking on Severn example", {
  g <- CreateGRiwrm(nodes)
  expect_equal(getNodeRanking(g),
               c("54095", "54002", "54029", "54001", "54032", "54057"))
})

test_that("Check ranking with direct injection node", {
  nodes$model[nodes$id == "54029"] <- NA
  g <- CreateGRiwrm(nodes)
  expect_equal(getNodeRanking(g),
               c("54095", "54002", "54001", "54032", "54057"))
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
})

