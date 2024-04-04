nodes <- loadSevernNodes()
nodes <- rbind(nodes,
               data.frame(id = "54032", down = "54002", length = 30, area = NA, model = "Diversion"))
g <- CreateGRiwrm(nodes)

test_that("isNodeDownstream works", {
  expect_true(isNodeDownstream(g, "54095", "54057"))
  expect_true(isNodeDownstream(g, "54032", "54002"))
  expect_true(isNodeDownstream(g, "54029", "54002"))
  expect_false(isNodeDownstream(g, "54095", "54029"))
})

test_that("isNodeupstream works", {
  expect_true(isNodeUpstream(g, "54057", "54095"))
  expect_true(isNodeUpstream(g, "54002", "54032"))
  expect_true(isNodeUpstream(g, "54002", "54029"))
  expect_false(isNodeUpstream(g, "54029", "54095"))
})

test_that("Ungauged clusters are processed when all upstream nodes are processed", {
  g <- CreateGRiwrm(read.csv(test_path("test-utils.GRiwrm_issue_149.csv")))
  expect_equal(getNodeRanking(g), c("G1", "U1", "G2", "U2", "U3", "G3"))
})
