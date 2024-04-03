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
