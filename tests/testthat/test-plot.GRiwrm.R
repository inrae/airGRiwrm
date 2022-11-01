# Setup a simple data.frame for GRiwrm
nodes <- loadSevernNodes()

test_that("Diverted ungauged nodes have correct color", {
  nodes_div <- nodes
  nodes_div$model[nodes_div$id == "54029"] <- "Ungauged"
  nodes_div <- rbind(nodes_div, data.frame(id = "54029",
                                           down = "54001",
                                           length = 20,
                                           model = "Diversion",
                                           area = NA))
  griwrm_div <- CreateGRiwrm(nodes_div)
  mmd <- plot(griwrm_div, display = FALSE)
  expect_true(any(grepl("id_54029 UpstreamUngaugedDiversion", strsplit(mmd, "\n\n")[[1]])))
})
