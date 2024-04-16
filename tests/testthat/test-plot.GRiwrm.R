test_that("Diverted ungauged nodes have correct color", {
  nodes_div <- loadSevernNodes()
  nodes_div$model[nodes_div$id == "54029"] <- "Ungauged"
  nodes_div <- rbind(nodes_div, data.frame(id = "54029",
                                           down = "54002",
                                           length = 20,
                                           model = "Diversion",
                                           area = NA))
  griwrm_div <- CreateGRiwrm(nodes_div)
  mmd <- plot(griwrm_div, display = FALSE)
  expect_true(any(grepl("id_54029 UpstreamUngaugedDiversion", strsplit(mmd, "\n\n")[[1]])))
})

test_that("Unguaged nodes and donors are in a box!", {
  nds <- loadSevernNodes()
  nds$donor <- as.character(NA)
  nds$model[nds$id %in% c("54001", "54032", "54029")] <- "Ungauged"
  nds$donor[nds$id %in% c("54001", "54029")] <- "54095"
  g <- CreateGRiwrm(nds)
  expect_s3_class(sortGRiwrm4plot(g), "GRiwrm")
  expect_equal(sortGRiwrm4plot(g)$id,
               c("54095", "54001", "54029", "54002", "54032", "54057"))
  s <- plot(g, display = FALSE)
  expect_equal(strsplit(s, "\n")[[1]][c(3,11)], c("subgraph donor_54095 [54095]", "end"))
})
