test_that("Checks on GRiwrm object with Runmodel_Reservoir", {
  db <- data.frame(id = c("Reservoir", "GaugingDown"),
                   length = c(1, NA),
                   down = c("GaugingDown", NA),
                   area = c(NA, 1),
                   model = c("RunModel_Reservoir", "RunModel_GR4J"),
                   stringsAsFactors = FALSE)
  expect_error(CreateGRiwrm(db),
               regexp = "upstream node")
})
