context("ConvertMeteoSD")

dataNumRows <- 2

test_that("Error: Meteo data should contain more than 1 row", {
  expect_error(
    ConvertMeteoSD(matrix(rep(c(1,1), 1), ncol = 2, byrow = TRUE), c(2,1)),
    regexp = "should contain more than one row"
  )
})

test_that("Error: Basin area too small", {
  expect_error(
    ConvertMeteoSD(matrix(rep(c(1,1), dataNumRows), ncol = 2, byrow = TRUE), c(1,1)),
    regexp = "should be greater than the sum"
  )
})

test_that("Error: Meteo data and areas dimensions not coherent", {
  expect_error(
    ConvertMeteoSD(matrix(rep(c(1,1), dataNumRows), ncol = 2, byrow = TRUE), c(1,1,1)),
    regexp = "number of columns should be equal"
  )
})

resultMatrix = matrix(rep(0,dataNumRows), ncol = 1)

test_that("No upstream basin should return input", {
  resultMatrix[,] <- 1
  expect_equal(
    ConvertMeteoSD(matrix(rep(c(1), dataNumRows), ncol = 1, byrow = TRUE), c(1)),
    resultMatrix
  )
})

test_that("Same inputs should return same value", {
  resultMatrix[,] <- 1
  expect_equal(
    ConvertMeteoSD(matrix(rep(c(1,1), dataNumRows), ncol = 2, byrow = TRUE), c(2,1)),
    resultMatrix
  )
  expect_equal(
    ConvertMeteoSD(matrix(rep(c(1,1,1), dataNumRows), ncol = 3, byrow = TRUE), c(10,1,1)),
    resultMatrix
  )
})

test_that("Downstream data should return 2", {
  resultMatrix[,] <- 2
  expect_equal(
    ConvertMeteoSD(matrix(rep(c(1.5,1), dataNumRows), ncol = 2, byrow = TRUE), c(2,1)),
    resultMatrix
  )
})

test_that("Downstream data should return 0", {
  expect_equal(
    ConvertMeteoSD(matrix(rep(c(1,2), dataNumRows), ncol = 2, byrow = TRUE), c(2,1)),
    resultMatrix
  )
})

nodes <-
  data.frame(
    id = c("Up", "Down"),
    down = c("Down", NA),
    area = c(1, 2),
    length = c(0, NA),
    model = "RunModel_GR4J",
    stringsAsFactors = FALSE
  )
griwrm <- CreateGRiwrm(nodes)

test_that("getUpstreamRunoffIds works", {
  expect_equal(getUpstreamRunOffIds("Up", griwrm), "Up")
  expect_equal(getUpstreamRunOffIds("Down", griwrm), c("Down", "Up"))

  nodes <-
    data.frame(
      id = c("Up1", "Up2", "Inter", "Up3", "Down"),
      down = c("Inter", "Inter", "Down", "Down", NA),
      area = c(1.1, 1.2, NA, 2.1, 5),
      length = c(0, 0, 0, 0, NA),
      model = "RunModel_GR4J",
      stringsAsFactors = FALSE
    )
  nodes$model[3] <- NA
  griwrm <- CreateGRiwrm(nodes)

  expect_equal(sort(getUpstreamRunOffIds("Down", griwrm)), sort(nodes$id[nodes$id != "Inter"]))
})

test_that("Downstream data should return 2", {
  meteo <- matrix(rep(c(1, 1.5), dataNumRows), ncol = 2, byrow = TRUE)
  colnames(meteo) <- c("Up", "Down")
  expect_equivalent(
    ConvertMeteoSD(griwrm, meteo),
    matrix(rep(c(1,2),dataNumRows), byrow = TRUE, ncol = 2)
  )
})

test_that("Downstream temperature should return -2", {
  meteo <- matrix(rep(c(-1, -1.5), dataNumRows), ncol = 2, byrow = TRUE)
  colnames(meteo) <- c("Up", "Down")
  expect_equivalent(
    ConvertMeteoSD(griwrm, meteo, temperature = TRUE),
    matrix(rep(c(-1,-2),dataNumRows), byrow = TRUE, ncol = 2)
  )
})
