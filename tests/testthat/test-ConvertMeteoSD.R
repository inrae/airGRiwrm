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

griwrm <-
  data.frame(
    id = c("Up", "Down"),
    down = c("Down", NA),
    area = c(1, 2),
    stringsAsFactors = FALSE
  )
class(griwrm) <- c("GRiwrm", class(griwrm))

test_that("Downstream data should return 2", {
  resultMatrix[,] <- 2
  meteo <- matrix(rep(c(1, 1.5), dataNumRows), ncol = 2, byrow = TRUE)
  colnames(meteo) <- c("Up", "Down")
  expect_equivalent(
    ConvertMeteoSD(griwrm, meteo),
    matrix(rep(c(1,2),dataNumRows), byrow = TRUE, ncol = 2)
  )
})
