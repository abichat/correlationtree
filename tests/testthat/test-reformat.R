context("Formatting")

#### df2mat() ####

mat_iris <- df2mat(iris, col = 5)
mat_swiss <- df2mat(swiss, col = 0)

test_that("df2mat(.) is a matrix", {
  expect_is(mat_iris, "matrix")
  expect_is(df2mat(mat_iris, col = 0), "matrix")
  expect_is(mat_swiss, "matrix")
})

test_that("df2mat(.) has the correct dimensions", {
  expect_equal(dim(mat_iris), dim(iris) - c(0, 1))
  expect_equal(dim(mat_swiss), dim(swiss))
})

test_that("df2mat() gives the correct output", {
  expect_equal(unname(mat_iris[, 1]), iris$Sepal.Length)
  expect_equal(unname(mat_iris[1, ]), c(iris$Sepal.Length[1],
                                        iris$Sepal.Width[1],
                                        iris$Petal.Length[1],
                                        iris$Petal.Width[1]))
  expect_equal(rownames(mat_iris), as.character(iris$Species))
  expect_equal(rownames(mat_swiss), rownames(swiss))
})

#### mat2list() ####

test_that("mat2list(.) is a list", {
  expect_is(mat2list(mat_iris), "list")
  expect_is(mat2list(mat_swiss), "list")
})

test_that("mat2list(.) has the correct dimensions", {
  expect_equal(length(mat2list(mat_iris)), nrow(iris))
  expect_equal(length(mat2list(mat_iris)[[1]]), ncol(iris) - 1)

  expect_equal(length(mat2list(mat_swiss)), nrow(swiss))
  expect_equal(length(mat2list(mat_swiss)[[1]]), ncol(swiss))
})

test_that("mat2list() gives the correct output", {
  expect_equal(mat2list(mat_iris)[[1]], c(iris$Sepal.Length[1],
                                          iris$Sepal.Width[1],
                                          iris$Petal.Length[1],
                                          iris$Petal.Width[1]))
})

#### sim2dist() ####

n <- 16
mat <- matrix(runif(n^2,-1, 1), ncol = n)
sim <- (mat + t(mat)) / 2
diag <- n * (seq_len(n) - 1) + seq_len(n)
sim[diag] <- 1

test_that("sim2dist() keeps dimensions", {
  expect_equal(dim(sim2dist(sim)), dim(sim))
  expect_equal(dim(sim2dist(sim, transformation = "1-absx")), dim(sim))
  expect_equal(dim(sim2dist(sim, transformation = function(x) -log(x^2))),
               dim(sim))
})
