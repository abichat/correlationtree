context("Correlations")

X <- c(1, -1, 0, 5, 0, 4, 0, 3)
Y <- c(3, -2, 4, 0, 0, 4, 0, 8)
Z <- c(-1, 2, 0, 0, 2, 2, 1, 3)

x <- c(1, -1, 0, 5, 4, 3)
y <- c(3, -2, 4, 0, 4, 8)

A <- rep(0, 8)
B <- rep(1, 8)
mat_sd0 <- matrix(c(X, Y, Z, A, B), byrow = TRUE, ncol = 8)

#### cor_wo_shared_zero() ####

test_that("cor_wo_shared_zero() is like cor()", {
  expect_equal(cor_wo_shared_zero(X, Y), cor(x, y))
  expect_equal(cor_wo_shared_zero(X, Y, method = "spearman"),
               cor(x, y, method = "spearman"))
})

test_that("cross_cor(.) is a distance", {
  expect_is(cross_cor(list(X, Y, Z)), "dist")
  expect_equal(length(cross_cor(list(X, Y, Z))), 3)
})

#### cross_cor() ####

test_that("cross_cor() is like cor()", {
  expect_equal(cross_cor(list(X, Y, Z), method = "kendall")[1],
               cor(x, y, method = "kendall"))
  expect_equal(cross_cor(list(X, Y, Z), remove = FALSE)[3], cor(Y, Z))
  expect_equal(cross_cor(list(X, Y, Z, Z^2), remove = FALSE)[3], cor(X, Z^2))
})

test_that("cross_cor() keeps names", {
  expect_equal(labels(cross_cor(list(a = X, b = Y, c = Z))), letters[1:3])
  expect_equal(labels(cross_cor(list(X, Y, Z))), paste0("vec", 1:3))
})


#### correlation_tree() ####

test_that("correlation_tree(.) is a phylo", {
  expect_is(correlation_tree(iris, col = 5), "phylo")
  expect_is(correlation_tree(swiss, col = 0), "phylo")
  expect_is(correlation_tree(swiss, matrix = TRUE), "phylo")
})

test_that("correlation_tree() keeps names", {
  expect_equal(length(correlation_tree(iris, col = 5)$tip.label), nrow(iris))
  expect_equal(sort(correlation_tree(swiss, col = 0)$tip.label),
               sort(rownames(swiss)))
  expect_equal(sort(correlation_tree(swiss, matrix = TRUE)$tip.label),
               sort(rownames(swiss)))
})

test_that("correlation_tree() fills correctly", {
  expect_error(suppressWarnings(correlation_tree(mat_sd0, matrix = TRUE)))
  expect_warning(correlation_tree(mat_sd0, matrix = TRUE, fill = TRUE))
})

# Add tests on heights and distances
