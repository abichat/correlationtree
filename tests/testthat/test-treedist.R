context("BHV distance")

library(ape)
library(distory)


trees <- c(rtree(10), rtree(10), rtree(10), rtree(10), rtree(10))


test_that("future_dist_BHV is correct", {
  expect_is(future_dist_BHV(trees), "dist")
  expect_equal(length(future_dist_BHV(trees)), 5*4/2)
  expect_equal(as.matrix(future_dist_BHV(trees)),
               as.matrix(dist.multiPhylo(trees)))
})

