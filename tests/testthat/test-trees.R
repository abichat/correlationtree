context("Trees")
# library(ape)

tree1 <- rtree(16)
tree2 <- shuffle_tiplabels(tree1)

#### shuffle_tiplabels() ####

test_that("shuffle_tiplabels() change only tip labels",{
  expect_equal(tree2$edge, tree1$edge)
  expect_equal(tree2$edge.length, tree1$edge.length)
  expect_equal(tree2$Nnode, tree1$Nnode)
  expect_equal(sort(tree2$tip.label), sort(tree1$tip.label))
})
