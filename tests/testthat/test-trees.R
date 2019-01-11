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

#### *lineage_length() ####

tree3 <- read.tree(text = "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);")
tree4 <- read.tree(text = "(A:0.9,B:0.9,(C:0.4,D:0.4):0.5);")

test_that("lineage_length() are correct", {
  expect_is(lineage_length(tree3, "A"), "numeric")
  expect_equal(lineage_length(tree3, "A"), 0.1)
  expect_equal(lineage_length(tree3, "C"), 0.8)
  expect_equal(mean_lineage_length(tree3), 0.5)
  expect_equal(mean_lineage_length(tree4), 0.9)
  expect_equal(mean_lineage_length(tree4, ultrametric = TRUE), 0.9)
})
