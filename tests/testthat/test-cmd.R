context("CMD")

clades <- c("k__Bacteria", "k__Bacteria|p__Proteobacteria",
            "k__Bacteria|p__Verrucomicrobia",
            "k__Bacteria|p__Firmicutes|c__Clostridia",
            "k__Bacteria|p__Verrucomicrobia|c__Verrucomicrobiae|o__Verrucomicrobiales",
            "k__Bacteria|p__Firmicutes|c__Bacilli|o__Lactobacillales|f__Streptococcaceae",
            "k__Bacteria|p__Bacteroidetes|c__Bacteroidia|o__Bacteroidales|f__Rikenellaceae|g__Alistipes",
            "k__Bacteria|p__Actinobacteria|c__Actinobacteria|o__Coriobacteriales|f__Coriobacteriaceae|g__Collinsella",
            "k__Bacteria|p__Actinobacteria|c__Actinobacteria|o__Actinomycetales|f__Micrococcaceae|g__Rothia|s__Rothia_dentocariosa",
            "k__Bacteria|p__Proteobacteria|c__Betaproteobacteria|o__Burkholderiales|f__Comamonadaceae|g__Delftia|s__Delftia_unclassified"
)


df <- data.frame(Clade = clades,
                 A = c(23417118, 3695, 0, 0, 0, 1768, 0, 0, 0, 0),
                 B = c(54804726, 10988, 0, 39116, 0, 1724, 0, 0, 209, 0),
                 C = c(19987699, 135019, 0, 0, 0, 0, 0, 0, 0, 0))

#### filter_cmd_level() ####

test_that("filter_cmd_level() returns the correct output",{
  expect_equal(ncol(filter_cmd_level(df, level = "kingdom")), ncol(df))
  expect_equal(colnames(filter_cmd_level(df, level = "phylum")), colnames(df))
  expect_equal(class(filter_cmd_level(df, level = "class")), class(df))
})

test_that("filter_cmd_level() with bad level returns an error",{
  expect_error(ncol(filter_cmd_level(df, level = "Species")))
})

test_that("filter_cmd_level() returns the correct output",{
  expect_equal(filter_cmd_level(df, level = "order"), dplyr::slice(df, 5))
  expect_equal(filter_cmd_level(df, level = "genus")$Clade, df[7:8, 1])
  expect_equal(filter_cmd_level(df, level = "species"), dplyr::slice(df, 9:10))
})
