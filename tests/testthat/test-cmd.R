context("CMD")

clades <- c("k__Bacteria",
            "k__Bacteria|p__Proteobacteria",
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

test_that("filter_cmd_level() is deprecated", {
  expect_error(filter_cmd_level(df, level = "kingdom"))
})


#### compute_cmd_taxtable() ####

test_that("compute_cmd_taxtable() is deprecated", {
  expect_error(compute_cmd_taxtable(clades[2:3]))
})


#### clean_cmd_clade() ####

test_that("clean_cmd_clade() is deprecated", {
  expect_error(clean_cmd_clade(clades[6]))
})


#### taxtable2tree() ####

test_that("clean_cmd_clade() is deprecated", {
  expect_error(taxtable2tree())
})


