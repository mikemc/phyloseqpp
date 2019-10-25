data(GlobalPatterns)
ps <- subset_taxa(GlobalPatterns, Phylum == "Chlamydiae")

test_that("functions give same results as phyloseq versions", {
    ps1 <- ps %>%
        filter_taxa2(~ sum(. > 0) > 5) %>%
        transform_sample_counts(~ . / sum(.))
    ps2 <- ps %>%
        phyloseq::filter_taxa(function(x) sum(x > 0) > 5, prune = TRUE) %>%
        phyloseq::transform_sample_counts(function(x) x / sum(x))
    expect_equal(ps1, ps2)
    taxa1 <- ps %>%
        filter_taxa(~ sum(. > 0) > 5)
    taxa2 <- ps %>%
        phyloseq::filter_taxa(function(x) sum(x > 0) > 5)
    expect_equal(taxa1, taxa2)
})
