data(GlobalPatterns)
ps <- subset_taxa(GlobalPatterns, Phylum == "Chlamydiae")

test_that("correct `as_tibble()` method is dispatched after tibble is loaded", {
    # Tests that Issue #17 has been resolved
    samtb <- sample_data(GlobalPatterns) %>% as_tibble
    library(tibble)
    expect_equal(sample_data(GlobalPatterns) %>% as_tibble, samtb)
})

test_that("conversion from phyloseq objects to tibbles and back leaves objects unchanged", {
    expect_identical(
        otu_table(ps), 
        otu_table(ps) %>% as_tibble %>% otu_table(taxa_are_rows(ps))
    )
    expect_identical(
        sample_data(ps), 
        sample_data(ps) %>% as_tibble %>% sample_data
    )
    expect_identical(
        tax_table(ps), 
        tax_table(ps) %>% as_tibble %>% tax_table
    )
})
