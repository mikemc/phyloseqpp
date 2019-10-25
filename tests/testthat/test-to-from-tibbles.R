data(GlobalPatterns)
ps <- subset_taxa(GlobalPatterns, Phylum == "Chlamydiae")

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
