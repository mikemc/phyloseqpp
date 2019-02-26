# Methods for construct phyloseq objects from tibbles -------------------------

# #' Constructor for creating taxonomyTable from a tibble with first column
# #'
# #' @param name
# #' @param loc
# setMethod("tax_table", "tbl_df", function(object, name = "OTU", loc = 1) {
# 	# Warn first
#     text <- paste(sep = "\n",
#         "Coercing from tbl_df class to character matrix",
#         "prior to building taxonomyTable.",
#         "This could introduce artifacts.",
#         "Check your taxonomyTable, or coerce to matrix manually.")
#     warning(text)
#     # TODO: First verify that the column with name `name` is in location `loc`.
#     mat <- object %>%
#         select(-loc) %>%
#         as("matrix")
#     rownames(mat) <- object[[loc]]
# 	tax_table(mat)
# })

#' Constructor for creating otu_table from a tibble (class tbl_df)
#' 
#' Assumes that the first column is the OTU/taxon names or the sample names,
#' depending on taxa_are_rows
#' 
#' @export
setMethod("otu_table", "tbl_df", function (object, taxa_are_rows) {
    if (taxa_are_rows) {
        warning("Assuming first column contains the taxa_names()")
    } else {
        warning("Assuming first column contains the sample_names()")
    }
    mat <- object %>%
        select(-1) %>%
        as("matrix")
    rownames(mat) <- object[[1]]
	otu_table(mat, taxa_are_rows)
})

#' Constructor for creating sample_data from a tibble (class tbl_df)
#'
#' Assumes that the first column is the sample names that will become
#' `sample_names(physeq)`
#' 
#' Converts to a plain data.frame with row names = sample names and then passes
#' to phyloseq's sample_data
#' 
#' @export
setMethod("sample_data", "tbl_df", function(object) {
    warning("Assuming first column contains the sample_names()")
    df <- object %>% 
        dplyr::select(-1) %>%
        as.data.frame
    rownames(df) <- object[[1]]
    sample_data(df)
})

#' Constructor for creating taxonomyTable from a tibble (class tbl_df)
#' 
#' Assumes that the first column is the OTU/taxon names that will become
#' `taxa_names(physeq)`.
#' 
#' Note: Unlike phyloseq's method for data.frames, this does not coerce the
#' columns to character vectors; but will probably change this.
#' 
#' @export
setMethod("tax_table", "tbl_df", function (object) {
    warning("Assuming first column contains the taxa_names()")
    mat <- object %>%
        select(-1) %>%
        as("matrix")
    rownames(mat) <- object[[1]]
	tax_table(mat)
    # Don't use phyloseq's data.frame method, because it removes the 
    # df <- object %>% 
    #     dplyr::select(-1) %>%
    #     as.data.frame
    # rownames(df) <- object[[1]]
	# tax_table(df)
})

