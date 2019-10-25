# Methods for construct phyloseq objects from tibbles -------------------------

#' Constructor for creating otu_table from a tibble (class tbl_df)
#' 
#' If taxa are rows, assumes that the first column is the OTU/taxon names. If
#' taxa are columns, assumes that the first column is the sample names.
#' 
#' @export
#'
#' @seealso 
#' phyloseq::otu_table
#' 
#' \dontrun{
#' # Read a .csv file with readr, which creates an object of class `tbl_df`
#' tbl <- readr::read_csv("path/to/otu_table.csv")
#' # Inspect and check if taxa are rows and that the first column contains the
#' # sample names or the taxa/OTU names
#' head(tbl) 
#' # Create a phyloseq `otu_table` object
#' otu <- otu_table(tbl, taxa_are_rows = FALSE)
#' }
setMethod("otu_table", "tbl_df", function (object, taxa_are_rows) {
    if (taxa_are_rows) {
        message(paste0("Assuming first column, `", names(object)[1], 
                "`, contains the taxa names"))
    } else {
        message(paste0("Assuming first column, `", names(object)[1], 
                "`, contains the sample names"))
    }
    mat <- object %>%
        dplyr::select(-1) %>%
        as("matrix")
    rownames(mat) <- object[[1]]
	otu_table(mat, taxa_are_rows)
})

#' Constructor for creating sample_data from a tibble (class tbl_df)
#'
#' Assumes that the first column is the sample names.
#' 
#' This function creates a plain `data.frame` from `object` that contains all
#' columns except the first one, which is used for the row names. The resulting
#' data frame is then passed to `sample_data()`.
#' 
#' @export
#'
#' @seealso 
#' phyloseq::sample_data
#' 
#' \dontrun{
#' # Read a .csv file with readr, which creates an object of class `tbl_df`
#' tbl <- readr::read_csv("path/to/sample_data.csv")
#' # Inspect and check that the first column contains the sample names
#' head(tbl) 
#' # Create a phyloseq `sample_data` object
#' sam <- sample_data(tbl)
#' }
setMethod("sample_data", "tbl_df", function(object) {
    message(paste0("Assuming first column, `", names(object)[1], 
            "`, contains the sample names"))
    df <- object %>% 
        dplyr::select(-1) %>%
        as.data.frame
    rownames(df) <- object[[1]]
    sample_data(df)
})

#' Constructor for creating taxonomyTable from a tibble (class tbl_df)
#' 
#' Assumes that the first column is the OTU/taxon names.
#' 
#' This function creates a matrix from `object` that contains all columns
#' except the first one, which is used for the row names. The resulting matrix
#' is then passed to `tax_table()`.
#' 
#' @export
#'
#' @seealso 
#' phyloseq::tax_table
#' 
#' @examples
#' \dontrun{
#' # Read a .csv file with readr, which creates an object of class `tbl_df`
#' tbl <- readr::read_csv("path/to/taxonomy_table.csv")
#' # Inspect and check that the first column contains the taxa/OTU names
#' head(tbl) 
#' # Create a phyloseq `taxonomyTable` object
#' tax <- tax_table(tbl)
#' }
setMethod("tax_table", "tbl_df", function (object) {
    message(paste0("Assuming first column, `", names(object)[1], 
            "`, contains the taxa names"))
    mat <- object %>%
        dplyr::select(-1) %>%
        as("matrix")
    rownames(mat) <- object[[1]]
	tax_table(mat)
})

