# NOTE: in future, may want to change how rownames are handled. Perhaps have
# default rownames=NA and allow for rownames option to be passed to the
# function.
# I suggest making the default that OTU be used as the label for consistency
# with phyloseq; but perhaps have handled by a package option so that can be
# easily set to e.g. Taxon or ASV
# 
# Regarding how to set as_tibble methods for S4 objects, see
# https://stackoverflow.com/questions/12100856/combining-s4-and-s3-methods-in-a-single-function

# TODO: fix up the documentation so that these all have one help page; see
# https://github.com/acidgenomics/transformer/blob/master/R/as_tibble-methods.R

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

#' @export
setGeneric("as_tibble")

#' as_tibble for otu_table
#'
#' If tidy == TRUE, will give in tidy format like psmelt
#'
#' @export
as_tibble.otu_table <- function(x, tidy = FALSE) {
    if (taxa_are_rows(x)) {
        row_type = "Taxon"
    } else {
        row_type = "Sample"
    }
    mat <- x %>% as("matrix")
    if (tidy) {
        if (row_type == "Sample") {
            mat <- t(mat)
        }
        tb <- mat %>% 
            tibble::as_tibble(rownames = "Taxon") %>%
            tidyr::gather("Sample", "Abundance", -Taxon)
    } else {
        tb <- mat %>% 
            tibble::as_tibble(rownames = row_type)
    }
    tb
}

# NOTE: Should to allow for having a column already named Sample in the
# sample metadata, which will currently cause an error

#' as_tibble for sample_data
#'
#' @export
as_tibble.sample_data <- function(x) {
    x %>% as("data.frame") %>% tibble::as_tibble(rownames = "Sample")
}

#' as_tibble for taxonomyTable
#'
#' @export
as_tibble.taxonomyTable <- function(x) {
    x %>% as("matrix") %>% tibble::as_tibble(rownames = "Taxon")
}

#' as_tibble for DNAStringSet
#'
#' @export
as_tibble.DNAStringSet <- function(x) {
    x %>% as.character %>% tibble::enframe("Taxon", "Sequence")
}

#' as_tibble for phyloseq objects
#'
#' @param tax whether to include taxonomy information
#'
#' @export
as_tibble.phyloseq <- function(x, tax = TRUE) {
    # There is always an otu_table; may not be sample_data or tax_table's
    tb <- otu_table(x) %>% as_tibble(tidy = TRUE)
    sam <- access(x, "sam_data")
    tt <- access(x, "tax_table")
    # Add sample data if it exists
    if (!is.null(sam)) {
        tb <- tb %>% 
            dplyr::left_join(sam %>% as_tibble, by = "Sample", 
                suffix = c("", ".sam"))
    }
    # Add tax_table if it exists and tax = TRUE
    if (tax & !is.null(tt)) {
        tb <- tb %>% 
            dplyr::left_join(tt %>% as_tibble, by = "Taxon", 
                suffix = c("", ".tax"))
    }
    tb
}
