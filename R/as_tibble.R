# NOTE: in future, may want to change how rownames are handled. Perhaps have
# default rownames=NA and allow for rownames option to be passed to the
# function.
# I suggest making the default that OTU be used as the label for consistency
# with phyloseq; but perhaps have handled by a package option so that can be
# easily set to e.g. Taxon or ASV
# 

#' as_tibble for otu_table
#'
#' If tidy == TRUE, will give in tidy format like psmelt
#'
#' @importFrom tibble as_tibble
#' @export
setMethod("as_tibble", c(x = "otu_table"),
    function(x, tidy = FALSE) {
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
)

# NOTE: Should to allow for having a column already named Sample in the
# sample metadata, which will currently cause an error

#' as_tibble for sample_data
#'
#' @export
setMethod("as_tibble", c(x = "sample_data"),
    function(x) {
        x %>% as("data.frame") %>% tibble::as_tibble(rownames = "Sample")
    }
)

#' as_tibble for taxonomyTable
#'
#' @export
setMethod("as_tibble", c(x = "taxonomyTable"),
    function(x) {
        x %>% as("matrix") %>% tibble::as_tibble(rownames = "Taxon")
    }
)

#' as_tibble for DNAStringSet
#'
#' @export
setMethod("as_tibble", c(x = "DNAStringSet"),
    function(x) {
        x %>% as.character %>% tibble::enframe("Taxon", "Sequence")
    }
)

#' as_tibble for phyloseq objects
#'
#' @param tax whether to include taxonomy information
#'
#' @export
setMethod("as_tibble", c(x = "phyloseq"),
    function(x, tax = TRUE) {
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
)

