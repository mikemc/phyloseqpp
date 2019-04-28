#' Transform abundance data in an \code{otu_table}, sample-by-sample.
#' 
#' This is a wrapper around phyloseq::transform_sample_counts that allows use
#' of a purrr-style anonymous function for `.f`.
#' 
#' @seealso 
#' phyloseq::transform_sample_counts
#' tidyseq::filter_taxa
#'
#' @export
#' @examples
#' library(phyloseq)
#' library(tidyseq)
#' data(GlobalPatterns)
#' # Filter low prevalence taxa and then convert to proportions
#' gp.prop <- GlobalPatterns %>%
#'   filter_taxa(~ sum(. > 0) > 5) %>%
#'   transform_sample_counts(~ . / sum(.))
transform_sample_counts <- function(physeq, .f, ...){
    fun <- purrr::as_mapper(.f)
    phyloseq::transform_sample_counts(physeq, fun, ...)
}

#' Filter taxa based on across-sample OTU abundance criteria
#' 
#' This is a wrapper around phyloseq::filter_taxa. It allows use of a
#' purrr-style anonymous function for `.f`. It also sets prune = TRUE by
#' default to be consistent with other subsetting functions and facilitate use
#' in pipe chains.
#'
#' @export
#' @seealso 
#' phyloseq::filter_taxa
#' tidyseq::transform_sample_counts
#' 
#' @examples
#' library(phyloseq)
#' library(tidyseq)
#' data(GlobalPatterns)
#' # Filter low prevalence taxa and then convert to proportions
#' gp.prop <- GlobalPatterns %>%
#'   filter_taxa(~ sum(. > 0) > 5) %>%
#'   transform_sample_counts(~ . / sum(.))
filter_taxa <- function(physeq, .f, prune = TRUE){
    fun <- purrr::as_mapper(.f)
    phyloseq::filter_taxa(physeq, fun, prune = prune)
}
