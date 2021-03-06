% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phyloseq-constructors.R
\docType{methods}
\name{otu_table,tbl_df-method}
\alias{otu_table,tbl_df-method}
\title{Constructor for creating otu_table from a tibble (class tbl_df)}
\usage{
\S4method{otu_table}{tbl_df}(object, taxa_are_rows)
}
\description{
If taxa are rows, assumes that the first column is the OTU/taxon names. If
taxa are columns, assumes that the first column is the sample names.
}
\seealso{
phyloseq::otu_table

\dontrun{
# Read a .csv file with readr, which creates an object of class `tbl_df`
tbl <- readr::read_csv("path/to/otu_table.csv")
# Inspect and check if taxa are rows and that the first column contains the
# sample names or the taxa/OTU names
head(tbl) 
# Create a phyloseq `otu_table` object
otu <- otu_table(tbl, taxa_are_rows = FALSE)
}
}
