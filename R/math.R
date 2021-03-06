# Mathematical functions that can be applied to taxonomic abundance vectors.

# TODO 
# - consider making trim and na_rm usage uniform
# - add better documentation to each function

# Summaries of relative abundance vectors--------------------------------------

#' Geometric mean of x.
#'
#' @export
gm_mean <- function(x, na_rm = FALSE) {
    exp(mean(log(x), na.rm = na_rm))
}

#' Geometric standard deviation of x.
#'
#' Note, uses denominator n-1
#'
#' @export
gm_sd <- function(x, na_rm = FALSE) {
    exp(sd(log(x), na.rm = na_rm))
}

#' Geometric range of x.
#'
#' @export
gm_range <- function(x) {
    max(x) / min(x)
}

# Transformations for relative abundance vectors ------------------------------

#' Convert proportions to odds
#'
#' @export
odds <- function (x) {
    x / (1-x)
}

#' Convert proportions to log-odds (logit)
#'
#' @export
logit <- function (x) {
    log(x) - log(1-x)
}


#' "Close" the elements of x to proportions
#'
#' @export
close_elts <- function (x, na_rm = FALSE) {
    x / sum(x, na.rm = na_rm)
}

#' Geometrically center the elements of x
#'
#' @export
center_elts <- function (x, na_rm = FALSE) {
    exp(log(x) - mean(log(x), na.rm = na_rm))
}

#' Centered log-ratio transform of x.
#' 
#' @param x Vector of abundances.
#' @param base Base for logarithm
#'
#' @export
clr <- function(x, base = exp(1), na_rm = FALSE) {
    log(x, base = base) - mean(log(x, base = base), na.rm = na_rm)
}

# Note, the distance and alpha diversity functions below should be thought of
# as alternatives to vegan and phyloseq's functions that can be used on melted
# phyloseq objects.

# Distance / dissimilarity between samples ------------------------------------

# TODO: 
# - expand this to allow all distance methods that vegan / phyloseq allows
# - change behavior of bray to warn if not normalized to proportions, and
# perhaps give an option close = TRUE/FALSE to do this automatically

#' Distance or dissimilarity between relative abundance vectors x and y
#'
#' @param method : distance/dissimilarity measure
#' @param trim : should x and y be reduced to their common positive elements
#' before computing the Aitchison distance (otherwise, the distance will be
#' Inf)
#' 
#' method == "aitchison" -> Aitchison distance
#' method == "bray" -> Bray-Curtis dissimilarity between x and y. Note,
#' converts x and y to proportions before computing.
#'
#' Bray method is equal to `vegan::vegdist(rbind(close_elts(x), close_elts(y)))[1]`
#'
#' @export
xydist <- function(x, y, method = "aitchison", trim = FALSE) {
    if (length(x) != length(y)) {
        stop("x and y have different lengths")
    }
    if (method == "aitchison") {
        if (trim) {
            idx <- (x > 0) & (y > 0)
            x <- x[idx]
            y <- y[idx]
        }
        sqrt( sum( (clr(x) - clr(y))^2 ) )
    } else if (method == "bray") {
        x <- close_elts(x)
        y <- close_elts(y)
        Cxy <- map2_dbl(x, y, min) %>% sum
        1 - Cxy
    }
}

#' Aitchison norm of x
#'
#' @param na_rm; remove NAs and NaNs before calculating
#'
#' @export
anorm <- function (x, na_rm = FALSE) {
    x %>%
        clr(., na_rm = na_rm) %>%
        {.^2} %>%
        sum(., na.rm = na_rm) %>%
        sqrt(.)
}

# Alpha diversity -------------------------------------------------------------

#' Shannon entropy for a vector of relative abundances
#'
#' @export
entropy <- function (x) {
    x <- x[x>0]
    x <- x / sum(x)
    - sum( x * log(x) )
}


# TODO: expand the diversity function to be able to provide standard diversity
# indices specified by index = ... or to give true diversity specified by q =
# #.

#' True diversity of composition x (i.e., Hill numbers)
#'
#' Diversity sensu Jost (i.e., Hill numbers, effective number of species, true
#' diversity; Jost2006)
#' 
#' @export
diversity <- function (x, order = 1) {
    x <- x[x>0]
    x <- x / sum(x)
    if (order == 1) {
        exp( - sum( x * log(x) ) )
    } else {
        sum(x ^ order) ^ (1 / (1 - order))
    }
}
