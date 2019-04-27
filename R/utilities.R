# Tidy helpers ---------------------------------------------------------------

#' Mutate within groups
#'
#' @export
mutate_by <- function(.data, group_vars, ...) {
    gvs <- rlang::enquos(group_vars)
    .data %>%
        group_by_at(vars(!!!gvs)) %>%
        mutate(...) %>%
        ungroup
}

# mutate_by_alt <- function(.data, group_var, ...) {
#     group_var <- rlang::enquo(group_var)
#     .data %>%
#         group_by(!!group_var) %>%
#         mutate(...) %>%
#         ungroup
# }

