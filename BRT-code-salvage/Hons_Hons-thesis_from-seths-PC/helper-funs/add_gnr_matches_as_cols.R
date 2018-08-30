add_gnr_matches_as_cols <- function(x, ignore = 1:3) {
    max_n <- max(x$number_gnr_matched_names)
    x %<>% cbind(matrix(ncol = max_n, nrow = nrow(x)))
    names(x)[-c(ignore)] <- glue("gnr_match_{x}", x = 1:max_n)
    return(as_tibble(x))
}
