distribute_gnr_matches_to_cols <- function(x, ignore = 1:3) {
    for (i in 1:nrow(x[-c(ignore)])) {
        for (j in seq_along(x[-c(ignore)])) {
            x[-c(ignore)][i, j] <- unique(x$gnr_matched_names[[i]])[j]
        }
    }
    return(as_tibble(x))
}

# Testing ----------------------------------------------------------------------

foo <- tibble(
    species = c(
        "a",
        "b",
        "c"
    ),
    gnr_matched_names = list(
        c("A", "@", "A"),
        c("d"),
        c("C", "e", "E")
    )
)

foo %>%
    count_gnr_matched_names() %>%
    add_gnr_matches_as_cols() %>%
    distribute_gnr_matches_to_cols()
