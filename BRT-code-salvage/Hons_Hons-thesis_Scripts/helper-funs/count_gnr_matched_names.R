count_gnr_matched_names <- function(x) {
    x %>%
        mutate(
            number_gnr_matched_names = gnr_matched_names %>%
                map(unique) %>%
                map(length) %>%
                unlist()
        )
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

count_gnr_matched_names(foo)
