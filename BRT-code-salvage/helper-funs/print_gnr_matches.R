print_gnr_matches <- function(x) {
    # Utilises the gnr_matched_names list column in the outputs of
    # flag_inconsistent_names() (i.e. is_consistent()).
    print(glue("[GNR match?]: [list of matched names]"))
    for (i in 1:nrow(x)) {
        names <- ""
        for (j in 1:length(x$gnr_matched_names[[i]])) {
            names <- glue("{names}{x$gnr_matched_names[[i]][j]}, ")
        }
        print(glue("{x$gnr_cond[i]}: {names}"))
    }
}
