no_syns_to_NA <- function(x) {
    # Replace all "no syns found", "no syns" with NA
    for (i in seq_along(x)) {
        for (j in seq_along(x[[i]])) {
            if (x[[i]][[j]] %in% c("no syns found", "no syns")) {
                x[[i]][[j]] <- NA
            }
        }
    }
    # Remove NAs from species that have other actual synonyms
    for (i in seq_along(x)) {
        if (not(any(is.na(x[[i]])))) {
            x[[i]] %<>% without(NA)
        }
    }
    return(x)
}
