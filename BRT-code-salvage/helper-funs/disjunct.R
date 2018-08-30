disjunct <- function(x, y) {
    union <- union(x, y)
    intersect <- intersect(x, y)
    disjunc <- setdiff(union, intersect)
    membership <- vector(length = length(disjunc))
    for (i in seq_along(disjunc)) {
        membership[i] <-
            if (disjunc[i] %in% x) {
                as.character(substitute(x))
            } else if (disjunc[i] %in% y) {
                as.character(substitute(y))
            } else {
                NA
            }
    }
    return(tibble(disjunc, membership))
}
