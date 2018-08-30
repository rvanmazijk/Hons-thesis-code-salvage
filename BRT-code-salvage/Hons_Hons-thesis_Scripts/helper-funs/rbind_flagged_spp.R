rbind_flagged_spp <- function(x) {
    # Takes a vector of flag_inconsistent_names() output tibbles
    # and rbind()s them all together :)
    rbind_x <- tibble()
    for (i in seq_along(x)) {
        rbind_x <- x[i]$flagged %>%
            rbind(rbind_x, .) %>%
            as_tibble()
    }
    return(rbind_x)
}
