get_saved_BRTs <- function(dir,
                           tol = NULL, tc = NULL, lr = NULL) {

    # Query all files in dir
    saved_BRT_dirs <-
        list.files(
            dir,
            full.names = TRUE
        )

    # Subset down where required
    if (not(is.null(tol))) {
        saved_BRT_dirs %<>%
            magrittr::extract(str_detect(., tol))
    }
    if (not(is.null(tc))) {
        saved_BRT_dirs %<>%
            magrittr::extract(str_detect(., tc))
    }
    if (not(is.null(lr))) {
        saved_BRT_dirs %<>%
            magrittr::extract(str_detect(., lr))
    }

    # Init list
    saved_BRTs <- list()

    for (i in seq_along(saved_BRT_dirs)) {

        # Read-in RDSs
        saved_BRTs[[i]] <- readRDS(saved_BRT_dirs[i])

        # Tidy-up list-names
        names(saved_BRTs)[i] <- saved_BRT_dirs[i] %>%
            str_split("/") %>%
            `[[`(1) %>%
            `[`(length(.)) %>%
            str_split("_") %>%
            `[[`(1) %>%
            glue("{.[2]}_{.[3]}_{.[4]}", . = .) %>%
            str_replace_all("\\.rds", "") %>%
            str_replace_all("\\-", "_")

    }

    return(saved_BRTs)

}

# Junk

#if (is.null(tol) && is.null(tc) && is.null(lr)) {
#    # TODO: return all RDS in dir
#}

# If queried params null, do nothing,
# as `stringr::` returns all when no subset specified

#saved_BRTs <- vector(
#    "list",
#    length =
#        if (is.null(tol) && is.null(tc)) {
#            length(lr)
#        } else if (is.null(tc) && is.null(lr)) {
#            length(tol)
#        } else if (is.null(tol) && is.null(tc)) {
#            length(tc)
#        } else if (is.null(tol)) {
#            length(tc) * length(lr)
#        } else if (is.null(tc)) {
#            length(tol) * length(lr)
#        } else if (is.null(lr)) {
#            length(tol) * length(tc)
#        }
#)
# TODO: c("a", "b", "c") -> "(a|b|c)"
