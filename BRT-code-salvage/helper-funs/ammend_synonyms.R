ammend_synonyms <- function(spp_column = "in `GBIF_..._tidy`",
                            spp_set = "from `..._spp_sets$species`",
                            syns = "from `get_synonyms()`",
                            simple = FALSE) {

    ammend_synonyms_simple <- function(spp_set = "list", syns = "list") {
        for (i in seq_along(spp_set)) {
            if (not(any(is.na(syns[[i]])))) {
            # `any()` is NB, as sometimes as "no syns found"
            # can slip in with a real syn
                for (j in seq_along(syns[[i]])) {
                    if (syns[[i]][[j]] %in% spp_set) {
                        spp_set[[i]] <- syns[[i]][[j]]
                    }
                }
            }
        }
        return(spp_set)
    }

    ammend_synonyms_dict <- function(spp_column, spp_set, syns) {

        syns_dict <- make_dict(spp_set, syns)

        for (dict_entry in syns_dict) {
            for (synonym in dict_entry$syns) {
                if (synonym %in% spp_column) {
                    spp_column[spp_column == synonym] <- dict_entry$raw_name
                    message(appendLF = FALSE, glue("
                        {dict_entry$raw_name}: \\
                        {synonym}: \\
                        ammended \\
                        \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \\
                        \r
                    "))
                } else {
                    message(appendLF = FALSE, glue("
                        {dict_entry$raw_name}: \\
                        {synonym}: \\
                        not in column \\
                        \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \\
                        \r
                    "))
                }
                flush.console()
            }
        }

        return(spp_column)

    }

    if (simple) {
        ammend_synonyms_simple(spp_set, syns)
    } else {
        ammend_synonyms_dict(spp_column, spp_set, syns)
    }

}

if (FALSE) {

    # Tests: ammend_synonyms_simple() ----------------------------------------------

    # Simple case of ≤ 1 synonym per query species
    spp_set <- list(
        a = "a",
        b = "b",
        c = "c",
        d = "d"
    )
    syns <- list(
        a = NA,
        b = "d",
        c = "a",
        d = NA
    )
    ammend_synonyms(spp_set = spp_set, syns = syns, simple = TRUE)

    # Complex case of ≥ 1 synonym per query species
    spp_set <- list(
        a = "a",
        b = "b",
        c = "c",
        d = "d"
    )
    syns <- list(
        a = NA,
        b = c("d", "B", "bb"),
        c = "C",
        d = NA
    )
    ammend_synonyms(spp_set = spp_set, syns = syns, simple = TRUE)

    # More complex case of circular synonymy: works fine w/ no mods!
    # The reason is that I modify `spp_set` as I go, thus eliminating circularity
    spp_set <- list(
        a = "a",
        b = "b",
        c = "c",
        d = "d"
    )
    syns <- list(
        a = "c",
        b = c("d", "B", "bb"),
        c = c("a", "C"),
        d = "b"
    )
    ammend_synonyms(spp_set = spp_set, syns = syns, simple = TRUE)


    # Tests: ammend_synonyms_dict() ------------------------------------------------

    spp_column <- list(
        a = "b",
        a = "a",
        b = "b",
        c = "c",
        a = "a",
        c = "c",
        c = "c"
    )
    spp_set <- unique(spp_column)
    syns <- list(
        a = "b",
        b = c("a", "b", "D"),
        c = NA
    )
    ammend_synonyms(spp_column = spp_column, spp_set = spp_set, syns = syns)

}
