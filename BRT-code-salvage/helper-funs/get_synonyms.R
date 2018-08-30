get_synonyms <- function(sp = "character") {

    # Preamble -----------------------------------------------------------------

    # During an earlier run of this function on the GCFR spp. list, I found that
    # sometimes `get_tpsid_()` would itself correct my query to a more accepted
    # synonym, but when this tpsid was given to `synonyms()`, it returns no
    # synonyms.

    # e.g.
    #
    # > get_tpsid_("Albuca unifolia")
    #
    #   $`Albuca unifolia`
    #           nameid  scientificname
    #   [1]     12345   Albuca unifoliata
    #   ...
    #
    # > get_tpsid_("Albuca unifoliata")
    #
    #   $`Albuca unifoliata`
    #           nameid  scientificname
    #   [1]     12345   Albuca unifoliata
    #   ...
    #
    # > synonyms(12345, db = "tropicos")
    #
    #   $`<NA>`
    #   [1]     NA

    # Solution? Turn that auto-corrected synonym from `get_tpsid_()` into a
    # synonym that I return at the end of `get_synonyms()`!

    # This is a sort-of "early-catch" functionality!

    # Basically, if `get_tpsid_(...)...$scientificname` is different to the
    # name I queried (`sp`), then I store `...$scientificname`.


    # Query Tropicos to get synonyms -------------------------------------------

    tpsid <- get_tpsid_(sp)

    if (not(is.null(tpsid[[1]]))) {

        if (tpsid[[1]]$scientificname != sp) {
            tpsid_syn <- tpsid %>%
                `[[`(1) %>%
                `$`(scientificname)
        } else {
            tpsid_syn <- "no syns found"
        }

        tpsid %<>%
            `[[`(1) %>%
            `$`(nameid)

        # Query Tropicos *now* to get the synonyms
        syns_tropicos <- synonyms(
            tpsid,  # Uses Tropicos ID
            db = "tropicos"
        )
        if (not(is.na(syns_tropicos))) {
            syns_tropicos %<>%
                `[[`(1) %>%
                `$`(scientificname)
        }

    } else {

        syns_tropicos <- "no syns found"
        tpsid_syn <- "no syns found"

    }

    if ((tpsid_syn == "no syns found") && (syns_tropicos == "no syns found")) {
        syns_tropicos <- "no syns found"
    } else {
        syns_tropicos <- syns_tropicos %and% tpsid_syn
        if ("no syns found" %in% syns_tropicos) {
            syns_tropicos %<>% without("no syns found")
        }
    }

    # Query ITIS to get more synonyms ------------------------------------------

    syns_itis <- synonyms(
        get_tsn(sp), # Uses ITIS taxo serial no.
        db = "itis",
        accepted = FALSE
    )
    if (not(is.na(syns_itis))) {
        syns_itis %<>%
            `[[`(1) %>%
            `$`(syn_name)
    }


    # Combine and return -------------------------------------------------------

    syns <-
        c(syns_tropicos, syns_itis) %>%
        binom_only() %>%
        unique() #%>%
        #`[[`(1) %>%
        #without(NA)
    return(syns)

}

# Tests

if (FALSE) {

    get_synonyms("Poa annua")

    foo <- synonyms(get_tsn("Poa annua"), db = "tropicos")
    foo2 <- synonyms(get_tsn("Poa annua"), db = "itis", accepted = FALSE)

    foo %>%
        `[[`(1) %>%
        `$`(syn_name) %>%
        binom_only() %>%
        unique()

    foo2 %>%
        `[[`(1) %>%
        `$`(syn_name) %>%
        binom_only() %>%
        unique()

}

if (FALSE) {
    tnrs("Abutilon dinteri")
    get_tpsid("Abutilon dinteri")
    get_tpsid_("Abelmoschus esculentus") %>%
        `[[`(1) %>%
        filter(scientificname == str_match(
            scientificname,
            "Abelmoschus esculentus"
        )) %>%
        `$`(nameid)
    synonyms("Abutilon dinteri", db = "tropicos")
}

if (FALSE) {
    sp <- "Albuca unifolia"
    tpsid <- get_tpsid_(sp)
    if (tpsid[[1]]$scientificname != sp) {
        tpsid_syn <- tpsid %>%
            `[[`(1) %>%
            `$`(scientificname)
    } else {
        tpsid_syn <- NULL
    }
    tpsid %<>%
        `[[`(1) %>%
        `$`(nameid)
    syns_tropicos <- synonyms(
        tpsid,
        db = "tropicos"
    )
    if (not(is.na(syns_tropicos))) {
        syns_tropicos %<>%
            `[[`(1) %>%
            `$`(scientificname)
    }
    syns_tropicos <- syns_tropicos %and% tpsid_syn
}
