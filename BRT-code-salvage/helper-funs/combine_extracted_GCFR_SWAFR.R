combine_extracted_GCFR_SWAFR <- function(GCFR, SWAFR) {
    extracted_BOTH_new <- as_tibble(rbind(
        cbind(
            GCFR,
            region = "GCFR"
        ),
        cbind(
            SWAFR,
            region = "SWAFR"
        )
    ))
    extracted_BOTH_new %<>% mutate(
        pt_ID = glue("{region}_{pt_ID}")
    )
    return(extracted_BOTH_new)
}
