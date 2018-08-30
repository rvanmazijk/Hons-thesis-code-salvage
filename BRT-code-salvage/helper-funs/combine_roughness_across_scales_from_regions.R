combine_roughness_across_scales_from_regions <- function(GCFR, SWAFR) {
    as_tibble(rbind(
        cbind(
            GCFR,
            region = rep("GCFR", 15000)
        ),
        cbind(
            SWAFR,
            region = rep("SWAFR", 15000)
        )
    )) %>%
        mutate(scale = 0.05 * as.numeric(fact))
}
