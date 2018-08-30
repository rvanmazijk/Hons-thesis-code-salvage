# Takes a GBIF derived dataframe and tidies it up for me, like so:

tidy_gbif <- function(x = "data.frame") {
    x %>%
        dplyr::filter(
            phylum == "Tracheophyta",  # Just in case
            taxonrank %in% c(
                "SPECIES",
                "SUBSPECIES",
                "FORM",
                "VARIETY"
            )
        ) %>%
        # (so that all infrasp. ranks can be merged into their spp.)
        dplyr::select(
            family,
            genus,
            species,
            infraspecificepithet,
            scientificname,
            taxonrank,
            # Note: use `species` mostly,
            # as does not contain any infrasp. info!
            # TODO: think about `species` vs `scientificname`
            decimallatitude, 
            decimallongitude,
            coordinateuncertaintyinmeters,
            coordinateprecision
        ) %>%
        dplyr::arrange(species)
}
