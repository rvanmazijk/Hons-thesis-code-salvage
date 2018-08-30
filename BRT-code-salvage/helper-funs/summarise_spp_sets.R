summarise_spp_sets <- function(flora_GBIF_tidy) {
    flora_GBIF_tidy %$%
        list(
            species                   = species,
            infraspecificepithet      = infraspecificepithet,
            scientificname            = scientificname,
            # Add these:
            scientificname_species    = binom_only(scientificname),
            species_subsp             = paste(species, infraspecificepithet) %>%
                                            str_replace_all(" NA", "")
        ) %>%
        map(unique) %>%
        map(sort) %$%
        list(
            # Same as before:
            species                   = species,
            infraspecificepithet      = infraspecificepithet,
            scientificname            = scientificname,
            scientificname_species    = scientificname_species,
            species_subsp             = species_subsp,
            # And add these:
            intersect_species_sciname = intersect(
                                            species, scientificname_species),
            union_species_sciname     = union(
                                            species, scientificname_species),
            disjunct_species_sciname  = disjunct(
                                            species, scientificname_species)
        ) %$%
        list(
            # Same as before:
            species                   = species,
            infraspecificepithet      = infraspecificepithet,
            scientificname            = scientificname,
            scientificname_species    = scientificname_species,
            species_subsp             = species_subsp,
            intersect_species_sciname = intersect_species_sciname,
            union_species_sciname     = union_species_sciname,
            disjunct_species_sciname  = disjunct_species_sciname,
            # And add these:
            disj_spp_sciname_genera   = disjunct_species_sciname$disjunc %>%
                                            genus_only() %>%
                                            unique() %>%
                                            sort()
        )
}
