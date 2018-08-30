make_community_dict2 <- function(flora_occ = "SpatialPointsDataFrame") {

    make_levels <- function(x) {
        x %>%
            as.character() %>%
            as.factor() %>%
            levels()
    }

    list_species_in_a_cell <- function(cell, flora_occ) {
        community <-
            flora_occ$species[flora_occ$cells == cell] %>%
            unique() %>%
            as.character()
        return(community)
        # TODO: Note: REMEMBER! We are doing PRESENCE/ABSENCE ONLY
    }

    # <!!>
    #flora_occ <- GCFR_spatial_pts
    # </!!>

    cell_names <- make_levels(flora_occ@data$cells)
    community_dictionary <- vector("list", length = length(cell_names))

    # <!!>
    #for (i in 1:10) {
    #    community_dictionary[[i]] <- list_species_in_a_cell(
    #        cell = cell_names[i],
    #        flora_occ
    #    )
    #}
    # </!!>

    names(community_dictionary) <- cell_names

    for (i in seq_along(cell_names)) {

        community_dictionary[[i]] <- list_species_in_a_cell(
            cell = cell_names[i],
            flora_occ
        )

        flush.console()
        message(appendLF = FALSE, glue("
            Cell number {i} of {length(cell_names)} ... \n
            \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \\
            \r"
        ))

    }

    return(community_dictionary)

}

make_community_dict3 <- function(flora_occ = "SpatialPointsDataFrame") {

    cell_names <- unique(as.character(flora_occ$cells))
    community_dict <- vector("list", length = length(cell_names))

    for (i in seq_along(cell_names)) {

        community_dict[[i]] <- flora_occ$species[
            flora_occ$cells == cell_names[i]
        ]

        flush.console()
        message(appendLF = FALSE, glue("
            Cell number {i} of {length(cell_names)} ... \n
            \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \\
            \r"
        ))

    }

    community_dict %<>%
        map(unique) %>%
        map(as.character)

    names(community_dict) <- cell_names
    return(community_dict)

}
