simple_jaccard <- function(x = "<chr>", y = "<lst, length = 1>") {
    # Jaccard's distance = (A + B - 2*J) / (A + B - J)
    #y <- y[[1]]
    A <- length(unique(x))
    B <- length(unique(y))
    J <- length(intersect(x, y))
    jacc_dist <- (A + B - 2*J) / (A + B - J)
    return(jacc_dist)
}

neighbourhood_jaccards <- function(focal = "<chr>", neighbours = "<lst>") {
    jacc_dists <- vector(length = length(neighbours))
    for (i in seq_along(jacc_dists)) {
        jacc_dists[i] <- simple_jaccard(
            x = focal,
            y = neighbours[[i]]
        )
    }
    return(jacc_dists)
}

multifocal_jaccards <- function(community_dict, reference_raster) {

    # Copy the reference raster, & empty it
    jaccard_raster <- reference_raster
    jaccard_raster[] <- NA

    # For each focal cell
    for (i in seq_along(community_dict)) {

        # Get the focal + neighbour cell names on the ref raster
        focal_cell_name <- names(community_dict[i])
        neighbour_cell_names <- as.character(get_neighbouring_cells(
            raster = reference_raster,
            focal_cell_name = as.numeric(focal_cell_name)
            # 2017-09-25 12:21 --- `as.numeric()` because `raster::`
        ))

        # Query to get those cells communities from the dict
        focal_cell <- community_dict[[focal_cell_name]]
        # `[[...]]` NB so NOT a list
        neighbour_cells <- community_dict[neighbour_cell_names]
        # `[...]` NB so IS a list

        # Get neighbourhood Jaccard's dists
        jacc_dists <- neighbourhood_jaccards(
            focal = focal_cell,
            neighbours = neighbour_cells
        )

        # Save the mean thereof to that cell in the output raster
        jaccard_raster[as.numeric(focal_cell_name)]  <- mean(jacc_dists)
        # 2017-09-25 12:19 --- See §Testing below for why `as.numeric()`
        #                      (it is correct!)

    }

    return(jaccard_raster)

}

# Testing

if (FALSE) {

    # These 3 all agree:
    GCFR_QDS_raster[
        as.numeric(names(GCFR_community_dict[2]))
    ]
    GCFR_QDS_raster %>% xyFromCell(
        as.numeric(names(GCFR_community_dict[2]))
    )
    GCFR_spatial_pts[
        GCFR_spatial_pts$cells == as.numeric(names(GCFR_community_dict[2])),
    ]
    # Taking the "rowname" of `GCFR_QDS_raster` returned for cell no.
    # `as.numeric(names(GCFR_community_dict[2]))`,
    # we can see that *that* rowname is not what should be used to index.
    # Yay :)
    xyFromCell(GCFR_QDS_raster, 1067)

}
