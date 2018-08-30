get_neighbouring_cells <- function(raster, focal_cell_name,
                                   directions = 8) {
    adjacency <- raster %>%
        adjacent(
            cells = focal_cell_name,
            directions = directions,
            include = FALSE  # Don't return the focal cell
        )
    neighbours <- adjacency[, 2]
    return(neighbours)
}

jaccard_focal_vs_neighbours <- function(communities_by_cell,
                                        focal_cell_name,
                                        neighbour_cell_names) {
    # Get all 9 cells in the window (= focal + 8x NNs)
    focal_cell <- communities_by_cell[
        rownames(communities_by_cell) == focal_cell_name,
    ]
    neighbour_cells <- communities_by_cell[
        rownames(communities_by_cell) %in% neighbour_cell_names,
    ]
    # Calculate
    jaccard_dists <-
        vegdist(
            x = rbind(
                focal_cell,
                neighbour_cells
            ),
            method = "jaccard"
        ) %>%
        as.matrix()
    # Only return mean dissim. to neighbours
    focal_mean_jaccard_dist <- mean(jaccard_dists[-1, 1])
    return(focal_mean_jaccard_dist)
}

jaccard_focal_vs_neighbours_quick <- function(community_dict,
                                              focal_cell_name,
                                              neighbour_cell_names) {

    # Get all 9 cells in the window (= focal + 8x NNs) -------------------------

    focal_cell <-
        community_dict[
            names(community_dict) == focal_cell_name
        ] %>%
        map(as.character) %>%
        unlist() %>%
        as.vector()
    neighbour_cells <-
        community_dict[
            names(community_dict) %in% neighbour_cell_names
        ] %>%
        map(as.character) %>%
        map(as.vector)


    # Pair-wise mini community matrices ----------------------------------------
    # Between a focal cell and one of its neighbours each time

    jaccard_dists <- vector("list")

    for (i in seq_along(neighbour_cells)) {

        # <!!>
        #focal_cell <- GCFR_clean_flora_community_dict[
        #    names(GCFR_clean_flora_community_dict) == focal_cell_name
        #] %>%
        #    map(as.character) %>%
        #    unlist() %>%
        #    as.vector()
        #neighbour_cells <- GCFR_clean_flora_community_dict[
        #    names(GCFR_clean_flora_community_dict) %in% neighbour_cell_names
        #]
        #neighbour_cell <- neighbour_cells[[4]]
        # </!!>

        mini_community <- focal_cell %and% neighbour_cells[[i]]
        mini_community_matrix <- matrix(
            nrow = 2,
            ncol = length(mini_community),
            data = NA
        )
        colnames(mini_community_matrix) <- mini_community
        rownames(mini_community_matrix) <- c("focal_cell", "neighbour_cell")

        # Fill focal cell row in mini community matrix
        for (sp in 1:ncol(mini_community_matrix)) {
            is_member <-
                colnames(mini_community_matrix)[sp] %in% focal_cell
            mini_community_matrix[1, sp] <- if (is_member) TRUE
                                            else FALSE
        }

        # Fill neighbour cell row in mini community matrix
        for (sp in 1:ncol(mini_community_matrix)) {
            is_member <-
                colnames(mini_community_matrix)[sp] %in% neighbour_cells[[i]]
            mini_community_matrix[2, sp] <- if (is_member) TRUE
                                            else FALSE
        }

        # Calculate Jaccard's distance between these two communities
        jaccard_dists[[i]] <- c(designdist(
            mini_community_matrix,
            method = "(A + B - 2*J) / (A + B - J)"
        ))
        names(jaccard_dists)[i] <- names(neighbour_cells)[i]

    }

    focal_mean_jaccard_dist <- mean(unlist(jaccard_dists), na.rm = TRUE)
    return(focal_mean_jaccard_dist)

}

jaccard_grid <- function(raster, quickly = FALSE,
                         communities_by_cell = NULL, community_dict = NULL) {

    if (is.null(communities_by_cell) && is.null(community_dict)) {
        stop("
            Please supply either a community matrix, row-wise by raster cell,
            or a community dictionary (sensu `make_community_dictionary()`)
        ")
    }

    if (quickly) {

        # Quicker implem with `jaccard_focal_vs_neighbours_quick()` ------------

        jaccard_raster <- raster
        jaccard_raster[] <- NA
        for (i in seq_along(community_dict)) {
            focal_cell_name <- as.numeric(names(community_dict)[i])
            neighbour_cell_names <- get_neighbouring_cells(
                raster,
                focal_cell_name
            )
            focal_mean_jaccard_dist <- jaccard_focal_vs_neighbours_quick(
                community_dict,
                focal_cell_name,
                neighbour_cell_names
            )
            jaccard_raster[focal_cell_name] <- focal_mean_jaccard_dist
            message(glue("
                Cell no. {i} of {length(community_dict)} ceomplete
            "))
        }
        return(jaccard_raster)

    } else {

        # Slower, earlier implem with `jaccard_focal_vs_neighbours()` ----------

        # Implem `get_neighbouring_cells()`, `jaccard_focal_vs_neighbours()`
        # across a raster and the communities labelled by
        # cell no. from that raster
        jaccard_raster <- raster
        jaccard_raster[] <- NA
        for (i in 1:nrow(communities_by_cell)) {
            focal_cell_name <- as.numeric(rownames(communities_by_cell)[i])
            neighbour_cell_names <- get_neighbouring_cells(
                raster,
                focal_cell_name
            )
            focal_mean_jaccard_dist <- jaccard_focal_vs_neighbours(
                communities_by_cell,
                focal_cell_name,
                neighbour_cell_names
            )
            jaccard_raster[focal_cell_name] <- focal_mean_jaccard_dist
        }
        return(jaccard_raster)

    }

}

