make_mini_community_matrix <- function(focal_cell, neighbour_cells) {

    #focal_cell[[1]] %<>% as.character()
    #neighbour_cells %<>% map(as.character)
    # (`as.character()` to stop numbers arriving in the mini_community set)

    mini_community <- as.character(focal_cell[[1]])
    for (i in seq_along(neighbour_cells)) {
        mini_community <-
            mini_community %and% as.character(neighbour_cells[[i]])
    }
    mini_community <- mini_community[-1]  # Getting rid of more "numbers"

    mini_community_matrix <- matrix(
        nrow = length(neighbour_cells) + 1,
        ncol = length(mini_community),
        data = NA
    )
    colnames(mini_community_matrix) <- mini_community
    rownames(mini_community_matrix) <- c(
        names(focal_cell),
        names(neighbour_cells)
    )

    # Fill focal cell row in mini community matrix
    for (sp in 1:ncol(mini_community_matrix)) {
        is_member <-
            colnames(mini_community_matrix)[sp] %>%
            is_in(as.character(focal_cell[[1]]))
        mini_community_matrix[1, sp] <-
            if (is_member) 1
            else 0
    }

    # Fill neighbour cell rows in mini community matrix
    for (neighbour in 2:nrow(mini_community_matrix)) {
        for (sp in 1:ncol(mini_community_matrix)) {
            is_member <-
                colnames(mini_community_matrix)[sp] %>%
                is_in(as.character(neighbour_cells[[neighbour - 1]]))
            mini_community_matrix[neighbour, sp] <-
                if (is_member) 1
                else 0
        }
    }

    return(mini_community_matrix)

}
