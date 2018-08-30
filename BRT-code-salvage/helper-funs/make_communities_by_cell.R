#' @param flora_occ SpatialPointsDataframe with structure as follows:
#'    class       : SpatialPointsDataFrame
#'    features    : <dbl>
#'    extent      : <dbl>
#'    coord. ref. : <std_CRS>
#'    variables   : 3
#'    names       :    ID, species,  cells
#'    min values  :     1,   <chr>,  <dbl>
#'    max values  : <dbl>,   <chr>,  <dbl>
#' Where "cells" is the label of the cell the occurrence is in on the raster
make_community_by_cell <- function(flora_occ = "SpatialPointsDataframe",
                                   dict_only = FALSE) {

    make_levels <- function(x) {
        x %>%
            as.character() %>%
            as.factor() %>%
            levels()
    }

    make_empty_community_matrix <- function(flora_occ) {
        # <!!>
        #flora_occ <- sim_flora_with_cells
        # </!!>
        cells <- make_levels(flora_occ@data$cells)
        species <- make_levels(flora_occ@data$species)
        empty_community <- matrix(
            nrow = length(cells),
            ncol = length(species)
        )
        rownames(empty_community) <- cells
        colnames(empty_community) <- species
        return(empty_community)
    }

    make_community_dictionary <- function(flora_occ) {
        # <!!>
        #flora_occ <- sim_flora_with_cells
        # </!!>
        list_species_in_a_cell <- function(cell, flora_occ) {
            community <- flora_occ@data$species[
                flora_occ@data$cells == cell
            ]
            community %<>% unique()
            return(community)
            # TODO: Note: REMEMBER! We are doing PRESENCE/ABSENCE ONLY
        }
        cell_names <- make_levels(flora_occ@data$cells)
        community_dictionary <- vector("list", length = length(cell_names))
        names(community_dictionary) <- cell_names
        for (i in seq_along(cell_names)) {
            community_dictionary[[i]] <- list_species_in_a_cell(
                cell = names(community_dictionary)[i],
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

    # Only return the community dictionary -------------------------------------

    if (dict_only) {

        community_dictionary <- make_community_dictionary(flora_occ)
        return(community_dictionary)

    # Or do the whole shebang --------------------------------------------------

    } else {

        fill_empty_community_matrix <- function(empty_community = "matrix",
                                                community_dictionary = "list") {
            for (cell in 1:nrow(empty_community)) {
                for (sp in 1:ncol(empty_community)) {
                    start_msg <- glue("
                        Cell number {cell} of {nrow(empty_community)} ... \n
                        Species number {sp} of {ncol(empty_community)} \\
                        ({colnames(empty_community)[sp]}) ... \\
                        \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \\
                        \r
                    ")
                    flush.console()
                    message(appendLF = FALSE, start_msg)
                    is_member <- colnames(empty_community)[sp] %in%
                        community_dictionary[[cell]]
                    empty_community[cell, sp] <-
                        if (is_member) {
                            TRUE
                        } else {
                            FALSE
                        }
                    end_msg <- glue("
                        Cell number {cell} of {nrow(empty_community)} ... \n
                        Species number {sp} of {ncol(empty_community)} \\
                        ({colnames(empty_community)[sp]}) \\
                        done! \\
                        \x20 \x20 \x20 \x20 \x20 \x20 \x20 \x20 \\
                        \r
                    ")
                    flush.console()
                    message(appendLF = FALSE, end_msg)
                }
            }
            communities_by_cell <- empty_community
            return(communities_by_cell)
        }

        # Normal-mode computations & return ------------------------------------

        communities_by_cell <- fill_empty_community_matrix(
            empty_community = make_empty_community_matrix(flora_occ),
            community_dictionary =  make_community_dictionary(flora_occ)
        )

        return(communities_by_cell)

    }

}
