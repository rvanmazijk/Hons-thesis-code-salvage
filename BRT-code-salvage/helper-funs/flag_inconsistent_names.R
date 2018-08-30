flag_inconsistent_names <- function(flora, name_range = 1:100) {

    # TODO: write docs for these funs

    do_tnrs <- function(spp_vect,
                        unique_index_range = NULL) {
        spp_vect_unique <- spp_vect %>%
            unique() %>%
            as.character()
        if (not(is.null(unique_index_range))) {
            spp_vect_unique <- spp_vect_unique[unique_index_range]
        }
        out_tnrs <- as_tibble(tnrs(
            query = spp_vect_unique,
            source = "iPlant_TNRS"
        ))
        return(out_tnrs)
    }

    do_gnr <- function(spp_vect,
                       unique_index_range = NULL,
                       data_source_ids = 12) {  # (12 = EOL)
        spp_vect_unique <- spp_vect %>%
            unique() %>%
            as.character()
        if (not(is.null(unique_index_range))) {
            spp_vect_unique <- spp_vect_unique[unique_index_range]
        }
        out_gnr <- as_tibble(gnr_resolve(
            names = spp_vect_unique,
            data_source_ids = data_source_ids
        ))
        return(out_gnr)
    }

    is_consistent <- function(tnrs_df,
                              gnr_df,
                              index = 1:nrow(tnrs_df)) {

        # Preamble -------------------------------------------------------------

        # If A = B, B = C, and C = D, then one needn't test that A = C or A = D,
        # as it is logically consequential!
        # And because A = D (tnrs_df$submittedname == gnr_df$user_supplied_name)
        # is a given for my work, I don't even need to test for B = C!
        # So ALL I need to check is that the submitted name is matched in both
        # TNRS and GNR


        # Check if names are matched by TRNS -----------------------------------

        tnrs_cond <- tnrs_df %$% {submittedname[index] == matchedname[index]}


        # Check if names are matched by GNR ------------------------------------

        # Organise the gnr_df
        gnr_df_indexed <- gnr_df %>%
            filter(user_supplied_name %in% tnrs_df$submittedname[index]) %>%
            transmute(
                user_supplied_name = user_supplied_name,
                matched_name = matched_name,
                # Remove authority from name:
                binom_only_matched_name = binom_only(matched_name),
                data_source_title = data_source_title,
                score = score
            ) %>%
            as_tibble()

        # Test it against the submitted names
        gnr_cond <- vector(length = length(tnrs_df$submittedname[index]))
        for (i in seq_along(tnrs_df$submittedname[index])) {
            submitted_name_i_is_in_gnr <- tnrs_df$submittedname[index][i] %in%
                gnr_df_indexed$binom_only_matched_name
            if (submitted_name_i_is_in_gnr) {
                gnr_cond[i] <- TRUE
            } else {
                gnr_cond[i] <- FALSE
            }
        }


        # Combine the results of these 2 tests ---------------------------------

        consistent <- tnrs_cond & gnr_cond
        conds <- tibble(
            tnrs_cond  = tnrs_cond,
            gnr_cond   = gnr_cond,
            consistent = consistent
        )


        # Fetch the names that GNR recommends ----------------------------------
        # (whether gnr_cond is true or not)
        # (in order to diagnose TRUE/FALSE validity in the outputs by hand :))
        # (used by print_gnr_matches() below!)

        gnr_matched_names <- vector(
            "list",
            length = length(tnrs_df$submittedname[index])
        )
        for (i in seq_along(tnrs_df$submittedname[index])) {
            gnr_matched_names[[i]] <- gnr_df_indexed %>%
                filter(user_supplied_name ==
                           tnrs_df$submittedname[index][i]) %>%
                dplyr::select(binom_only_matched_name) %>%
                as_vector()
        }

        name_summary <- tibble(
            submitted_name    = tnrs_df$submittedname[index],
            tnrs_matched_name = tnrs_df$matchedname[index],
            gnr_matched_names = gnr_matched_names
        )


        # Return ---------------------------------------------------------------

        return(list(
            gnr_df = gnr_df_indexed,
            conds = as_tibble(cbind(conds, name_summary))
        ))

    }


    # Run all together ---------------------------------------------------------

    tnrs_actual <- do_tnrs(flora$species, name_range)
    gnr_actual <- do_gnr(flora$species, name_range)
    consistency_check <- is_consistent(tnrs_actual, gnr_actual)
    flagged <- consistency_check$conds %>%
        filter(not(consistent))

    return(list(
        tnrs_actual              = tnrs_actual,
        gnr_actual               = gnr_actual,
        consistency_check_gnr_df = consistency_check$gnr_df,
        consistency_check_conds  = consistency_check$conds,
        flagged                  = flagged
    ))

}
