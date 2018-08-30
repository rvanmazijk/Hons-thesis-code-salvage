make_all_vars_tidy_PCs <- function(x, var_names = names(x)) {

    make_tidy_PCs <- function(x, var_name, var_type) {
        x %$%
            tibble(
                PC1 = PCA$x[, 1],
                PC2 = PCA$x[, 2],
                region = extracted_pts$region,
                variable = var_name,
                variable_type = var_type
            ) %>%
            gather(PC, PC_value, -region, -variable, -variable_type)
    }

    # For labelling as climatic or soil
    climate_et_al <- c(
        "elev",
        "MAP",
        "MLST",
        "NDVI",
        "PCV",
        "PDQ",
        "PWQ",
        "TCQ",
        "TWQ"
    )

    # "Grow" the tibble through all envrionmental variables
    tidy_PCs <- tibble()
    for (i in seq_along(x)) {
        tidy_PCs %<>% rbind(
            make_tidy_PCs(
                x[[i]],
                var_names[i],
                var_type = if (var_names[i] %in% climate_et_al) "climate"
                           else "soil"
            )
        )
    }
    all_vars_tidy_PCs <- tidy_PCs

    # Tidy-up
    all_vars_tidy_PCs %<>% mutate(
        region = as.factor(region),
        variable = as.factor(variable),
        variable_type = as.factor(variable_type),
        PC = as.factor(PC)
    )

    # Rename PC label levels
    levels(all_vars_tidy_PCs$PC) <- c(
        "PC1\nroughness",
        "PC2\nspatial scale"
    )

    # Create re-scaled PC values as Z-scorres
    all_vars_tidy_PCs %<>%
        group_by(variable, PC) %>%
        mutate(PC_value_z = map_to_z(PC_value))

    return(all_vars_tidy_PCs)

}
