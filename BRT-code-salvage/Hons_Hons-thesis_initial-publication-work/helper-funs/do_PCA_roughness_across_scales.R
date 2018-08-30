do_PCA_roughness_across_scales <- function(extracted_layer_agg_rough,
                                           var_name,
                                           already_spread = FALSE) {

    if (already_spread) {

        extracted_layer_agg_rough_for_PCA <- extracted_layer_agg_rough

    } else {

        # Organise `extracted_layer_agg_rough` for use with `prcomp()`
        my_key <- "fact"
        my_value <- glue("rough_agg_{var_name}")
        extracted_layer_agg_rough_for_PCA <- extracted_layer_agg_rough %>%
            spread_(
                key_col = my_key,
                value_col = my_value
            ) %>%
            na.omit()

    }

    # Run `prcomp()` without the lon, lat, and pt_ID columns
    roughness_scale_PCA <- prcomp(
        extracted_layer_agg_rough_for_PCA[, -c(1:4)]
    )

    return(roughness_scale_PCA)

}
