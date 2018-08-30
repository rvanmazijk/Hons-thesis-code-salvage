#' <description>
#'
#' @param layer RasterLayer to analyse
#' @param var_name The name of the variable `layer` represents
#' @param facts numeric, length ≥ 1, with values ≥ 2,
#'              the factors to which `layer` must be aggregated too.
#' @param seed numeric,
#'             for replicable random sample of raster points to `extract`
#' @param border SpatialPolygonsDatafram,
#'               the region within which random points are generated to `extract`
#' @param how_many_rand_pts How many random points to `extract`
#'
#' @return A list:
#'             1. `layer_agg` list of RasterLayers,
#'                            each an aggregate of `layer`
#'             2. `layer_agg_df` Dataframe verion of `layer_agg`
#'             3. `layer_agg_rough` list of RasterLayers,
#'                                  each the roughness (sensu `raster::terrain`)
#'                                  of a raster from `layer_agg`
#'             4. `layer_agg_rough_df` Dataframe verion of `layer_agg_rough`
#'             5. `abs_vs_rough_df` Combined dataframe of `layer_agg_df` and -`_rough_df`
#'             6. `rand_pts` ...
#'             6. `extracted_layer_agg_rough` ...
#'             7. `models` ...
#'             8. `AICS` ...
#'             9. `roughness_scale_PCA` ...
#'
#' @examples
#' ```r
#' roughness_and_scale_test_results <- analyse_roughness_and_scale(
#'     layer = GCFR_elev,
#'     var_name = "elev",
#'     facts = 2:15,  # default
#'     seed = 57701,  # default
#'     border = GCFR_border,
#'     how_many_rand_pts = 1000  # default
#' )
#' ```
analyse_roughness_and_scale <- function(layer,
                                        var_name,
                                        facts = 2:15,
                                        seed = 57701,
                                        border,
                                        how_many_rand_pts = 1000) {

    aggregate_layer <- function(layer, var_name, facts) {

        # Aggregate
        layer_agg <- layer %>%
            custom_aggregate_loop(facts = facts)
        layer_agg %<>% c(layer, .)
        names(layer_agg)[1] <- "1"

        # Peak
        for (layer in layer_agg) plot(layer)

        # Store as dataframe
        layer_agg_df <- layer_agg %>%
            as_df_raster_vals_list(n = c(1, facts))
        names(layer_agg_df) <- c(var_name, "fact")
        layer_agg_df %<>% as_tibble()

        # FIXME: NAs in layer an issue?
        # TODO: save
        # TODO: plot

        return(list(
            layer_agg = layer_agg,
            layer_agg_df = layer_agg_df
        ))

    }

    roughness_of_agg <- function(layer_agg, var_name, facts) {

        # Calculate
        layer_agg_rough <- layer_agg %>%
            map(terrain, "roughness")

        # Peak
        for (layer in layer_agg_rough) plot(layer)

        # Store as dataframe
        layer_agg_rough_df <- layer_agg_rough %>%
            as_df_raster_vals_list(n = c(1, facts))
        names(layer_agg_rough_df) <- c(
            glue("rough_agg_{var_name}"),
            "fact"
        )
        layer_agg_rough_df %<>% as_tibble()

        # FIXME: NAs in layer an issue?
        # TODO: save
        # TODO: plot

        return(list(
            layer_agg_rough = layer_agg_rough,
            layer_agg_rough_df = layer_agg_rough_df
        ))

    }

    combine_absolute_and_rough_dataframes <- function(layer_agg_df,
                                                      layer_agg_rough_df,
                                                      var_name) {

        abs_vs_rough_df <-
            cbind(
                layer_agg_df["fact"],
                layer_agg_df[var_name],
                layer_agg_rough_df[glue("rough_agg_{var_name}")]
            ) %>%
            as_tibble()
        names(abs_vs_rough_df) <- c(
            "fact",
            var_name,
            glue("rough_agg_{var_name}")
        )

        # Peak
        summary(abs_vs_rough_df)

        # FIXME: NAs in layer an issue?
        # TODO: save
        # TODO: plot?

        return(abs_vs_rough_df)

    }

    extract_at_sample_pts <- function(layer_agg_rough, border, var_name) {

        # So that I can send these values to be modelled, below

        # Make random pts
        set.seed(seed)
        rand_pts <- layer %>%
            mask(border) %>%
            sampleRandom(
                size = how_many_rand_pts,
                xy = TRUE,
                sp = TRUE,
                na.rm = TRUE
            )
        # Has absolute layer values as a variable already :)

        # Get roughness layer values
        extracted_layer_agg_rough <- layer_agg_rough %>%
            map(raster::extract, rand_pts, sp = TRUE) %>%
            map(dplyr::as_tibble) %>%
            map(dplyr::select, x, y, roughness)
        extracted_layer_agg_rough

        # Tidy up and combine absolute and roughness layer extracted values
        # Rename list's tibbles
        for (i in 1:(length(facts) + 1)) {
            names(extracted_layer_agg_rough[[i]]) <- c(
                "lon",
                "lat",
                glue("rough_agg_{var_name}")
            )
        }
        # Add `pt_ID` column to all tibbles (i.e. each "fact") in list
        extracted_layer_agg_rough %<>%
            map(function(x) {
                x %<>% cbind(pt_ID = rownames(.), .)
            })
        # Add `fact` column to each tibble (identical *within* each tibble)
        for (i in 1:(length(facts) + 1)) {
            extracted_layer_agg_rough[[i]] %<>%
                cbind(fact = names(extracted_layer_agg_rough)[i]) %>%
                as_tibble()
        }
        # Reshape tibbles
        extracted_layer_agg_rough %<>%
            reshape2::melt(
                id.vars = c("lon", "lat", "pt_ID", "fact"),
                value.name = glue("rough_agg_{var_name}")
            ) %>%
            dplyr::select(
                lon,
                lat,
                pt_ID,
                fact,
                glue("rough_agg_{var_name}")
            ) %>%
            as_tibble()
        # Make `fact` numeric
        class(extracted_layer_agg_rough$fact) <- "numeric"

        # Peak
        extracted_layer_agg_rough

        # FIXME: NAs in layer an issue?
        # TODO: save
        # TODO: plot

        return(list(
            rand_pts = rand_pts,
            extracted_layer_agg_rough = extracted_layer_agg_rough
        ))

    }

    regress_roughness_on_scales <- function(extracted_layer_agg_rough,
                                            var_name) {

        # Make model formulae
        model_formulae <- extracted_layer_agg_rough %$% list(
            lin = glue("    rough_agg_{var_name}  ~     fact"),
            log = glue("    rough_agg_{var_name}  ~ log(fact)"),
            exp = glue("log(rough_agg_{var_name}) ~     fact"),
            llg = glue("log(rough_agg_{var_name}) ~ log(fact)"),
            qud = glue("    rough_agg_{var_name}  ~     fact  + I(    fact  ^ 2)"),
            qxp = glue("log(rough_agg_{var_name}) ~     fact  + I(    fact  ^ 2)"),
            qlg = glue("    rough_agg_{var_name}  ~ log(fact) + I(log(fact) ^ 2)"),
            qll = glue("log(rough_agg_{var_name}) ~ log(fact) + I(log(fact) ^ 2)")
        )

        # Fit with those
        models <- model_formulae %>%
            map(lm, data = extracted_layer_agg_rough)
        # Summarise with AIC
        AICs <- models %>%
            map(AIC) %>%
            unlist()
        delta_AICs <- AICs - min(AICs)
        w_i <- exp(-0.5 * delta_AICs) / sum(exp(-0.5 * delta_AICs))
        AICs %<>% cbind(
            model_formulae,
            .,
            delta_AICs,
            w_i
        )

        # Peak
        AICs
        # TODO: plots

        # TODO: save

        return(list(
            models = models,
            AICs = AICs
        ))

    }

    do_PCA_roughness_across_scales <- function(extracted_layer_agg_rough,
                                               var_name) {

        # Organise `extracted_layer_agg_rough` for use with `prcomp()`
        my_key <- "fact"
        my_value <- glue("rough_agg_{var_name}")
        extracted_layer_agg_rough_for_PCA <- extracted_layer_agg_rough %>%
            spread_(
                key_col = my_key,
                value_col = my_value
            ) %>%
            na.omit()

        # Run `prcomp()` without the lon, lat, and pt_ID columns
        roughness_scale_PCA <- prcomp(
            extracted_layer_agg_rough_for_PCA[, -c(1:3)]
        )

        # TODO: plots
        # TODO: save

        return(roughness_scale_PCA)

    }

    layer_agg <- aggregate_layer(
        layer,
        var_name,
        facts
    )
    layer_agg_df <- layer_agg$layer_agg_df
    layer_agg <- layer_agg$layer_agg

    layer_agg_rough <- roughness_of_agg(
        layer_agg,
        var_name,
        facts
    )
    layer_agg_rough_df <- layer_agg_rough$layer_agg_rough_df
    layer_agg_rough <- layer_agg_rough$layer_agg_rough

    abs_vs_rough_df <- combine_absolute_and_rough_dataframes(
        layer_agg_df,
        layer_agg_rough_df,
        var_name
    )

    extracted_layer_agg_rough <- extract_at_sample_pts(
        layer_agg_rough,
        border,
        var_name
    )
    rand_pts <- extracted_layer_agg_rough$rand_pts
    extracted_layer_agg_rough <-
        extracted_layer_agg_rough$extracted_layer_agg_rough

    models <- regress_roughness_on_scales(
        extracted_layer_agg_rough,
        var_name
    )
    AICS <- models$AICs
    models <- models$models

    roughness_scale_PCA <- do_PCA_roughness_across_scales(
        extracted_layer_agg_rough,
        var_name
    )

    return(list(
        layer_agg                 = layer_agg,
        layer_agg_df              = layer_agg_df,
        layer_agg_rough           = layer_agg_rough,
        layer_agg_rough_df        = layer_agg_rough_df,
        abs_vs_rough_df           = abs_vs_rough_df,
        rand_pts                  = rand_pts,
        extracted_layer_agg_rough = extracted_layer_agg_rough,
        models                    = models,
        AICS                      = AICS,
        roughness_scale_PCA       = roughness_scale_PCA
    ))

}

# Testing

if (FALSE) {
    # Run
    roughness_and_scale_test_results <- analyse_roughness_and_scale(
        layer = GCFR_elev,
        var_name = "elev",
        facts = 2:15,  # default
        seed = 57701,  # default
        border = GCFR_border,
        how_many_rand_pts = 1000  # default
    )
    # Explore results
    roughness_and_scale_test_results
    summary(roughness_and_scale_test_results)
    plot(GCFR_elev)
    plot(
        roughness_and_scale_test_results$rand_pts,
        add = TRUE
    )
}
