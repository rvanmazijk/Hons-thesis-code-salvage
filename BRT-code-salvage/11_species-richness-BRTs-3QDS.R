# Fitting log(richness) ~ E + H @ **3QDS** scale, w/ BRTs
# Hons thesis
# R. van Mazijk
# (with modified code from M.D. Cramer)

p_load(gbm, dismo, parallel, foreach, visreg)
set.seed(20170915)

import_soils_sethsPC <- function() {

    giswd <- "G:/GIS_backup_2017-09-27/"

    GCFR_soils <- stack_soils(region = "GCFR", regiondir = paste(sep = "/",
                                                                 giswd,
                                                                 "SoilGrids250m",
                                                                 "GCFR"
    ))
    proj4string(GCFR_soils) == std_CRS  # TRUE
    res(GCFR_soils)
    # Re-crop to box, because when read-in the extent pops back global
    GCFR_soils %<>% crop(GCFR_box)

    SWAFR_soils <- stack_soils(region = "SWAFR", regiondir = paste(sep = "/",
                                                                   giswd,
                                                                   "SoilGrids250m",
                                                                   "SWAFR"
    ))
    proj4string(SWAFR_soils) == std_CRS  # TRUE
    res(SWAFR_soils)
    SWAFR_soils %<>% crop(SWAFR_box)

}

assemble_data <- function() {

    # GCFR ---------------------------------------------------------------------

    GCFR_environment_list <- list(
        GCFR_elev,
        GCFR_MAP,
        GCFR_PWQ,
        GCFR_PDQ,
        GCFR_PCV,
        GCFR_MLST,
        GCFR_TWQ,
        GCFR_TCQ,
        GCFR_NDVI,
        GCFR_soils
    )

    # Crop all envt layers to same extent
    # Smallest extent is MLST's
    smallest_extent <- extent(GCFR_MLST)
    GCFR_environment_list %<>% map(crop, smallest_extent)
    GCFR_environment_stack <- stack(GCFR_environment_list)

    # Resample all to QDS resolution
    GCFR_environment_stack %<>% resample(GCFR_richness_3QDS, method = "bilinear")

    plot(GCFR_environment_stack)

    # Make roughness versions
    GCFR_roughness_list <- GCFR_environment_stack %>%
        (function(x) {
            out <- list()
            for (i in 1:nlayers(x)) {
                out[[i]] <- terrain(
                    x[[i]],
                    "roughness"
                )
                names(out)[i] <- glue("rough_{names(x)[i]}")
            }
            return(out)
        })
    GCFR_roughness_stack <- stack(GCFR_roughness_list)

    # Combine w/ richness data
    GCFR_all_stack <- stack(
        GCFR_richness_3QDS,
        GCFR_environment_stack,
        GCFR_roughness_stack
    )
    GCFR_tibble <- GCFR_all_stack[] %>%
        as_tibble() %>%
        na.omit()
    names(GCFR_tibble)
    names(GCFR_tibble)[1] <- "richness"
    names(GCFR_tibble)[2:19] <- c(
        "elev",
        "MAP",
        "PWQ",
        "PDQ",
        "PCV",
        "MLST",
        "TWQ",
        "TCQ",
        "NDVI",
        "CECSOL",
        "BLDFIE",
        "CLYPPT",
        "CRFVOL",
        "OCDENS",
        "PHIKCL",
        "SLTPPT",
        "SNDPPT",
        "AWCh1"
    )
    names(GCFR_tibble)[20:37] <- glue("rough_{names(GCFR_tibble)[2:19]}")


    # SWAFR ---------------------------------------------------------------------

    SWAFR_environment_list <- list(
        SWAFR_elev,
        SWAFR_MAP,
        SWAFR_PWQ,
        SWAFR_PDQ,
        SWAFR_PCV,
        SWAFR_MLST,
        SWAFR_TWQ,
        SWAFR_TCQ,
        SWAFR_NDVI,
        SWAFR_soils
    )

    # Crop all envt layers to same extent
    # Smallest extent is MLST's
    smallest_extent <- extent(SWAFR_MLST)
    SWAFR_environment_list %<>% map(crop, smallest_extent)
    SWAFR_environment_stack <- stack(SWAFR_environment_list)

    # Resample all to QDS resolution
    SWAFR_environment_stack %<>% resample(SWAFR_richness_3QDS, method = "bilinear")

    plot(SWAFR_environment_stack)

    # Make roughness versions
    SWAFR_roughness_list <- SWAFR_environment_stack %>%
        (function(x) {
            out <- list()
            for (i in 1:nlayers(x)) {
                out[[i]] <- terrain(
                    x[[i]],
                    "roughness"
                )
                names(out)[i] <- glue("rough_{names(x)[i]}")
            }
            return(out)
        })
    SWAFR_roughness_stack <- stack(SWAFR_roughness_list)

    # Combine w/ richness data
    SWAFR_all_stack <- stack(
        SWAFR_richness_3QDS,
        SWAFR_environment_stack,
        SWAFR_roughness_stack
    )
    SWAFR_tibble <- SWAFR_all_stack[] %>%
        as_tibble() %>%
        na.omit()
    names(SWAFR_tibble)
    names(SWAFR_tibble)[1] <- "richness"
    names(SWAFR_tibble)[2:19] <- c(
        "elev",
        "MAP",
        "PWQ",
        "PDQ",
        "PCV",
        "MLST",
        "TWQ",
        "TCQ",
        "NDVI",
        "CECSOL",
        "BLDFIE",
        "CLYPPT",
        "CRFVOL",
        "OCDENS",
        "PHIKCL",
        "SLTPPT",
        "SNDPPT",
        "AWCh1"
    )
    names(SWAFR_tibble)[20:37] <- glue("rough_{names(SWAFR_tibble)[2:19]}")

}

gbm_step_CV_optimisations <- function() {

    w_predictor_set_3_log_richness <- function() {

        # And w/ max(nt) = 10,000 (default) again

        tree_complexities <- 1:5  # only up to 5-way interactions, *please*!

        learning_rates <- c(  # From Protea-SDMs code
            0.01,
            0.005,
            0.001,
            0.0005,
            0.0001
        )

        out_dir <-
            "G:/mega_gbm_step_loop_ijk_outputs_3QDS_2017-09-30/w-collinearity-checks/"
        out_dir_set_3 <- paste0(out_dir, "set_3/")

        out_dir_GCFR <- paste0(out_dir_set_3, "GCFR/")
        out_dir_SWAFR <- paste0(out_dir_set_3, "SWAFR/")
        out_dir_BOTH <- paste0(out_dir_set_3, "BOTH/")

        # And now this (following collinearity checks +
        # notebook notes):

        set_3 <- which(names(GCFR_tibble) %in% c(  # POSITIVE subsetb now (safer)
            "PCV",    "rough_PCV",
            "MLST",   "rough_MLST",
            "elev",   "rough_elev",
            "SLTPPT",
            "TCQ",    "rough_TCQ",
            "CRFVOL", "rough_CRFVOL",
            "CLYPPT", "rough_CLYPPT",
            "CECSOL", "rough_CECSOL",
            "rough_OCDENS",
            "BLDFIE", "rough_BLDFIE",
            "PDQ",    "rough_PDQ",
            "NDVI",   "rough_NDVI",
            "SNDPPT",
            "MAP",    "rough_MAP"
        ))

        GCFR_ <- function() {
            mega_gbm_step_loop_ijk(
                data = GCFR_tibble %>%
                    filter(richness > 0) %>%
                    mutate(richness = log(richness)) %>%
                    as.data.frame(),
                out_dir                   = out_dir_GCFR,
                gbm.x                     = set_3,
                gbm.y                     = 1,
                tolerances                = 0.0005,
                tree_complexities         = tree_complexities,
                learning_rates            = learning_rates
            )
        }

        SWAFR <- function() {
            mega_gbm_step_loop_ijk(
                data = SWAFR_tibble %>%
                    filter(richness > 0) %>%
                    mutate(richness = log(richness)) %>%
                    as.data.frame(),
                out_dir                   = out_dir_SWAFR,
                gbm.x                     = set_3,
                gbm.y                     = 1,
                tolerances                = 0.0005,
                tree_complexities         = tree_complexities,
                learning_rates            = learning_rates
            )
        }

        BOTH <- function() {
            set_3 <- which(c(names(GCFR_tibble), "region") %in% c(
                "PCV",    "rough_PCV",
                "MLST",   "rough_MLST",
                "elev",   "rough_elev",
                "SLTPPT",
                "TCQ",    "rough_TCQ",
                "CRFVOL", "rough_CRFVOL",
                "CLYPPT", "rough_CLYPPT",
                "CECSOL", "rough_CECSOL",
                "rough_OCDENS",
                "BLDFIE", "rough_BLDFIE",
                "PDQ",    "rough_PDQ",
                "NDVI",   "rough_NDVI",
                "SNDPPT",
                "MAP",    "rough_MAP",
                "region"  # !!! NB !!!
            ))
            mega_gbm_step_loop_ijk(
                data = as.data.frame(rbind(
                    GCFR_tibble %>%
                        filter(richness > 0) %>%
                        mutate(
                            richness = log(richness),
                            region = factor("GCFR")
                        ),
                    SWAFR_tibble %>%
                        filter(richness > 0) %>%
                        mutate(
                            richness = log(richness),
                            region = factor("SWAFR")
                        )
                )),
                out_dir                   = out_dir_BOTH,
                gbm.x                     = set_3,
                gbm.y                     = 1,
                tolerances                = 0.0005,
                tree_complexities         = tree_complexities,
                learning_rates            = learning_rates
            )
        }

    }

}

investigating_those_optimisations_redo <- function() {

    w_predictor_set_3_log_richness <- function() {

        out_dir <-
            "G:/mega_gbm_step_loop_ijk_outputs_HDS_2017-09-30/w-collinearity-checks/"
        out_dir_set_3 <- paste0(out_dir, "set_3/")

        out_dir_GCFR <- paste0(out_dir_set_3, "GCFR/")
        out_dir_SWAFR <- paste0(out_dir_set_3, "SWAFR/")
        out_dir_BOTH <- paste0(out_dir_set_3, "BOTH/")

        GCFR <- function() {

            saved_BRTs_tol_0.0005 <- get_saved_BRTs(
                dir = out_dir_GCFR,
                tol = "tol-5e-04"
            )
            saved_BRTs <- c(saved_BRTs_tol_0.0005)
            summary(saved_BRTs)

            GCFR_set_3_model_summaries <-
                foreach(model = saved_BRTs) %do% {
                    model %$% list(
                        tolerance = gbm.call$tolerance,
                        tc = gbm.call$tree.complexity,
                        lr = gbm.call$learning.rate,
                        pseudo_r2 = pseudo_r2(
                            cv.statistics$deviance.mean,
                            self.statistics$mean.null
                        ),
                        AUC = self.statistics$discrimination,
                        cv_AUC = cv.statistics$discrimination.mean,
                        deviance = self.statistics$mean.resid,
                        cv_deviance = cv.statistics$deviance.mean
                    )
                }
            for (i in seq_along(GCFR_set_3_model_summaries)) {
                names(GCFR_set_3_model_summaries)[i] <-
                    names(saved_BRTs)[i]
            }
            GCFR_set_3_model_summaries %<>%
                map(unlist) %>%
                as_tibble() %>%
                t() %>%
                as_tibble() %>%
                mutate(model_name = names(saved_BRTs))
            names(GCFR_set_3_model_summaries) <- c(
                "tolerance",
                "tc",
                "lr",
                "pseudo_r2",
                "AUC",
                "cv_AUC",
                "deviance",
                "cv_deviance",
                "model_name"
            )

            summary(GCFR_set_3_model_summaries)

            ggplot(GCFR_set_3_model_summaries) +
                geom_tile(aes(x = tc, y = as.factor(lr), fill = pseudo_r2)) +
                scale_fill_distiller(palette =  "Spectral") +
                facet_wrap(~ tolerance)

            summary(lm(
                pseudo_r2 ~ lr + tc + tolerance,
                data = GCFR_set_3_model_summaries
            ))

            visreg::visreg(lm(
                pseudo_r2 ~ lr + tc + tolerance,
                data = GCFR_set_3_model_summaries
            ))

            # Conclusion: gonna go with tolerance 0.0005 from now on :)


        }

        SWAFR <- function() {

            saved_BRTs_tol_0.001 <- get_saved_BRTs(
                dir = out_dir_SWAFR,
                tol = "tol-0.001"
            )
            saved_BRTs_tol_0.0005 <- get_saved_BRTs(
                dir = out_dir_SWAFR,
                tol = "tol-5e-04"
            )
            saved_BRTs <- c(saved_BRTs_tol_0.001, saved_BRTs_tol_0.0005)
            summary(saved_BRTs)

            SWAFR_set_3_model_summaries <-
                foreach(model = saved_BRTs) %do% {
                    model %$% list(
                        tolerance = gbm.call$tolerance,
                        tc = gbm.call$tree.complexity,
                        lr = gbm.call$learning.rate,
                        pseudo_r2 = pseudo_r2(
                            cv.statistics$deviance.mean,
                            self.statistics$mean.null
                        ),
                        AUC = self.statistics$discrimination,
                        cv_AUC = cv.statistics$discrimination.mean,
                        deviance = self.statistics$mean.resid,
                        cv_deviance = cv.statistics$deviance.mean
                    )
                }
            for (i in seq_along(SWAFR_set_3_model_summaries)) {
                names(SWAFR_set_3_model_summaries)[i] <-
                    names(saved_BRTs)[i]
            }
            SWAFR_set_3_model_summaries %<>%
                map(unlist) %>%
                as_tibble() %>%
                t() %>%
                as_tibble() %>%
                mutate(model_name = names(saved_BRTs))
            names(SWAFR_set_3_model_summaries) <- c(
                "tolerance",
                "tc",
                "lr",
                "pseudo_r2",
                "AUC",
                "cv_AUC",
                "deviance",
                "cv_deviance",
                "model_name"
            )

            summary(SWAFR_set_3_model_summaries)

            ggplot(SWAFR_set_3_model_summaries) +
                geom_tile(aes(x = tc, y = as.factor(lr), fill = pseudo_r2)) +
                scale_fill_distiller(palette =  "Spectral") +
                facet_wrap(~ tolerance)

        }

        BOTH <- function() {
            # TODO
        }

    }

}

choosing_one_config_to_refit_and_simplify <- function() {

    # TODO

}
