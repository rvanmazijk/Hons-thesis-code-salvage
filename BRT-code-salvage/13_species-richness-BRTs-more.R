# ...
# Hons thesis
# R. van Mazijk

# Now, after the exploration in `12.R`,
# I think I should run all this at default tol and tol = 0.0001,
# just to be safe

p_load(gbm, dismo, parallel, foreach, visreg)
set.seed(20170915)

QDS <- function() {

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

        GCFR <- function() {

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
            GCFR_environment_stack %<>% resample(GCFR_richness_QDS, method = "bilinear")

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
                GCFR_richness_QDS,
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

        }


        SWAFR <- function() {

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
            SWAFR_environment_stack %<>% resample(SWAFR_richness_QDS, method = "bilinear")

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
                SWAFR_richness_QDS,
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

    }

    gbm_step_CV_optimisations <- function() {

        tol_0.001 <- function() {

            tree_complexities <- 1:5
            learning_rates <- c(
                0.01,
                0.005,
                0.001,
                0.0005,
                0.0001
            )
            set_3 <- which(names(GCFR_tibble) %in% c(
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

            out_dir <- "G:/gbm_step_outputs_2017-09-30/QDS/"

            out_dir_GCFR <- paste0(out_dir, "GCFR/")
            out_dir_SWAFR <- paste0(out_dir, "SWAFR/")
            out_dir_BOTH <- paste0(out_dir, "BOTH/")

            # FIXME
            #cluster <- makeCluster(detectCores() - 1, outfile = "")
            #registerDoParallel(cluster, detectCores() - 1)
            #foreach(region_tibble = c(GCFR_tibble, SWAFR_tibble),
            #        region_dir = c(out_dir_GCFR, out_dir_SWAFR)) %dopar% {
            #    mega_gbm_step_loop_ijk(
            #        data = as.data.frame(
            #            dplyr::mutate(
            #                dplyr::filter(
            #                    region_tibble,
            #                    "richness" > 0
            #                ),
            #                "richness" = log("richness")
            #            )
            #        ),
            #        out_dir                   = region_dir,
            #        gbm.x                     = set_3,
            #        gbm.y                     = 1,
            #        tolerances                = 0.001,  # Default
            #        tree_complexities         = tree_complexities,
            #        learning_rates            = learning_rates
            #    )
            #}
            #stopImplicitCluster()

            GCFR <- function() {
                mega_gbm_step_loop_ijk(
                    data = GCFR_tibble %>%
                        filter(richness > 0) %>%
                        mutate(richness = log(richness)) %>%
                        as.data.frame(),
                    out_dir                   = out_dir_GCFR,
                    gbm.x                     = set_3,
                    gbm.y                     = 1,
                    tolerances                = 0.001,  # Default
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
                    tolerances                = 0.001,  # Default
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
                    tolerances                = 0.001,  # Default
                    tree_complexities         = tree_complexities,
                    learning_rates            = learning_rates
                )
            }

        }

        tol_0.0001 <- function() {

            tree_complexities <- 1:5
            learning_rates <- c(
                0.01,
                0.005,
                0.001,
                0.0005,
                0.0001
            )
            set_3 <- which(names(GCFR_tibble) %in% c(
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

            out_dir <- "G:/gbm_step_outputs_2017-09-30/QDS/"

            out_dir_GCFR <- paste0(out_dir, "GCFR/")
            out_dir_SWAFR <- paste0(out_dir, "SWAFR/")
            out_dir_BOTH <- paste0(out_dir, "BOTH/")

            GCFR <- function() {
                mega_gbm_step_loop_ijk(
                    data = GCFR_tibble %>%
                        filter(richness > 0) %>%
                        mutate(richness = log(richness)) %>%
                        as.data.frame(),
                    out_dir                   = out_dir_GCFR,
                    gbm.x                     = set_3,
                    gbm.y                     = 1,
                    tolerances                = 0.0001,  # Even smaller!
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
                    tolerances                = 0.0001,
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
                    tolerances                = 0.0001,
                    tree_complexities         = tree_complexities,
                    learning_rates            = learning_rates
                )
            }

        }

    }

}
