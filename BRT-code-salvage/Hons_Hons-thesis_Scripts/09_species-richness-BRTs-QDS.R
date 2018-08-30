# Fitting log(richness) ~ E + H @ **QDS** scale, w/ BRTs
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
                names(out)[i] <- glue("
                    rough_{names(x)[i]}
                ")
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
    names(GCFR_tibble)[20:37] <- glue("
        rough_{names(GCFR_tibble)[2:19]}
    ")


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
                names(out)[i] <- glue("
                rough_{names(x)[i]}
            ")
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
    names(SWAFR_tibble)[20:37] <- glue("
        rough_{names(SWAFR_tibble)[2:19]}
    ")

}

gbm_step_CV_optimisations <- function() {

    good_day_run <- function() {

        # w/o collinearity checks

        # 2017-09-26 15h47 ---

        # Following some thoughts and fiddles (see previous commits + paper notes),
        # I have found that tweaking the tolerance setting allows me to get
        # more than 50 trees in a model and still keep it Gaussian!
        # (Eat it, Poisson!)

        # Now, to implem that which I have scribbled in my notebook

        tolerances <- c(
            0.001,  # default in gbm.step()
            0.0005,
            0.0001,
            0.00005,
            0.00001,
            0.000009,
            0.000008,  # my guess is that the best is > 7, < 8
            0.000007,
            0.000006,
            0.000005  # gotta stop somewhere!
        )

        tree_complexities <- 1:5  # only up to 5-way interactions, *please*!

        learning_rates <- c(  # From Protea-SDMs code
            0.01,
            0.005,
            0.001,
            0.0005,
            0.0001
        )

        out_dir <- "F:/mega_gbm_step_loop_ijk_outputs_2017-09-26/"
        out_dir_GCFR <- paste0(out_dir, "w-o-collinearity-checks/GCFR/")
        out_dir_SWAFR <- paste0(out_dir, "w-o-collinearity-checks/SWAFR/")

        GCFR <- function() {

            mega_gbm_step_loop_ijk(
                data                      = as.data.frame(GCFR_tibble),
                out_dir                   = out_dir_GCFR,
                do_tolerances_in_parallel = TRUE,
                gbm.x                     = 2:37,
                gbm.y                     = 1,
                tolerances                = tolerances,
                tree_complexities         = tree_complexities,
                learning_rates            = learning_rates
            )

        }

        SWAFR <- function() {

            mega_gbm_step_loop_ijk(
                data                      = as.data.frame(SWAFR_tibble),
                out_dir                   = out_dir_SWAFR,
                do_tolerances_in_parallel = TRUE,
                gbm.x                     = 2:37,
                gbm.y                     = 1,
                tolerances                = tolerances,
                tree_complexities         = tree_complexities,
                learning_rates            = learning_rates
            )

        }

        BOTH <- function() {

            out_dir_BOTH <- paste0(out_dir, "w-o-collinearity-checks/BOTH/")

            # TODO
            # ? need region label column?
            mega_gbm_step_loop_ijk(
                data                      = rbind(GCFR_tibble, SWAFR_tibble),
                out_dir                   = out_dir_BOTH,
                do_tolerances_in_parallel = TRUE,
                gbm.x                     = 2:37,
                gbm.y                     = 1,
                tolerances                = tolerances,
                tree_complexities         = tree_complexities,
                learning_rates            = learning_rates
            )

        }

    }

    goodBAD_day2_run_redo <- function() {

        # w/ collinearity checks

        # 2017-09-27 09h58 ---

        # Following some thoughts and fiddles (see previous commits + paper notes),
        # I have found that tweaking the tolerance setting allows me to get
        # more than 50 trees in a model and still keep it Gaussian!
        # (Eat it, Poisson!)

        # Now, to implem that which I have scribbled in my notebook


        collinearity_checks <- function() {

            # NB: must do for GCFR + SWAFR together to get common predictor set

            p_load(virtualspecies)
            install.packages("virtualspecies", dep = TRUE)

            names(GCFR_all_stack) <- names(GCFR_tibble)
            names(SWAFR_all_stack) <- names(SWAFR_tibble)
            BOTH_all_stack <- merge(GCFR_all_stack, SWAFR_all_stack)
            names(BOTH_all_stack) <- names(GCFR_tibble)

            removeCollinearity(
                raster.stack = BOTH_all_stack[[-1]],
                multicollinearity.cutoff = 0.75,  # my intuition
                select.variables = FALSE,
                sample.points = FALSE,
                plot = TRUE
            )
            # Note: get same resulting clusters with cutoff = 0.8

            # Now, do I choose? Or make it arb?
            # Well, if it *is* ultimately arb (as Mike says),
            # then it won't matter if I *do* choose, hehe...
            #
            # <paper notes interval>

            # Redo-note:
            # Collinearity checks luckily DIDN'T include species richness.
            # GOOD!

        }

        actual_run <- function() {

            tolerances <- c(
                0.001,  # default in gbm.step()
                0.0005,
                0.0001,
                0.00005,
                0.00001,
                0.000009,
                0.000008,  # my guess is that the best is > 7, < 8
                0.000007,
                0.000006,
                0.000005  # gotta stop somewhere!
            )

            tree_complexities <- 1:5  # only up to 5-way interactions, *please*!

            learning_rates <- c(  # From Protea-SDMs code
                0.01,
                0.005,
                0.001,
                0.0005,
                0.0001
            )

            out_dir <-
                "G:/mega_gbm_step_loop_ijk_outputs_2017-09-28/w-collinearity-checks/"
            out_dir_set_1 <- paste0(out_dir, "set_1/")
            out_dir_set_2 <- paste0(out_dir, "set_2/")

            out_dir_GCFR <- list(
                set_1_dir = paste0(out_dir_set_1, "GCFR/"),
                set_2_dir = paste0(out_dir_set_2, "GCFR/")
            )
            out_dir_SWAFR <- list(
                set_1_dir = paste0(out_dir_set_1, "SWAFR/"),
                set_2_dir = paste0(out_dir_set_2, "SWAFR/")
            )
            out_dir_BOTH <- list(
                set_1_dir = paste0(out_dir_set_1, "BOTH/"),
                set_2_dir = paste0(out_dir_set_2, "BOTH/")
            )


            # And now this (following collinearity checks +
            #  notebook notes):

            predictor_sets <- list(
                set_1 = which(names(GCFR_tibble) %notin% c(
                    "richness",  # Redo-note: YESSSSS! NOOOOOOW IT'LL WORK :-(/B-)
                    "PWQ", "rough_PWQ",
                    "TWQ", "rough_TWQ",
                    "NDVI",
                    "BLDFIE",  # t.f. keep SOC (OCDENS)
                    "SNDPPT", "rough_SNDPPT",
                    "AWCh1", "rough_AWCh1",
                    "rough_SLTPPT",
                    "rough_CLYPPT"
                )),
                set_2 = which(names(GCFR_tibble) %notin% c(
                    "richness",  # Redo-note: YESSSSS! NOOOOOOW IT'LL WORK :-(/B-(
                    "PWQ", "rough_PWQ",
                    "TWQ", "rough_TWQ",
                    "NDVI",
                    "OCDENS",  # t.f. keeps BLDFIE
                    "SNDPPT", "rough_SNDPPT",
                    "AWCh1", "rough_AWCh1",
                    "rough_SLTPPT",
                    "rough_CLYPPT"
                ))
            )

            GCFR <- function() {

                pmap(
                    list(
                        set = predictor_sets,
                        out_dir = out_dir_GCFR
                    ),
                    function(set, out_dir) {
                        mega_gbm_step_loop_ijk(
                            data                      = as.data.frame(GCFR_tibble),
                            out_dir                   = out_dir,
                            do_tolerances_in_parallel = TRUE,
                            gbm.x                     = set,
                            gbm.y                     = 1,
                            tolerances                = tolerances,
                            tree_complexities         = tree_complexities,
                            learning_rates            = learning_rates
                        )
                    }
                )

            }

            SWAFR <- function() {

                pmap(
                    list(
                        set = predictor_sets,
                        out_dir = out_dir_SWAFR
                    ),
                    function(set, out_dir) {
                        mega_gbm_step_loop_ijk(
                            data                      = as.data.frame(SWAFR_tibble),
                            out_dir                   = out_dir,
                            do_tolerances_in_parallel = TRUE,
                            gbm.x                     = set,
                            gbm.y                     = 1,
                            tolerances                = tolerances,
                            tree_complexities         = tree_complexities,
                            learning_rates            = learning_rates
                        )
                    }
                )

            }

            BOTH <- function() {

                # TODO
                # ? need region label column?
                pmap(
                    list(
                        set = predictor_sets,
                        out_dir = out_dir_BOTH
                    ),
                    function(set, out_dir) {
                        mega_gbm_step_loop_ijk(
                            data                      = rbind(GCFR_tibble,
                                                              SWAFR_tibble),
                            out_dir                   = out_dir,
                            do_tolerances_in_parallel = TRUE,
                            gbm.x                     = set,
                            gbm.y                     = 1,
                            tolerances                = tolerances,
                            tree_complexities         = tree_complexities,
                            learning_rates            = learning_rates
                        )
                    }
                )

            }

        }

    }

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
            "G:/mega_gbm_step_loop_ijk_outputs_2017-09-28/w-collinearity-checks/"
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
            #mega_gbm_step_loop_ijk(
            #    data = GCFR_tibble %>%
            #        filter(richness > 0) %>%
            #        mutate(richness = log(richness)) %>%
            #        as.data.frame(),
            #    out_dir                   = out_dir_GCFR,
            #    gbm.x                     = set_3,
            #    gbm.y                     = 1,
            #    tolerances                = 0.001,  # default
            #    tree_complexities         = tree_complexities,
            #    learning_rates            = learning_rates
            #)
            # Let's tweaking the tolerance a bit,
            # just to see if we can get the pseudo-R2 a nit higher? ;)
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
            #mega_gbm_step_loop_ijk(
            #    data = SWAFR_tibble %>%
            #        filter(richness > 0) %>%
            #        mutate(richness = log(richness)) %>%
            #        as.data.frame(),
            #    out_dir                   = out_dir_SWAFR,
            #    gbm.x                     = set_3,
            #    gbm.y                     = 1,
            #    tolerances                = 0.001,  # default
            #    tree_complexities         = tree_complexities,
            #    learning_rates            = learning_rates
            #)
            # Let's tweaking the tolerance a bit,
            # just to see if we can get the pseudo-R2 a nit higher? ;)
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

    w_o_collinearity_checks <- function() {

        out_dir <- "../mega_gbm_step_loop_ijk_outputs_2017-09-26/"
        out_dir_GCFR <- paste0(out_dir, "w-o-collinearity-checks/GCFR/")
        out_dir_SWAFR <- paste0(out_dir, "w-o-collinearity-checks/SWAFR/")

        # Explore by tolerance

        GCFR <- function() {

            saved_BRTs_tol_0.001 <- get_saved_BRTs(
                dir = out_dir_GCFR,
                tol = "tol-0.001"
            )
            saved_BRTs_tol_0.001 %>%
                map(summary)

            # ...
            # TODO
            # ...

        }

        SWAFR <- function() {

            # TODO

        }

    }

    w_predictor_set_1 <- function() {

        out_dir <-
            "G:/mega_gbm_step_loop_ijk_outputs_2017-09-28/"
        out_dir_GCFR <- paste0(out_dir, "w-collinearity-checks/set_1/GCFR/")
        out_dir_SWAFR <- paste0(out_dir, "w-collinearity-checks/set_1/SWAFR/")


        # Exploring by tolerance

        GCFR <- function() {

            saved_BRTs_tol_0.001 <- get_saved_BRTs(
                dir = out_dir_GCFR,
                tol = "tol-0.001"
            ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.0005 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-5e-04"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.0001 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-1e-04"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.00005 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-5e-05"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.00001 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-1e-05"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.000009 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-9e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.000008 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-8e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.000007 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-7e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !! Not all for tol 0.000007 have only one tree! (as expected)
            # Let's look inside (more done below):
            summary(saved_BRTs_tol_0.000007)

            saved_BRTs_tol_0.000006 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-6e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !! Not all for tol 0.000006 have only one tree! (as expected)
            # Let's look inside (more done below):
            summary(saved_BRTs_tol_0.000006)

            saved_BRTs_tol_0.000005 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-5e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            # Save the good-ones for below
            GCFR_good_tolerances_set_1 <- list(
                saved_BRTs_tol_0.000007,
                saved_BRTs_tol_0.000006,
                saved_BRTs_tol_0.000005
            )
            saveRDS(GCFR_good_tolerances_set_1, here::here(
                "Results",
                "BRTs",
                "GCFR_good_tolerances_set_1_2017-09-29.rds"
            ))
            GCFR_good_tolerances_set_1 <- readRDS(here::here(
                "Results",
                "BRTs",
                "GCFR_good_tolerances_set_1_2017-09-29.rds"
            ))
            # TODO: move out of repo ~ largeness

        }

        SWAFR <- function() {

            saved_BRTs_tol_0.001 <- get_saved_BRTs(
                dir = out_dir_SWAFR,
                tol = "tol-0.001"
            ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.0005 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-5e-04"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.0001 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-1e-04"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.00005 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-5e-05"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.00001 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-1e-05"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !!

            saved_BRTs_tol_0.000009 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-9e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !!

            saved_BRTs_tol_0.000008 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-8e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !!

            saved_BRTs_tol_0.000007 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-7e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !! Not all for tol 0.000007 have only one tree! (as expected)
            # Let's look inside (more done below):
            summary(saved_BRTs_tol_0.000007)

            saved_BRTs_tol_0.000006 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-6e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !! Not all for tol 0.000006 have only one tree! (as expected)
            # Let's look inside (more done below):
            summary(saved_BRTs_tol_0.000006)

            saved_BRTs_tol_0.000005 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-5e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            # Save the good-ones for below
            SWAFR_good_tolerances_set_1 <- list(
                saved_BRTs_tol_0.00001,
                saved_BRTs_tol_0.000009,
                saved_BRTs_tol_0.000008,
                saved_BRTs_tol_0.000007,
                saved_BRTs_tol_0.000006,
                saved_BRTs_tol_0.000005
            )
            saveRDS(SWAFR_good_tolerances_set_1, here::here(
                "Results",
                "BRTs",
                "SWAFR_good_tolerances_set_1.rds"
            ))
            SWAFR_good_tolerances_set_1 <- readRDS(here::here(
                "Results",
                "BRTs",
                "SWAFR_good_tolerances_set_1.rds"
            ))
            # TODO: move out of repo ~ largeness

        }

        # Exploring the non-single-tree model sets

        GCFR_good_tolerances_set_1 %>% summary()
        GCFR_good_tolerances_set_1 %>% map(summary)
        GCFR_good_tolerances_set_1 %>% map(map, summary)

        GCFR_set_1_pseudo_R2s <-
            foreach(tol = GCFR_good_tolerances_set_1) %do% {
                foreach(model = tol) %do% {
                    model %$% {
                        pseudo_r2(
                            cv.statistics$deviance.mean,
                            self.statistics$mean.null
                        )
                    }
                }
            }
        names(GCFR_set_1_pseudo_R2s) <- c("tol_7e_06", "tol_6e_06", "tol_5e_06")
        for (i in seq_along(GCFR_set_1_pseudo_R2s)) {
            names(GCFR_set_1_pseudo_R2s[[i]]) <-
                names(GCFR_good_tolerances_set_1[[i]])
        }
        GCFR_set_1_pseudo_R2s_tibble <- GCFR_set_1_pseudo_R2s %>%
            map(as.numeric) %>%
            as.data.frame() %>%
            cbind(
                tc = names(GCFR_set_1_pseudo_R2s[[1]]) %>%
                    str_replace("tol_7e_06_", "") %>%
                    str_split_fixed("_", n = 5) %>%
                    `[`(, 2),
                lr = names(GCFR_set_1_pseudo_R2s[[1]]) %>%
                    str_replace("tol_7e_06_", "") %>%
                    str_split_fixed("_", n = 5) %>%
                    `[`(, 4:5) %>%
                    as_tibble() %>%
                    transmute(lr = glue("{V1}{V2}"))
            ) %>%
            as_tibble() %>%
            gather(tol, pseudo_r2, -tc, -lr) %>%
            mutate(
                tc = as.factor(tc),
                lr = as.factor(lr),
                tol = as.factor(tol)
            )
        ggplot(GCFR_set_1_pseudo_R2s_tibble) +
            geom_point(aes(x = tc, y = pseudo_r2, col = lr)) +
            facet_wrap(~ tol)


        SWAFR_good_tolerances_set_1 %>% summary()
        SWAFR_good_tolerances_set_1 %>% map(summary)
        SWAFR_good_tolerances_set_1 %>% map(map, summary)

        # ...
        # TODO
        # ...

    }

    w_predictor_set_2 <- function() {

        out_dir <- "F:/mega_gbm_step_loop_ijk_outputs_2017-09-26/"
        out_dir_GCFR <- paste0(out_dir, "w-collinearity-checks/set_2/GCFR/")
        out_dir_SWAFR <- paste0(out_dir, "w-collinearity-checks/set_2/SWAFR/")


        # Exploring by tolerance -----------------------------------------------

        GCFR <- function() {

            saved_BRTs_tol_0.001 <- get_saved_BRTs(
                dir = out_dir_GCFR,
                tol = "tol-0.001"
            ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.0005 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-5e-04"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.0001 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-1e-04"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.00005 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-5e-05"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.00001 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-1e-05"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.000009 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-9e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.000008 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-8e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.000007 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-7e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !! Not all for tol 0.000007 have only one tree! (as expected)
            # Let's look inside (more done below):
            summary(saved_BRTs_tol_0.000007)

            saved_BRTs_tol_0.000006 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-6e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !! Not all for tol 0.000006 have only one tree! (as expected)
            # Let's look inside (more done below):
            summary(saved_BRTs_tol_0.000006)

            saved_BRTs_tol_0.000005 <-
                get_saved_BRTs(
                    dir = out_dir_GCFR,
                    tol = "tol-5e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            # Save the good-ones for below
            GCFR_good_tolerances_set_2 <- list(
                saved_BRTs_tol_0.000007,
                saved_BRTs_tol_0.000006,
                saved_BRTs_tol_0.000005
            )

        }

        SWAFR <- function() {

            saved_BRTs_tol_0.001 <- get_saved_BRTs(
                dir = out_dir_SWAFR,
                tol = "tol-0.001"
            ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.0005 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-5e-04"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.0001 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-1e-04"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.00005 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-5e-05"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            saved_BRTs_tol_0.00001 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-1e-05"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !!

            saved_BRTs_tol_0.000009 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-9e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !!

            saved_BRTs_tol_0.000008 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-8e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !!

            saved_BRTs_tol_0.000007 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-7e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !! Not all for tol 0.000007 have only one tree! (as expected)
            # Let's look inside (more done below):
            summary(saved_BRTs_tol_0.000007)

            saved_BRTs_tol_0.000006 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-6e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })
            # !! Not all for tol 0.000006 have only one tree! (as expected)
            # Let's look inside (more done below):
            summary(saved_BRTs_tol_0.000006)

            saved_BRTs_tol_0.000005 <-
                get_saved_BRTs(
                    dir = out_dir_SWAFR,
                    tol = "tol-5e-06"
                ) %T>%
                (function(x) {
                    x %>%
                        map(function(x) x$n.trees) %>%
                        unlist() %>%
                        equals(1) %>%
                        any() %>%
                        not() %>%
                        print()
                })

            # Save the good-ones for below
            SWAFR_good_tolerances_set_2 <- list(
                saved_BRTs_tol_0.00001,
                saved_BRTs_tol_0.000009,
                saved_BRTs_tol_0.000008,
                saved_BRTs_tol_0.000007,
                saved_BRTs_tol_0.000006,
                saved_BRTs_tol_0.000005
            )

        }

        # Exploring the non-single-tree model sets -----------------------------

        GCFR_good_tolerances_set_2 %>% summary()
        GCFR_good_tolerances_set_2 %>% map(summary)
        GCFR_good_tolerances_set_2 %>% map(map, summary)

        SWAFR_good_tolerances_set_2 %>% summary()
        SWAFR_good_tolerances_set_2 %>% map(summary)
        SWAFR_good_tolerances_set_2 %>% map(map, summary)

        # ...
        # TODO
        # ...

    }

    w_predictor_set_3_log_richness <- function() {

        out_dir <-
            "G:/mega_gbm_step_loop_ijk_outputs_2017-09-28/w-collinearity-checks/"
        out_dir_set_3 <- paste0(out_dir, "set_3/")

        out_dir_GCFR <- paste0(out_dir_set_3, "GCFR/")
        out_dir_SWAFR <- paste0(out_dir_set_3, "SWAFR/")
        out_dir_BOTH <- paste0(out_dir_set_3, "BOTH/")

        GCFR <- function() {

            saved_BRTs_tol_0.001 <- get_saved_BRTs(
                dir = out_dir_GCFR,
                tol = "tol-0.001"
            )
            saved_BRTs_tol_0.0005 <- get_saved_BRTs(
                dir = out_dir_GCFR,
                tol = "tol-5e-04"
            )
            saved_BRTs <- c(saved_BRTs_tol_0.001, saved_BRTs_tol_0.0005)
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
