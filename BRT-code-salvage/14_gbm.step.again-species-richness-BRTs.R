# ...
# Hons thesis
# R. van Mazijk

# With same other settings as before: (from mega_gbm_step_loop_ijk.R)
#     > dismo::gbm.step(
#     +     data             = data,
#     +     gbm.x            = gbm.x,
#     +     gbm.y            = gbm.y,
#     +     family           = family,
#     +     tolerance.method = "auto",
#     +     tolerance        = tolerance,
#     +     tree.complexity  = j,
#     +     learning.rate    = k,
#     +     bag.fraction     = 0.75,   # default
#     +     prev.stratify    = TRUE,
#     +     n.folds          = 10,
#     +     silent           = FALSE,
#     +     plot.main        = TRUE,
#     +     n.trees          = 1,      # starting nt
#     +     step.size        = 1,      # nt increment
#     +     max.trees        = 10000   # (default) (esp. for set_3)
#     + )

# Setup ------------------------------------------------------------------------

set.seed(20170915)

{
    giswd <- "/Volumes/RUAN_PVT/GIS_backup_2017-09-27/"

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

# QDS
{

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
    GCFR_tibble_QDS <- GCFR_tibble


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

    SWAFR_tibble_QDS <- SWAFR_tibble

}
# HDS
{

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
    GCFR_environment_stack %<>% resample(GCFR_richness_HDS, method = "bilinear")

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
        GCFR_richness_HDS,
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
    GCFR_tibble_HDS <- GCFR_tibble


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
    SWAFR_environment_stack %<>% resample(SWAFR_richness_HDS, method = "bilinear")

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
        SWAFR_richness_HDS,
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

    SWAFR_tibble_HDS <- SWAFR_tibble

}
# 3QDS
{

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
                names(out)[i] <- glue("
                    rough_{names(x)[i]}
                ")
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
    names(GCFR_tibble)[20:37] <- glue("
        rough_{names(GCFR_tibble)[2:19]}
    ")
    GCFR_tibble_3QDS <- GCFR_tibble


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
                names(out)[i] <- glue("
                rough_{names(x)[i]}
            ")
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
    names(SWAFR_tibble)[20:37] <- glue("
        rough_{names(SWAFR_tibble)[2:19]}
    ")

    SWAFR_tibble_3QDS <- SWAFR_tibble

}


# Actual-runs ------------------------------------------------------------------

# GCFR -------------------------------------------------------------------------

# Import saved BRT-models
GCFR_QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "QDS",
    region = "GCFR"
)
GCFR_HDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "HDS",
    region = "GCFR"
)
GCFR_3QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "3QDS",
    region = "GCFR"
)

# Extract
GCFR_QDS_BRT_candidate <- GCFR_QDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001
GCFR_HDS_BRT_candidate <- GCFR_HDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001
GCFR_3QDS_BRT_candidate <- GCFR_3QDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001

# Import saved gbm.simp's
GCFR_QDS_BRT_candidate.simp <- readRDS(
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "GCFR_QDS_BRT_candidate.simp.rds"
    )
)
GCFR_HDS_BRT_candidate.simp <- readRDS(
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "GCFR_HDS_BRT_candidate.simp.rds"
    )
)
GCFR_3QDS_BRT_candidate.simp <- readRDS(
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "GCFR_3QDS_BRT_candidate.simp.rds"
    )
)

# Re-run with the simp predictor set
# QDS <[x] DONE!>
{
    optimal_no_drops <- GCFR_QDS_BRT_candidate.simp$deviance.summary %$%
        which(mean == min(mean))
    GCFR_QDS_BRT_final <- gbm.step(
        # Use data stored in model-save! :D
        data = GCFR_tibble_QDS %>%
            filter(richness > 0) %>%
            mutate(richness = log(richness)) %>%
            as.data.frame(),
        # Use the predictor set at the optimal no. of dropped predictors
        gbm.x = GCFR_QDS_BRT_candidate.simp$pred.list[[optimal_no_drops]],
        gbm.y = 1,
        family           = "gaussian",
        tolerance.method = "auto",
        tolerance        = 0.0005,
        tree.complexity  = 5,
        learning.rate    = 0.001,
        bag.fraction     = 0.75,   # default
        prev.stratify    = TRUE,
        n.folds          = 10,
        silent           = FALSE,
        plot.main        = TRUE,
        n.trees          = 1,      # starting nt
        step.size        = 1,      # nt increment
        max.trees        = 10000   # (default) (esp. for set_3)
    )
    # Save
    saveRDS(
        GCFR_QDS_BRT_final,
        paste0(
            "/Volumes/RUAN_PVT/gbm_step_simpler_outputs_2017-10-19/",
            "GCFR_QDS_BRT_final.rds"
        )
    )
}
# HDS <[x] Done>
{
    optimal_no_drops <- GCFR_HDS_BRT_candidate.simp$deviance.summary %$%
        which(mean == min(mean))
    GCFR_HDS_BRT_final <- gbm.step(
        # Use data stored in model-save! :D
        data = GCFR_tibble_HDS %>%
            filter(richness > 0) %>%
            mutate(richness = log(richness)) %>%
            as.data.frame(),
        # Use the predictor set at the optimal no. of dropped predictors
        gbm.x = GCFR_HDS_BRT_candidate.simp$pred.list[[optimal_no_drops]],
        gbm.y = 1,
        family           = "gaussian",
        tolerance.method = "auto",
        tolerance        = 0.0005,
        tree.complexity  = 5,
        learning.rate    = 0.001,
        bag.fraction     = 0.75,   # default
        prev.stratify    = TRUE,
        n.folds          = 10,
        silent           = FALSE,
        plot.main        = TRUE,
        n.trees          = 1,      # starting nt
        step.size        = 1,      # nt increment
        max.trees        = 10000   # (default) (esp. for set_3)
    )
    # Save
    saveRDS(
        GCFR_HDS_BRT_final,
        paste0(
            "/Volumes/RUAN_PVT/gbm_step_simpler_outputs_2017-10-19/",
            "GCFR_HDS_BRT_final.rds"
        )
    )
}
# 3QDS <[x] Done!>
{
    optimal_no_drops <- GCFR_3QDS_BRT_candidate.simp$deviance.summary %$%
        which(mean == min(mean))
    GCFR_3QDS_BRT_final <- gbm.step(
        # Use data stored in model-save! :D
        data = GCFR_tibble_3QDS %>%
            filter(richness > 0) %>%
            mutate(richness = log(richness)) %>%
            as.data.frame(),
        # Use the predictor set at the optimal no. of dropped predictors
        gbm.x = GCFR_3QDS_BRT_candidate.simp$pred.list[[optimal_no_drops]],
        gbm.y = 1,
        family           = "gaussian",
        tolerance.method = "auto",
        tolerance        = 0.0005,
        tree.complexity  = 5,
        learning.rate    = 0.001,
        bag.fraction     = 0.75,   # default
        prev.stratify    = TRUE,
        n.folds          = 10,
        silent           = FALSE,
        plot.main        = TRUE,
        n.trees          = 1,      # starting nt
        step.size        = 1,      # nt increment
        max.trees        = 10000   # (default) (esp. for set_3)
    )
    # Save
    saveRDS(
        GCFR_3QDS_BRT_final,
        paste0(
            "/Volumes/RUAN_PVT/gbm_step_simpler_outputs_2017-10-19/",
            "GCFR_3QDS_BRT_final.rds"
        )
    )
}

# SWAFR ------------------------------------------------------------------------

# Import saved BRT-models
SWAFR_QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "QDS",
    region = "SWAFR"
)
SWAFR_HDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "HDS",
    region = "SWAFR"
)
SWAFR_3QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "3QDS",
    region = "SWAFR"
)

# Extract
SWAFR_QDS_BRT_candidate <- SWAFR_QDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001
SWAFR_HDS_BRT_candidate <- SWAFR_HDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001
SWAFR_3QDS_BRT_candidate <- SWAFR_3QDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001

# Import saved gbm.simp's
SWAFR_QDS_BRT_candidate.simp <- readRDS(
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "SWAFR_QDS_BRT_candidate.simp.rds"
    )
)
SWAFR_HDS_BRT_candidate.simp <- readRDS(
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "SWAFR_HDS_BRT_candidate.simp.rds"
    )
)
SWAFR_3QDS_BRT_candidate.simp <- readRDS(
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "SWAFR_3QDS_BRT_candidate.simp.rds"
    )
)

# Re-run with the simp predictor set
# QDS <[x] DONE!>
{
    optimal_no_drops <- SWAFR_QDS_BRT_candidate.simp$deviance.summary %$%
        which(mean == min(mean))
    SWAFR_QDS_BRT_final <- gbm.step(
        # Use data stored in model-save! :D
        data = SWAFR_tibble_QDS %>%
            filter(richness > 0) %>%
            mutate(richness = log(richness)) %>%
            as.data.frame(),
        # Use the predictor set at the optimal no. of dropped predictors
        gbm.x = SWAFR_QDS_BRT_candidate.simp$pred.list[[optimal_no_drops]],
        gbm.y = 1,
        family           = "gaussian",
        tolerance.method = "auto",
        tolerance        = 0.0005,
        tree.complexity  = 5,
        learning.rate    = 0.001,
        bag.fraction     = 0.75,   # default
        prev.stratify    = TRUE,
        n.folds          = 10,
        silent           = FALSE,
        plot.main        = TRUE,
        n.trees          = 1,      # starting nt
        step.size        = 1,      # nt increment
        max.trees        = 10000   # (default) (esp. for set_3)
    )
    # Save
    saveRDS(
        SWAFR_QDS_BRT_final,
        paste0(
            "/Volumes/RUAN_PVT/gbm_step_simpler_outputs_2017-10-19/",
            "SWAFR_QDS_BRT_final.rds"
        )
    )
}
# HDS <[x] Done!>
{
    optimal_no_drops <- SWAFR_HDS_BRT_candidate.simp$deviance.summary %$%
        which(mean == min(mean))
    SWAFR_HDS_BRT_final <- gbm.step(
        # Use data stored in model-save! :D
        data = SWAFR_tibble_HDS %>%
            filter(richness > 0) %>%
            mutate(richness = log(richness)) %>%
            as.data.frame(),
        # Use the predictor set at the optimal no. of dropped predictors
        gbm.x = SWAFR_HDS_BRT_candidate.simp$pred.list[[optimal_no_drops]],
        gbm.y = 1,
        family           = "gaussian",
        tolerance.method = "auto",
        tolerance        = 0.0005,
        tree.complexity  = 5,
        learning.rate    = 0.001,
        bag.fraction     = 0.75,   # default
        prev.stratify    = TRUE,
        n.folds          = 10,
        silent           = FALSE,
        plot.main        = TRUE,
        n.trees          = 1,      # starting nt
        step.size        = 1,      # nt increment
        max.trees        = 10000   # (default) (esp. for set_3)
    )
    # Save
    saveRDS(
        SWAFR_HDS_BRT_final,
        paste0(
            "/Volumes/RUAN_PVT/gbm_step_simpler_outputs_2017-10-19/",
            "SWAFR_HDS_BRT_final.rds"
        )
    )
}
# 3QDS <[ ] TODO>
{
    optimal_no_drops <- SWAFR_3QDS_BRT_candidate.simp$deviance.summary %$%
        which(mean == min(mean))
    SWAFR_3QDS_BRT_final <- gbm.step(
        # Use data stored in model-save! :D
        data = SWAFR_tibble_3QDS %>%
            filter(richness > 0) %>%
            mutate(richness = log(richness)) %>%
            as.data.frame(),
        # Use the predictor set at the optimal no. of dropped predictors
        gbm.x = SWAFR_3QDS_BRT_candidate.simp$pred.list[[optimal_no_drops]],
        gbm.y = 1,
        family           = "gaussian",
        tolerance.method = "auto",
        tolerance        = 0.0005,
        tree.complexity  = 5,
        learning.rate    = 0.001,
        bag.fraction     = 0.75,   # default
        prev.stratify    = TRUE,
        n.folds          = 10,
        silent           = FALSE,
        plot.main        = TRUE,
        n.trees          = 1,      # starting nt
        step.size        = 1,      # nt increment
        max.trees        = 10000   # (default) (esp. for set_3)
    )
    # Save
    saveRDS(
        SWAFR_3QDS_BRT_final,
        paste0(
            "/Volumes/RUAN_PVT/gbm_step_simpler_outputs_2017-10-19/",
            "SWAFR_3QDS_BRT_final.rds"
        )
    )
}


# BOTH -------------------------------------------------------------------------

# Import saved BRT-models
BOTH_QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "QDS",
    region = "BOTH"
)
BOTH_HDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "HDS",
    region = "BOTH"
)
BOTH_3QDS_saved_BRTs <- explore_gbm_step_saves(
    dir = "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30",
    scale = "3QDS",
    region = "BOTH"
)

# Extract
BOTH_QDS_BRT_candidate <- BOTH_QDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001
BOTH_HDS_BRT_candidate <- BOTH_HDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001
BOTH_3QDS_BRT_candidate <- BOTH_3QDS_saved_BRTs$saved_BRTs$tol_5e_04_tc_5_lr_0.001

# Import saved gbm.simp's
BOTH_QDS_BRT_candidate.simp <- readRDS(
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "BOTH_QDS_BRT_candidate.simp.rds"
    )
)
BOTH_HDS_BRT_candidate.simp <- readRDS(
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "BOTH_HDS_BRT_candidate.simp.rds"
    )
)
BOTH_3QDS_BRT_candidate.simp <- readRDS(
    paste0(
        "/Volumes/RUAN_PVT/gbm_simplify_outputs_2017-10-19/",
        "BOTH_3QDS_BRT_candidate.simp.rds"
    )
)

# Re-run with the simp predictor set
# QDS <[x] DONE!>
{
    optimal_no_drops <- BOTH_QDS_BRT_candidate.simp$deviance.summary %$%
        which(mean == min(mean))
    BOTH_QDS_BRT_final <- gbm.step(
        # Use data stored in model-save! :D
        data = as.data.frame(rbind(
            GCFR_tibble_QDS %>%
                filter(richness > 0) %>%
                mutate(
                    richness = log(richness),
                    region = factor("GCFR")
                ),
            SWAFR_tibble_QDS %>%
                filter(richness > 0) %>%
                mutate(
                    richness = log(richness),
                    region = factor("SWAFR")
                )
        )),
        # Use the predictor set at the optimal no. of dropped predictors
        gbm.x = BOTH_QDS_BRT_candidate.simp$pred.list[[optimal_no_drops]],
        gbm.y = 1,
        family           = "gaussian",
        tolerance.method = "auto",
        tolerance        = 0.0005,
        tree.complexity  = 5,
        learning.rate    = 0.001,
        bag.fraction     = 0.75,   # default
        prev.stratify    = TRUE,
        n.folds          = 10,
        silent           = FALSE,
        plot.main        = TRUE,
        n.trees          = 1,      # starting nt
        step.size        = 1,      # nt increment
        max.trees        = 10000   # (default) (esp. for set_3)
    )
    # Save
    saveRDS(
        BOTH_QDS_BRT_final,
        paste0(
            "/Volumes/RUAN_PVT/gbm_step_simpler_outputs_2017-10-19/",
            "BOTH_QDS_BRT_final.rds"
        )
    )
}
# HDS <[x] Done>
{
    optimal_no_drops <- BOTH_HDS_BRT_candidate.simp$deviance.summary %$%
        which(mean == min(mean))
    BOTH_HDS_BRT_final <- gbm.step(
        # Use data stored in model-save! :D
        data = as.data.frame(rbind(
            GCFR_tibble_HDS %>%
                filter(richness > 0) %>%
                mutate(
                    richness = log(richness),
                    region = factor("GCFR")
                ),
            SWAFR_tibble_HDS %>%
                filter(richness > 0) %>%
                mutate(
                    richness = log(richness),
                    region = factor("SWAFR")
                )
        )),
        # Use the predictor set at the optimal no. of dropped predictors
        gbm.x = BOTH_HDS_BRT_candidate.simp$pred.list[[optimal_no_drops]],
        gbm.y = 1,
        family           = "gaussian",
        tolerance.method = "auto",
        tolerance        = 0.0005,
        tree.complexity  = 5,
        learning.rate    = 0.001,
        bag.fraction     = 0.75,   # default
        prev.stratify    = TRUE,
        n.folds          = 10,
        silent           = FALSE,
        plot.main        = TRUE,
        n.trees          = 1,      # starting nt
        step.size        = 1,      # nt increment
        max.trees        = 10000   # (default) (esp. for set_3)
    )
    # Save
    saveRDS(
        BOTH_HDS_BRT_final,
        paste0(
            "/Volumes/RUAN_PVT/gbm_step_simpler_outputs_2017-10-19/",
            "BOTH_HDS_BRT_final.rds"
        )
    )
}
# 3QDS <[x] done>
{
    optimal_no_drops <- BOTH_3QDS_BRT_candidate.simp$deviance.summary %$%
        which(mean == min(mean))
    BOTH_3QDS_BRT_final <- gbm.step(
        # Use data stored in model-save! :D
        data = as.data.frame(rbind(
            GCFR_tibble_3QDS %>%
                filter(richness > 0) %>%
                mutate(
                    richness = log(richness),
                    region = factor("GCFR")
                ),
            SWAFR_tibble_3QDS %>%
                filter(richness > 0) %>%
                mutate(
                    richness = log(richness),
                    region = factor("SWAFR")
                )
        )),
        # Use the predictor set at the optimal no. of dropped predictors
        gbm.x = BOTH_3QDS_BRT_candidate.simp$pred.list[[optimal_no_drops]],
        gbm.y = 1,
        family           = "gaussian",
        tolerance.method = "auto",
        tolerance        = 0.0005,
        tree.complexity  = 5,
        learning.rate    = 0.001,
        bag.fraction     = 0.75,   # default
        prev.stratify    = TRUE,
        n.folds          = 10,
        silent           = FALSE,
        plot.main        = TRUE,
        n.trees          = 1,      # starting nt
        step.size        = 1,      # nt increment
        max.trees        = 10000   # (default) (esp. for set_3)
    )
    # Save
    saveRDS(
        BOTH_3QDS_BRT_final,
        paste0(
            "/Volumes/RUAN_PVT/gbm_step_simpler_outputs_2017-10-19/",
            "BOTH_3QDS_BRT_final.rds"
        )
    )
}
