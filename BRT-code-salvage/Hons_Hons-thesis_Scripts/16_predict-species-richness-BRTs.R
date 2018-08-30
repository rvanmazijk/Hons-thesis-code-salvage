# ...
# Hons thesis
# R. van Mazijk

# Setup ------------------------------------------------------------------------

source("Scripts/01_setup.R")
source("Scripts/analyses/02_import-region-polys.R")
source("Scripts/analyses/03_import-environmental-data.R")
source("Scripts/analyses/04_import-floral-data.R")

# Read-in final BRT models
{

    GCFR_QDS_BRT_final <- readRDS(here::here(
        "Results/gbm_step_simpler_outputs_2017-10-19/",
        "GCFR_QDS_BRT_final.rds"
    ))
    GCFR_HDS_BRT_final <- readRDS(here::here(
        "Results/gbm_step_simpler_outputs_2017-10-19/",
        "GCFR_HDS_BRT_final.rds"
    ))
    GCFR_3QDS_BRT_final <- readRDS(here::here(
        "Results/gbm_step_simpler_outputs_2017-10-19/",
        "GCFR_3QDS_BRT_final.rds"
    ))

    SWAFR_QDS_BRT_final <- readRDS(here::here(
        "Results/gbm_step_simpler_outputs_2017-10-19/",
        "SWAFR_QDS_BRT_final.rds"
    ))
    SWAFR_HDS_BRT_final <- readRDS(here::here(
        "Results/gbm_step_simpler_outputs_2017-10-19/",
        "SWAFR_HDS_BRT_final.rds"
    ))
    SWAFR_3QDS_BRT_final <- readRDS(here::here(
        "Results/gbm_step_simpler_outputs_2017-10-19/",
        "SWAFR_3QDS_BRT_final.rds"
    ))

    BOTH_QDS_BRT_final <- readRDS(here::here(
        "Results/gbm_step_simpler_outputs_2017-10-19/",
        "BOTH_QDS_BRT_final.rds"
    ))
    BOTH_HDS_BRT_final <- readRDS(here::here(
        "Results/gbm_step_simpler_outputs_2017-10-19/",
        "BOTH_HDS_BRT_final.rds"
    ))
    BOTH_3QDS_BRT_final <- readRDS(here::here(
        "Results/gbm_step_simpler_outputs_2017-10-19/",
        "BOTH_3QDS_BRT_final.rds"
    ))
    BRT_finals <- list(  # NB: same order as n_top_vars!!!
        SWAFR_QDS = SWAFR_QDS_BRT_final,
        SWAFR_HDS = SWAFR_HDS_BRT_final,
        SWAFR_3QDS = SWAFR_3QDS_BRT_final,
        GCFR_QDS = GCFR_QDS_BRT_final,
        GCFR_HDS = GCFR_HDS_BRT_final,
        GCFR_3QDS = GCFR_3QDS_BRT_final,
        BOTH_QDS = BOTH_QDS_BRT_final,
        BOTH_HDS = BOTH_HDS_BRT_final,
        BOTH_3QDS = BOTH_3QDS_BRT_final
    )

    # quick side thing: table of nts
    nts <- foreach(model = BRT_finals) %do% model$n.trees
    names(nts) <- names(BRT_finals)
    # $SWAFR_QDS
    # [1] 2196
    #
    # $SWAFR_HDS
    # [1] 1963
    #
    # $SWAFR_3QDS
    # [1] 1381
    #
    # $GCFR_QDS
    # [1] 1986
    #
    # $GCFR_HDS
    # [1] 2014
    #
    # $GCFR_3QDS
    # [1] 1648
    #
    # $BOTH_QDS
    # [1] 2171
    #
    # $BOTH_HDS
    # [1] 2174
    #
    # $BOTH_3QDS
    # [1] 1991
    #
    # Add this manually to the partial dep plots!

}

# Read-in raster data
{

    giswd <- "/Volumes/RUAN_UCT/GIS/"

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
{

    make_roughness_versions <- function(x) {
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
    }

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
    GCFR_environment_stack_QDS <- resample(GCFR_environment_stack, GCFR_richness_QDS, method = "bilinear")
    GCFR_environment_stack_HDS <- resample(GCFR_environment_stack, GCFR_richness_HDS, method = "bilinear")
    GCFR_environment_stack_3QDS <-  resample(GCFR_environment_stack, GCFR_richness_3QDS, method = "bilinear")

    GCFR_roughness_list_QDS <- GCFR_environment_stack_QDS %>% make_roughness_versions()
    GCFR_roughness_list_HDS <- GCFR_environment_stack_HDS %>% make_roughness_versions()
    GCFR_roughness_list_3QDS <- GCFR_environment_stack_3QDS %>% make_roughness_versions()

    GCFR_roughness_stack_QDS <- stack(GCFR_roughness_list_QDS )
    GCFR_roughness_stack_HDS <- stack(GCFR_roughness_list_HDS )
    GCFR_roughness_stack_3QDS <- stack(GCFR_roughness_list_3QDS)

    # Combine w/ richness data
    GCFR_all_stack_HDS <- stack(
        GCFR_richness_HDS,
        GCFR_environment_stack_HDS,
        GCFR_roughness_stack_HDS
    )
    GCFR_all_stack_QDS <- stack(
        GCFR_richness_QDS,
        GCFR_environment_stack_QDS,
        GCFR_roughness_stack_QDS
    )
    GCFR_all_stack_3QDS <- stack(
        GCFR_richness_3QDS,
        GCFR_environment_stack_3QDS,
        GCFR_roughness_stack_3QDS
    )

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
    SWAFR_environment_stack_QDS <- resample(SWAFR_environment_stack, SWAFR_richness_QDS, method = "bilinear")
    SWAFR_environment_stack_HDS <- resample(SWAFR_environment_stack, SWAFR_richness_HDS, method = "bilinear")
    SWAFR_environment_stack_3QDS <- resample(SWAFR_environment_stack, SWAFR_richness_3QDS, method = "bilinear")

    SWAFR_roughness_list_QDS <- SWAFR_environment_stack_QDS %>% make_roughness_versions()
    SWAFR_roughness_list_HDS <- SWAFR_environment_stack_HDS %>% make_roughness_versions()
    SWAFR_roughness_list_3QDS <- SWAFR_environment_stack_3QDS %>% make_roughness_versions()

    SWAFR_roughness_stack_QDS <- stack(SWAFR_roughness_list_QDS )
    SWAFR_roughness_stack_HDS <- stack(SWAFR_roughness_list_HDS )
    SWAFR_roughness_stack_3QDS <- stack(SWAFR_roughness_list_3QDS)

    # Combine w/ richness data
    SWAFR_all_stack_HDS <- stack(
        SWAFR_richness_HDS,
        SWAFR_environment_stack_HDS,
        SWAFR_roughness_stack_HDS
    )
    SWAFR_all_stack_QDS <- stack(
        SWAFR_richness_QDS,
        SWAFR_environment_stack_QDS,
        SWAFR_roughness_stack_QDS
    )
    SWAFR_all_stack_3QDS <- stack(
        SWAFR_richness_3QDS,
        SWAFR_environment_stack_3QDS,
        SWAFR_roughness_stack_3QDS
    )

    # Final tidy up ------------------------------------------------------------

    # Rm AWCh1
    names(GCFR_all_stack_QDS)
    names(SWAFR_all_stack_QDS)
    GCFR_all_stack_QDS <- GCFR_all_stack_QDS[[-c(19, 37)]]
    SWAFR_all_stack_QDS <- SWAFR_all_stack_QDS[[-c(19, 37)]]
    GCFR_all_stack_HDS <- GCFR_all_stack_HDS[[-c(19, 37)]]
    SWAFR_all_stack_HDS <- SWAFR_all_stack_HDS[[-c(19, 37)]]
    GCFR_all_stack_3QDS <- GCFR_all_stack_3QDS[[-c(19, 37)]]
    SWAFR_all_stack_3QDS <- SWAFR_all_stack_3QDS[[-c(19, 37)]]
    # Rename to match df used to make BRTs
    var_names <- c(
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
        "rough_elev",
        "rough_MAP",
        "rough_PWQ",
        "rough_PDQ",
        "rough_PCV",
        "rough_MLST",
        "rough_TWQ",
        "rough_TCQ",
        "rough_NDVI",
        "rough_CECSOL",
        "rough_BLDFIE",
        "rough_CLYPPT",
        "rough_CRFVOL",
        "rough_OCDENS",
        "rough_PHIKCL",
        "rough_SLTPPT",
        "rough_SNDPPT"
    )
    names(GCFR_all_stack_QDS  )[2:nlayers(GCFR_all_stack_QDS)]   <- var_names
    names(SWAFR_all_stack_QDS )[2:nlayers(SWAFR_all_stack_QDS)]  <- var_names
    names(GCFR_all_stack_HDS  )[2:nlayers(GCFR_all_stack_HDS)]   <- var_names
    names(SWAFR_all_stack_HDS )[2:nlayers(SWAFR_all_stack_HDS)]  <- var_names
    names(GCFR_all_stack_3QDS )[2:nlayers(GCFR_all_stack_3QDS)]  <- var_names
    names(SWAFR_all_stack_3QDS)[2:nlayers(SWAFR_all_stack_3QDS)] <- var_names

    GCFR_all_stack_QDS   %<>% mask(GCFR_border)
    GCFR_all_stack_HDS   %<>% mask(GCFR_border)
    GCFR_all_stack_3QDS  %<>% mask(GCFR_border)
    SWAFR_all_stack_QDS  %<>% mask(SWAFR_border)
    SWAFR_all_stack_HDS  %<>% mask(SWAFR_border)
    SWAFR_all_stack_3QDS %<>% mask(SWAFR_border)

}


# Predict to own regions -------------------------------------------------------
{
    # QDS
    pred_richness_GCFR_QDS <- predict(
        GCFR_all_stack_QDS[[-1]], #${w/o richness},
        BRT_finals$GCFR_QDS,
        n.trees = BRT_finals$GCFR_QDS$gbm.call$best.trees,
        type = "response"
    )
    #plot(exp(pred_richness_GCFR_QDS) - 1)
    pred_richness_SWAFR_QDS <- predict(
        SWAFR_all_stack_QDS[[-1]], #${w/o richness},
        BRT_finals$SWAFR_QDS,
        n.trees = BRT_finals$SWAFR_QDS$gbm.call$best.trees,
        type = "response"
    )
    #plot(exp(pred_richness_SWAFR_QDS) - 1)
    BOTH_all_stack_QDS <-
        merge(GCFR_all_stack_QDS, SWAFR_all_stack_QDS)
    names(BOTH_all_stack_QDS) <- "richness" %and% var_names
    pred_richness_BOTH_QDS <- predict(
        BOTH_all_stack_QDS[[-1]], #${w/o richness},
        BRT_finals$BOTH_QDS,
        n.trees = BRT_finals$BOTH_QDS$gbm.call$best.trees,
        type = "response"
    )
    #plot(exp(pred_richness_BOTH_QDS) - 1)

    # HDS
    pred_richness_GCFR_HDS <- predict(
        GCFR_all_stack_HDS[[-1]], #${w/o richness},
        BRT_finals$GCFR_HDS,
        n.trees = BRT_finals$GCFR_HDS$gbm.call$best.trees,
        type = "response"
    )
    #plot(exp(pred_richness_GCFR_HDS) - 1)
    pred_richness_SWAFR_HDS <- predict(
        SWAFR_all_stack_HDS[[-1]], #${w/o richness},
        BRT_finals$SWAFR_HDS,
        n.trees = BRT_finals$SWAFR_HDS$gbm.call$best.trees,
        type = "response"
    )
    #plot(exp(pred_richness_SWAFR_HDS) - 1)
    BOTH_all_stack_HDS <-
        merge(GCFR_all_stack_HDS, SWAFR_all_stack_HDS)
    names(BOTH_all_stack_HDS) <- "richness" %and% var_names
    pred_richness_BOTH_HDS <- predict(
        BOTH_all_stack_HDS[[-1]], #${w/o richness},
        BRT_finals$BOTH_HDS,
        n.trees = BRT_finals$BOTH_HDS$gbm.call$best.trees,
        type = "response"
    )
    #plot(exp(pred_richness_BOTH_HDS) - 1)

    # 3QDS
    pred_richness_GCFR_3QDS <- predict(
        GCFR_all_stack_3QDS[[-1]], #${w/o richness},
        BRT_finals$GCFR_3QDS,
        n.trees = BRT_finals$GCFR_3QDS$gbm.call$best.trees,
        type = "response"
    )
    #plot(exp(pred_richness_GCFR_3QDS) - 1)
    pred_richness_SWAFR_3QDS <- predict(
        SWAFR_all_stack_3QDS[[-1]], #${w/o richness},
        BRT_finals$SWAFR_3QDS,
        n.trees = BRT_finals$SWAFR_3QDS$gbm.call$best.trees,
        type = "response"
    )
    #plot(exp(pred_richness_SWAFR_3QDS) - 1)
    BOTH_all_stack_3QDS <-
        merge(GCFR_all_stack_3QDS, SWAFR_all_stack_3QDS)
    names(BOTH_all_stack_3QDS) <- "richness" %and% var_names
    pred_richness_BOTH_3QDS <- predict(
        BOTH_all_stack_3QDS[[-1]], #${w/o richness},
        BRT_finals$BOTH_3QDS,
        n.trees = BRT_finals$BOTH_3QDS$gbm.call$best.trees,
        type = "response"
    )
    #plot(exp(pred_richness_BOTH_3QDS) - 1)
}

# Predict to SWAFR and GCFR w/ BOTH --------------------------------------------
{
    # QDS
    pred_richness_GCFR_w_BOTH_QDS <- predict(
        GCFR_all_stack_QDS[[-1]], #${w/o richness},
        BRT_finals$BOTH_QDS,
        n.trees = BRT_finals$BOTH_QDS$gbm.call$best.trees,
        type = "response"
    )
    pred_richness_SWAFR_w_BOTH_QDS <- predict(
        SWAFR_all_stack_QDS[[-1]], #${w/o richness},
        BRT_finals$BOTH_QDS,
        n.trees = BRT_finals$BOTH_QDS$gbm.call$best.trees,
        type = "response"
    )
    # HDS
    pred_richness_GCFR_w_BOTH_HDS <- predict(
        GCFR_all_stack_HDS[[-1]], #${w/o richness},
        BRT_finals$BOTH_HDS,
        n.trees = BRT_finals$BOTH_HDS$gbm.call$best.trees,
        type = "response"
    )
    pred_richness_SWAFR_w_BOTH_HDS <- predict(
        SWAFR_all_stack_HDS[[-1]], #${w/o richness},
        BRT_finals$BOTH_HDS,
        n.trees = BRT_finals$BOTH_HDS$gbm.call$best.trees,
        type = "response"
    )
    # QDS
    pred_richness_GCFR_w_BOTH_3QDS <- predict(
        GCFR_all_stack_3QDS[[-1]], #${w/o richness},
        BRT_finals$BOTH_3QDS,
        n.trees = BRT_finals$BOTH_3QDS$gbm.call$best.trees,
        type = "response"
    )
    pred_richness_SWAFR_w_BOTH_3QDS <- predict(
        SWAFR_all_stack_3QDS[[-1]], #${w/o richness},
        BRT_finals$BOTH_3QDS,
        n.trees = BRT_finals$BOTH_3QDS$gbm.call$best.trees,
        type = "response"
    )
}

# Obs & pred maps --------------------------------------------------------------

maps <- list(
    GCFR_richness_QDS,   exp(pred_richness_GCFR_QDS) - 1,
    GCFR_richness_HDS,   exp(pred_richness_GCFR_HDS) - 1,
    GCFR_richness_3QDS,  exp(pred_richness_GCFR_3QDS) - 1,
    SWAFR_richness_QDS,  exp(pred_richness_SWAFR_QDS) - 1,
    SWAFR_richness_HDS,  exp(pred_richness_SWAFR_HDS) - 1,
    SWAFR_richness_3QDS, exp(pred_richness_SWAFR_3QDS) - 1
)
names(maps) <- c(
    "GCFR_QDS_obs", "GCFR_QDS_pred",
    "GCFR_HDS_obs", "GCFR_HDS_pred",
    "GCFR_3QDS_obs", "GCFR_3QDS_pred",
    "SWAFR_QDS_obs", "SWAFR_QDS_pred",
    "SWAFR_HDS_obs", "SWAFR_HDS_pred",
    "SWAFR_3QDS_obs", "SWAFR_3QDS_pred"
)
for (i in c(1, 3, 5, 7, 9, 11)) {
    op <- par()
    zmax <- max(maps[[i]][], maps[[i + 1]][], na.rm = TRUE)
    tiff(file = glue("map_{names(maps)[i]}_pred.tiff"), width = 250, height = 400)
    par(mfrow = c(2, 1), mar = c(2, 2, 2, 2))
    maps[[i]][maps[[i]] == 0] <- NA
    maps[[i + 1]][maps[[i + 1]] == 0] <- NA
    plot(maps[[i]], zlim = c(0, zmax), main = "Observed")
    plot(maps[[i + 1]], zlim = c(0, zmax), main = "Predicted")
    dev.off()
    par(op)
}

# Compare obs and predicted ----------------------------------------------------

pred_richness_tidy <- as_tibble(rbind(
    data.frame(
        region = "GCFR",
        scale = "QDS",
        pred = pred_richness_GCFR_QDS[],
        obs = GCFR_richness_QDS[]
    ),
    data.frame(
        region = "GCFR",
        scale = "HDS",
        pred = pred_richness_GCFR_HDS[],
        obs = GCFR_richness_HDS[]
    ),
    data.frame(
        region = "GCFR",
        scale = "3QDS",
        pred = pred_richness_GCFR_3QDS[],
        obs = GCFR_richness_3QDS[]
    ),
    data.frame(
        region = "SWAFR",
        scale = "QDS",
        pred = pred_richness_SWAFR_QDS[],
        obs = SWAFR_richness_QDS[]
    ),
    data.frame(
        region = "SWAFR",
        scale = "HDS",
        pred = pred_richness_SWAFR_HDS[],
        obs = SWAFR_richness_HDS[]
    ),
    data.frame(
        region = "SWAFR",
        scale = "3QDS",
        pred = pred_richness_SWAFR_3QDS[],
        obs = SWAFR_richness_3QDS[]
    ),
    data.frame(
        region = "BOTH",
        scale = "QDS",
        pred = pred_richness_BOTH_QDS[],
        obs = merge(GCFR_richness_QDS, SWAFR_richness_QDS)[]
    ),
    data.frame(
        region = "BOTH",
        scale = "HDS",
        pred = pred_richness_BOTH_HDS[],
        obs = merge(GCFR_richness_HDS, SWAFR_richness_HDS)[]
    ),
    data.frame(
        region = "BOTH",
        scale = "3QDS",
        pred = pred_richness_BOTH_3QDS[],
        obs = merge(GCFR_richness_3QDS, SWAFR_richness_3QDS)[]
    ),
    data.frame(
        region = "GCFR_from_BOTH",
        scale = "QDS",
        pred = pred_richness_GCFR_w_BOTH_QDS[],
        obs = GCFR_richness_QDS[]
    ),
    data.frame(
        region = "GCFR_from_BOTH",
        scale = "HDS",
        pred = pred_richness_GCFR_w_BOTH_HDS[],
        obs = GCFR_richness_HDS[]
    ),
    data.frame(
        region = "GCFR_from_BOTH",
        scale = "3QDS",
        pred = pred_richness_GCFR_w_BOTH_3QDS[],
        obs = GCFR_richness_3QDS[]
    ),
    data.frame(
        region = "SWAFR_from_BOTH",
        scale = "QDS",
        pred = pred_richness_SWAFR_w_BOTH_QDS[],
        obs = SWAFR_richness_QDS[]
    ),
    data.frame(
        region = "SWAFR_from_BOTH",
        scale = "HDS",
        pred = pred_richness_SWAFR_w_BOTH_HDS[],
        obs = SWAFR_richness_HDS[]
    ),
    data.frame(
        region = "SWAFR_from_BOTH",
        scale = "3QDS",
        pred = pred_richness_SWAFR_w_BOTH_3QDS[],
        obs = SWAFR_richness_3QDS[]
    )
))
pred_richness_tidy <- read_csv(here::here("Results", "pred_richness_tidy.csv"))

# Test relationships betw obs and exp
pred_vs_obs_test <- function(x = "data.frame", region, scale) {
    x <- x[x$region == region & x$scale == scale, c("pred", "obs")]
    model <- lm(pred ~ log(obs + 1), data = x)
    model_tidy <- broom::tidy(model)
    model_glance <- broom::glance(model)
    model_RMA <- lmodel2(
        pred ~ log(obs + 1),
        data = x,
        range.y = "relative", range.x = "relative"
    )
    model_RMA <- model_RMA$regression.results %>%
        filter(Method == "RMA") %>%
        dplyr::select(Intercept, Slope)
    return(data.frame(
        region = region,
        scale = scale,
        int = model_tidy$estimate[1],
        slope = model_tidy$estimate[2],
        RMA_int = model_RMA$Intercept,
        RMA_slope = model_RMA$Slope,
        p_slope = model_tidy$p.value[2],
        r2 = model_glance$r.squared
    ))
}
summary_stats <-
    rbind(
        pred_vs_obs_test(pred_richness_tidy, "GCFR", "QDS"),
        pred_vs_obs_test(pred_richness_tidy, "GCFR", "HDS"),
        pred_vs_obs_test(pred_richness_tidy, "GCFR", "3QDS"),
        pred_vs_obs_test(pred_richness_tidy, "SWAFR", "QDS"),
        pred_vs_obs_test(pred_richness_tidy, "SWAFR", "HDS"),
        pred_vs_obs_test(pred_richness_tidy, "SWAFR", "3QDS"),
        pred_vs_obs_test(pred_richness_tidy, "BOTH", "QDS"),
        pred_vs_obs_test(pred_richness_tidy, "BOTH", "HDS"),
        pred_vs_obs_test(pred_richness_tidy, "BOTH", "3QDS"),
        pred_vs_obs_test(pred_richness_tidy, "GCFR ~ BOTH", "QDS"),
        pred_vs_obs_test(pred_richness_tidy, "GCFR ~ BOTH", "HDS"),
        pred_vs_obs_test(pred_richness_tidy, "GCFR ~ BOTH", "3QDS"),
        pred_vs_obs_test(pred_richness_tidy, "SWAFR ~ BOTH", "QDS"),
        pred_vs_obs_test(pred_richness_tidy, "SWAFR ~ BOTH", "HDS"),
        pred_vs_obs_test(pred_richness_tidy, "SWAFR ~ BOTH", "3QDS")
    ) %>%
    mutate(
        int_lab       = paste0("beta[0] == ",     round(int,       digits = 3)),
        slope_lab     = paste0("beta[1] == ",     round(slope,     digits = 3)),
        RMA_int_lab   = paste0("beta[0] == ",     round(RMA_int,   digits = 3)),
        RMA_slope_lab = paste0("beta[1] == ",     round(RMA_slope, digits = 3)),
        r2            = paste0("italic(R)^2 == ", round(r2,        digits = 3)),
        region = region %>%
            as.character(region) %>%
            str_replace_all("_from_", " ~ ") %>%
            as.factor()
    ) %>%
    as_tibble()
levels(summary_stats$region) %<>% paste0("(", LETTERS[1:5], ") ", .)
pred_richness_tidy %<>% mutate(region = region %>%
    as.character() %>%
    str_replace_all("_from_", " ~ ") %>%
    as.factor()
)
levels(pred_richness_tidy$region) %<>% paste0("(", LETTERS[1:5], ") ", .)

xymin <- min(pred_richness_tidy$pred, na.rm = TRUE)
xymax <- max(c(pred_richness_tidy$pred, log(pred_richness_tidy$obs + 1)), na.rm = TRUE)
pred_obs_plot <- ggplot(pred_richness_tidy) +
    # Scatter
    geom_point(aes(x = log(obs + 1), y = pred, col = pred - log(obs + 1))) +
    # Y = X
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", col = "grey50") +
    # RMA line
    geom_abline(
        data = summary_stats,
        aes(intercept = RMA_int, slope = RMA_slope),
        col = "blue"
    ) +
    # OLS line
    geom_abline(
        data = summary_stats,
        aes(intercept = int, slope = slope),
        col = "black"
    ) +
    # Theming
    facet_grid(scale ~ region) +
    labs(
        x = expression(paste("Observed species richness (", log("S" + 1), ")")),
        y = expression(paste("Predicted species richness (", log(hat("S") + 1), ")"))
    ) +
    scale_x_continuous(
        limits = c(xymin, xymax)
        #breaks = nice_S_scale$breaks,
        #labels = nice_S_scale$labels
    ) +
    scale_y_continuous(
        limits = c(xymin, xymax)
        #breaks = nice_S_scale$breaks,
        #labels = nice_S_scale$labels
    ) +
    # Annotations
    geom_text(
        data = summary_stats,
        aes(label = RMA_slope_lab),
        col = "blue",
        x = 3, y = 8,
        parse = TRUE,
        hjust = 0
    ) +
    geom_text(
        data = summary_stats,
        aes(label = slope_lab),
        col = "black",
        x = 3, y = 7,
        parse = TRUE,
        hjust = 0
    ) +
    # All P-values super sig, so don't plot that text
    geom_text(
        data = summary_stats,
        aes(label = r2),
        col = "grey50",
        x = 3, y = 6,
        parse = TRUE,
        hjust = 0
    ) +
    scale_color_distiller(
        name = "Error",
        palette = "Spectral",
        limits = c(-3, 3)
    ) +
    theme_bw() +
    theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(angle = 0),
        strip.text.y = element_text(angle = 0),
        strip.background = element_blank()
    )
ggsave(
    plot = pred_obs_plot,
    filename = here::here(
        "Results",
        "richness_pred_obs_plot.tiff"
    ),
    width = 25,
    height = 15,
    units = "cm",
    dpi = 125
)
write.csv(pred_richness_tidy, here::here("Results", "pred_richness_tidy.csv"))
pred_richness_tidy <- read.csv(here::here("Results", "pred_richness_tidy.csv"))

#                 green,    orange,
my_pal_col <- c("#009E73", "#E69F00")
t_tests_pred_obs <- list(
    pred_richness_tidy %>%
        filter(not(is.na(pred))) %>%
        filter(region == "GCFR", scale == "QDS") %$%
        t.test(pred, log(obs + 1)),
    pred_richness_tidy %>%
        filter(not(is.na(pred))) %>%
        filter(region == "GCFR", scale == "HDS") %$%
        t.test(pred, log(obs + 1)),
    pred_richness_tidy %>%
        filter(not(is.na(pred))) %>%
        filter(region == "GCFR", scale == "3QDS") %$%
        t.test(pred, log(obs + 1)),
    pred_richness_tidy %>%
        filter(not(is.na(pred))) %>%
        filter(region == "SWAFR", scale == "QDS") %$%
        t.test(pred, log(obs + 1)),
    pred_richness_tidy %>%
        filter(not(is.na(pred))) %>%
        filter(region == "SWAFR", scale == "HDS") %$%
        t.test(pred, log(obs + 1)),
    pred_richness_tidy %>%
        filter(not(is.na(pred))) %>%
        filter(region == "SWAFR", scale == "3QDS") %$%
        t.test(pred, log(obs + 1))
)
t_tests_pred_obs %<>% map(broom::tidy)
t_tests_pred_obs <- foreach(t_test = t_tests_pred_obs) %do% t_test$p.value
#my_pal_fill <- c("grey50", "white")
richness_pred_boxplots <- pred_richness_tidy %>%
    na.exclude() %>%
    filter(
        region %notin% c("BOTH", "GCFR ~ BOTH", "SWAFR ~ BOTH")
    ) %>%
    gather("richness_type", "richness", -scale, -region, -X) %>%
    as_tibble() %>%
    mutate(
        richness = ifelse(richness_type == "obs",
            log(richness + 1),
            richness),
        richness_type = ifelse(richness_type == "obs",
            "(B) Observed",
            "(A) Predicted")
    ) %>%
    ggplot() +
    geom_boxplot(aes(
        x = scale,
        y = richness,
        col = region#,
        #fill = richness_type
    ), outlier.colour = "white") +
    #ylim(0, 4500) +
    stat_summary(
       aes(
           x = scale,
           y = richness,
           col = region
       ),
       position = position_dodge(width = 0.75),
       fun.y = mean,
       geom = "point"
    ) +
    # Seeing as all means are sig diff betwen GCFR and SWAFR (see below),
    # I'm gonna ad some asterisks!
    #geom_text(aes(x = scale), y = 4000, label = "*", col = "grey50") +
    facet_grid(~ richness_type) +
    scale_color_manual(values = my_pal_col, name = "Region") +
    #scale_fill_manual(values = my_pal_fill, name = "") +
    labs(
        x = "Scale",
        y = "Species richness"
    ) +
    theme_bw() +
    theme(
        strip.background = element_blank(),
        panel.grid = element_blank()
    )
ggsave(
    plot = richness_pred_boxplots,
    filename = here::here(
        "Results",
        "richness_pred_boxplots.tiff"
    ),
    width = 12,
    height = 6,
    units = "cm",
    dpi = 120
)
