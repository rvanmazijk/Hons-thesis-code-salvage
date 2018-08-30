# Implementing GDF calculation (Hauenstein et al. 2016) on my BRTs
# ---PRACTICE
# Hons thesis
# R. van Mazijk

# Setup ------------------------------------------------------------------------

source("../../../Users/ruanvanmazijk/Google Drive/GDF/simpl_kgdf.R")


# Import models ----------------------------------------------------------------

dir <- "/Volumes/RUAN_PVT/gbm_step_outputs_2017-09-30"
scale <- "QDS"
region <- "GCFR"
GCFR_QDS_saved_BRTs <- get_saved_BRTs(
    dir = glue("{dir}/{scale}/{region}/"),
    tol = "tol-5e-04"
)

# Import data ------------------------------------------------------------------
# Copied from `13.R`

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


# Run kGDF ---------------------------------------------------------------------

# From Google Drive/GDF fork

kGDF(
    y = GCFR_tibble$richness,
    fm = GCFR_QDS_saved_BRTs$tol_5e_04_tc_1_lr_0.001,
    data = GCFR_tibble,
    distr = "normal",
    mod = "BRT",
    noise = 0.25,  # default
    perf = GCFR_QDS_saved_BRTs$tol_5e_04_tc_1_lr_0.001$n.trees,  # nt
    pass = 10,  # thumb-suck? check paper too
    method = "jackknife"  # default
)
# Fails

update(
    GCFR_QDS_saved_BRTs$tol_5e_04_tc_1_lr_0.001,
    as.formula(richness ~ MAP),
    data = GCFR_tibble,
    weights = rep(1, nrow(GCFR_tibble)),
    distribution = "gaussian",
    #tolerance.method = "auto",
    #tolerance        = 0.0005,
    interaction.depth  = 1,
    target.trees = GCFR_QDS_saved_BRTs$tol_5e_04_tc_1_lr_0.001$trees,
    #learning.rate    = 0.001,
    bag.fraction     = 0.75#,   # default
    #prev.stratify    = TRUE,
    #n.folds          = 10,
    #silent           = FALSE,
    #plot.main        = TRUE,
    #n.trees          = 1,      # starting nt
    #step.size        = 1,      # nt increment
    #max.trees        = 10000
)
# Fails

# Can't get past this point b.c. if I leave out target.trees, I get:
#   > Error in gbm.fit(x, y, offset = offset, distribution = distribution, w = w,  :
#   >   object 'target.trees' not found
# And, haha, if I *do* include it (as above), I get *this* error:
#   > Error in gbm::gbm(formula = richness ~ MAP, distribution = "gaussian",  :
#   >   unused argument (target.trees = GCFR_QDS_saved_BRTs$tol_5e_04_tc_1_lr_0.001$trees)
